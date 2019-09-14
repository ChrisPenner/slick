# Slick

Want to get started quickly? Check out the [Slick site template](https://github.com/ChrisPenner/slick-template)!

Slick is a static site generator written and configured using Haskell. It's the spiritual successor to my previous
static-site generator project [SitePipe](https://github.com/chrispenner/SitePipe/); but is faster, simpler, and more
easily used in combination with other tools.

Slick provides a small set of tools and combinators for building static
websites on top of the [Shake](https://shakebuild.com/) build system. Shake is
adaptable, fast, reliable, and caches aggressively so it's a sensible tool for
static-site builds, but figuring out how to get started can be a bit abstract. Slick aims to answer the question of
'how do I get a site building?' while giving you the necessary tools and examples to figure out how to accomplish your
goals.

See the [hackage docs](https://hackage.haskell.org/package/slick) for in depth help on available combinators.

# Overview

Here's a quick overview of what Slick can do:

- Slick uses the Shake build tool; the same used by ghcide! We recommend using `Development.Shake.Forward`; it auto-discovers which resources it should cache as you go! This means a blazing fast static site builder without all the annoying dependency tracking.
-   Slick provides helpers for loading in blog-post-like things using Pandoc under the hood;
    -   This means that if Pandoc can read it, you can use it with Slick!
    -   Write your blog posts in Markdown or LaTeX and render it to
        syntax-highlighted HTML!
    -   Slick processes Pandoc (and LaTeX) metadata into a usable form (as an
        [Aeson](https://hackage.haskell.org/package/aeson) Value object) which you can manipulate as you please.
- Slick provides combinators for rendering [Mustache templates](https://mustache.github.io/)
    - Slick wraps Justus Adam's [Mustache](http://hackage.haskell.org/package/mustache-2.3.0/docs/Text-Mustache.html)
        library and provides cached template rendering with awareness of changes to templates, partials, and Mustache
        objects.
    - It's a thin wrapper so you can still use things like Mustache functions, etc. if you like!
- Provides only the individual tools without opinions about how to wire them up; if you want to load blog posts from 
    a database and render them out using Blaze html; well go ahead, we can help with that!
- Provides caching of arbitrary (JSON serializable) objects using Shake resulting in super-fast rebuild times! 

Another static site generator? What about Hakyll/Jekyll?
--------------------------------------------------------

Yup, yet another static site generator. I've tried using Hakyll and Jekyll on
different occasions and found there was too much magic going on with all of the
monadic contexts for me to understand how to customize things for my use-cases.
Even adding simple tags/categories to my blog seemed far more complex then it
needed to be; Hakyll specifically got me really bogged down; what was the
Compiler monad? How does an Item work? How do I add a custom field? Why
couldn't I just edit data directly like I'm used to doing in Haskell? They
seemed a bit too opinionated without giving me escape hatches to wire in my own
functionality. If they're working for you, then great! But they weren't working
for me, so that's where SitePipe and subsequently Slick came from.

Quick Start
---------------

Want to get started quickly? Check out the [Slick site template](https://github.com/ChrisPenner/slick-template) and follow the steps there.


# Example Site:

Here's an example of using slick to build an ENTIRE blog with full automatic asset caching.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Forward
import           Development.Shake.FilePath
import           GHC.Generics               (Generic)
import           Slick
import qualified Data.Text                  as T

outputFolder :: FilePath
outputFolder = "docs/"

-- | Data for the index page
data IndexInfo =
  IndexInfo
    { posts :: [Post]
    } deriving (Generic, Show, FromJSON, ToJSON)

-- | Data for a blog post
data Post =
    Post { title   :: String
         , author  :: String
         , content :: String
         , url     :: String
         , date    :: String
         , image   :: Maybe String
         }
    deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

-- | given a list of posts this will build a table of contents
buildIndex :: [Post] -> Action ()
buildIndex posts' = do
  indexT <- compileTemplate' "site/templates/index.html"
  let indexInfo = IndexInfo {posts = posts'}
      indexHTML = T.unpack $ substitute indexT (toJSON indexInfo)
  writeFile' (outputFolder </> "index.html") indexHTML

-- | Find and build all posts
buildPosts :: Action [Post]
buildPosts = do
  pPaths <- getDirectoryFiles "." ["site/posts//*.md"]
  forP pPaths buildPost

-- | Load a post, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postContent <- readFile' srcPath
  -- load post content and metadata as JSON blob
  postData <- markdownToHTML . T.pack $ postContent
  let postUrl = T.pack . dropDirectory1 $ srcPath -<.> "html"
      withPostUrl = _Object . at "url" ?~ String postUrl
  -- Add additional metadata we've been able to compute
  let fullPostData = withPostUrl $ postData
  template <- compileTemplate' "site/templates/post.html"
  writeFile' (outputFolder </> T.unpack postUrl) . T.unpack $ substitute template fullPostData
  -- Convert the metadata into a Post object
  convert fullPostData

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
    filepaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*"]
    void $ forP filepaths $ \filepath ->
        copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  allPosts <- buildPosts
  buildIndex allPosts
  copyStaticFiles

-- | Kick it all off
main :: IO ()
main = do
  let shOpts = forwardOptions $ shakeOptions { shakeVerbosity = Chatty}
  shakeArgsForward shOpts buildRules
```

Not pictured above is:

- Using custom Pandoc readers to load other document types, there are many helpers for this in the [slick library](https://hackage.haskell.org/package/slick)
- Using custom build tools like sassy css or js minifiers; you can do these things using [Shake](https://hackage.haskell.org/package/shake) directly.


# Caching guide

Shake takes care of most of the tricky parts, but there're still a few things you need to know.

Cache-busting in Slick works using [`Development.Shake.Forward`](https://hackage.haskell.org/package/shake/docs/Development-Shake-Forward.html). The idea is that you can wrap actions with [`cacheAction`](https://hackage.haskell.org/package/shake-0.18.3/docs/Development-Shake-Forward.html#v:cacheAction), providing an unique identifier for each time it runs. Shake will track any dependencies which are triggered during the first run of that action and can use them to detect when that particular action must be re-run. Typically you'll want to cache an action for each "thing" you have to load, e.g. when you load a post, or when you build a page. You can also nest these caches if you like.

When using `cacheAction` Shake will automatically serialize and store the results of that action to disk so that on a later build it can simply 'hydrate' that asset without running the command. For this reason, your data models should probably implement `Binary`. Here's an example data model:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Data.Aeson (ToJSON, FromJSON)
import Development.Shake.Classes (Binary)
import GHC.Generics (Generic)

-- | Data for a blog post
data Post =
    Post { title   :: String
         , author  :: String
         , content :: String
         , url     :: String
         , date    :: String
         , image   :: Maybe String
         }
    deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)
```

If you need to run arbitrary shell commands you can use [`cache`](https://hackage.haskell.org/package/shake-0.18.3/docs/Development-Shake-Forward.html#v:cache); it will do its best to track file use during the run of the command and cache-bust on that; results may vary. It's likely better to use explicit tracking commands like `readFile'` when possible, (or even just use `readFile'` on the files you depend on, then throw away the results. It's equivalent to explicitly depending on the file contents).

Shake has many dependency tracking combinators available; whenever possible you should use the shake variants of these (e.g. `copyFileChanged`, `readFile'`, `writeFile'`, etc.). This will allow shake to detect when and what it needs to rebuild.

Note: You'll likely need to delete `.shake` in your working directory after editing your `Main.hs` file as shake can get confused if rules change without it noticing.
