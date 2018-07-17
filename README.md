# Slick

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

Also check out the [example site](https://github.com/ChrisPenner/Slick/blob/master/example-site/app/Main.hs)!

# Overview

Here's a quick overview of what Slick can do:

-   Slick provides helpers for loading in blog-post-like things using Pandoc
    under the hood;
    -   This means that if Pandoc can read it, you can use it with Slick!
    -   Write your blog posts in Markdown or LaTeX and render it to
        syntax-highlighted HTML!
    -   Slick processes Pandoc (and LaTeX) metadata into a usable form (as an
        Aeson Value object) which you can manipulate as you please.
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

The easiest way to get started is to clone this repo and try out
the example in the example-site directory. 

You can build the example using Stack by `cd`ing into the directory and running
`stack build && stack exec example-site-exe site`. This creates a 'dist' folder with the
results of the build. A quick way to serve the site is to use [Serve](https://www.npmjs.com/package/serve).

```shell
$ npm install -g serve
serve dist
```

Then navigate to the port which is serving (usually http://localhost:3000 or http://localhost:5000 )


# Example Site:

Here's an example of using slick to render out the posts for a pretty simple blog;

```haskell
module Main where

import qualified Data.Text as T
import Development.Shake
import Development.Shake.FilePath
import Data.Foldable
import Slick


-- convert a source filepath to a build filepath
-- e.g. site/css/style.css -> build/css/style.css
srcToBuild :: FilePath -> FilePath
srcToBuild path = "build" </> dropDirectory1 path

main' :: IO ()
main' =
  shakeArgs shakeOptions $ do
    -- Require all the things we need to build the site
    -- For this simplified example we'll just copy static assets and build a page for each post
    "site" ~> need ["static", "posts"]
    -- Require all static assets
    "static" ~> do
      staticFiles <- getDirectoryFiles "." ["site/css//*", "site/js//*", "site/images//*"]
      let copyStaticFile path = copyFileChanged path (srcToBuild path)
      traverse_ copyStaticFile staticFiles
     -- Find and require every post to be built
     -- this uses the `~>` 'phony' rule because it doesn't actually write any files on its own
    "posts" ~> do
      postPaths <- getDirectoryFiles "site/posts" ["*.md"]
      -- We tell shake we need to build each individual post
      -- We require each post separately so that Shake can cache them individually
      need (((-<.> "html") . srcToBuild) <$> postPaths)
     -- rule for actually building posts
    "build/posts//*.html" %> \out -> do 
      -- Recover the path where the source file for the post should be
      let srcPath = (dropDirectory1 out) -<.> "md"
      fileContents <- readFile' srcPath
      -- Load a markdown source file into an Aeson Value 
      -- The 'content' key contains an html-rendered string
      -- Any metadata from a yaml block is loaded into the appropriate keys in the Aeson object
      -- e.g. author, date, tags, etc.
      postData <- markdownToHTML . T.pack $ fileContents
      -- Load a mustache template using using cache if available
      template <- compileTemplate' "site/templates/post.html"
      -- Fill in the template using the post metadata/content
      writeFile' out . T.unpack $ substitute template postData
```

Not pictured above is:

- Deserializing post metadata into an object which implements `FromJSON`
- Using custom Pandoc readers to load other document types
- Using `jsonCache`s to cache intermediate JSON results to improve build times and simplify logic.
