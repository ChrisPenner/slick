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


# Example Site:

```haskell
main :: IO ()
main =
  shakeArgs shakeOptions $ do
    -- Require all the things we need to build the whole site
    "site" ~> need ["static", "posts", "build/index.html"]
    -- Require all static assets
    "static" ~> do
      staticFiles <-
        getDirectoryFiles "." ["site/css//*", "site/js//*", "site/images//*"]
      -- 
      need (("build" </>) . dropDirectory1 <$> staticFiles)
     -- Find and require every post to be built
    "posts" ~> findPosts
    -- Rule for handling static assets, just copy them from source to dest
    ["build/css//*", "build/js//*", "build/images//*"] |%> \out -> do
      copyFileChanged ("site" </> dropDirectory1 out) out
    -- build the main table of contents
    "build/index.html" %> buildIndex postCache
     -- rule for actually building posts
    "build/posts//*.html" %> buildPost postCache

data IndexInfo = IndexInfo
  { posts :: [Post]
  } deriving (Generic, Show)

instance FromJSON IndexInfo

instance ToJSON IndexInfo

data Post = Post
  { title :: String
  , author :: String
  , content :: String
  , url :: String
  , date :: String
  , image :: Maybe String
  } deriving (Generic, Eq, Ord, Show)

instance FromJSON Post

instance ToJSON Post

postNames :: Action [FilePath]
postNames = getDirectoryFiles "." ["site/posts//*.md"]

destToSrc :: FilePath -> FilePath
destToSrc p = "site" </> dropDirectory1 p

srcToDest :: FilePath -> FilePath
srcToDest p = "build" </> dropDirectory1 p

srcToURL :: FilePath -> String
srcToURL = ("/" ++) . dropDirectory1 . (-<.> ".html")

loadPost :: PostFilePath -> Action Post
loadPost (PostFilePath postPath) = do
  let srcPath = destToSrc postPath -<.> "md"
  postData <- readFile' srcPath >>= markdownToHTML . T.pack
  let postURL = T.pack . srcToURL $ postPath
      withURL = _Object . at "url" ?~ String postURL
      withSrc = _Object . at "srcPath" ?~ String (T.pack srcPath)
  convert . withSrc . withURL $ postData

buildIndex :: (PostFilePath -> Action Post) -> FilePath -> Action ()
buildIndex postCache out = do
  posts <- postNames >>= traverse (postCache . PostFilePath)
  indexT <- compileTemplate' "site/templates/index.html"
  let indexInfo = IndexInfo {posts}
      indexHTML = T.unpack $ substitute indexT (toJSON indexInfo)
  writeFile' out indexHTML

findPosts :: Action ()
findPosts = do
  pNames <- postNames
  need ((\p -> srcToDest p -<.> "html") <$> pNames)

buildPost :: (PostFilePath -> Action Post) -> FilePath -> Action ()
buildPost postCache out = do
  let srcPath = destToSrc out -<.> "md"
      postURL = srcToURL srcPath
  post <- postCache (PostFilePath srcPath)
  template <- compileTemplate' "site/templates/post.html"
  writeFile' out . T.unpack $ substitute template (toJSON post)

sortByDate :: [Post] -> [Post]
sortByDate = sortBy (flip compareDates)
  where
    compareDates = compare `on` date

newtype PostFilePath =
  PostFilePath String
  deriving (Show, Eq, Hashable, Binary, NFData)

```

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

