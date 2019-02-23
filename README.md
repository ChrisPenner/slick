# Slicker

`Slicker` is a static site generator written and configured using Haskell. It's a fork of the package `Slick` that extends it functionality for production usage.

`Slicker` provides a small set of tools and combinators for building static
websites on top of the [Shake](https://shakebuild.com/) build system. Shake is
adaptable, fast and  reliable

## Overview

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

## Quick Start

The easiest way to get started is to clone this repo and try out
the example in the example-site directory.

You can build the example using Stack by `cd`ing into the directory and running
```
$ stack build
$ stack exec -- example-site-exe site
```
This creates a 'dist' folder with the results of the build. A quick way to serve the site is to use internal functionality with `--preview` key without need to use external tools like

```
$ stack exec -- example-site-exe --preview
```

Then navigate to the port which is serving (usually http://localhost:3030)
