---
title: "Gem: Neovim"
author: Chris Penner
date: Oct 15, 2014
tags: [open-source, vim]
categories: Open-source-gems
language: VimL, C
platforms: Mac, Linux
state: Pre-beta
type: Text-editor
program: neovim
image: neovim-logo.png
links: Neovim|http://neovim.org, Github|http://github.com/neovim/neovim
---

This is the first of a series which will highlight open-source "Gems in the rough" that is, projects which are worth taking a look at, downloading, or contributing to. Let's get started!

Neovim is a rebirth of the retro text editor [Vim](http://en.wikipedia.org/wiki/Vim_(text_editor)) (circa 1991). It's interesting that the team decided to rebuild it because Vim itself is still very much alive, there are new plugins and patches released often and development on it continues. The folks at Neovim have realized however that it's getting old and the Vim Script language that original vim plugins are written in is a syntax nightmare. It's tougher to extend and interact with than it could be, so they decided to renew the project by rebuilding the entire code-base into something easier to maintain.

Some of Neovim's driving principals are as follows:

* Allow vim to be extended in any language.
* Allow plugins to run asynchronously and send events.
* Implement an embedded text interface, ready for integration into any application.

I'll be writing more about vim and what it's capable of soon, so keep an eye out for more, and run vimtutor on your terminal to try it out! You can help out right now by checking out the source on [Github](http://github.com/neovim/neovim) or by donating on [BountySource](https://www.bountysource.com/teams/neovim/).

Follow me on twitter [@chrislpenner](http://www.twitter.com/chrislpenner) to catch new articles as they come!
