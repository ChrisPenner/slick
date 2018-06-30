---
title: "Gem: Duckling"
author: Chris Penner
date: Feb 21, 2015
tags: [open-source, clojure]
categories: open-source-gems
language: Clojure
type: Dynamic Customizable Parser
image: duckling.png
links: Duckling|http://duckling-lib.org/, Github|https://github.com/wit-ai/duckling
---

Duckling is a very interesting project that I think exemplifies many great
design principles. It's a parser written in Clojure that can turn natural
language sentences into structured computer readable data. Clojure, if you
haven't heard, is a Lisp that runs on Java's virtual machine. Now's a good a
time as any to check it out!

Here are some things Duckling can understand:

* "from 9:30 - 11:00 on Thursday
* "the day before labor day 2020"
* "thirty two Celsius"
* "seventh"

Some of the design decisions that really set Duckling apart:

* Extensibility: Easily write your own set of rules to make it work for your own purposes.
* Probabilistic: Duckling doesn't always know what's right, but it'll take its
    best guess and tell you how sure it is.
* Data Agnostic: Duckling doesn't make assumptions about what you need, it can
    be trained to do whatever you like.

Go ahead and check out the docs and give building your own parser a try:
[Duckling](http://duckling-lib.org/ ).

Follow me on twitter [@chrislpenner](http://www.twitter.com/chrislpenner) to catch new articles as they come!
