---
title: Semantic Versioning
author: Chris Penner
date: Apr 2, 2015
tags: [programming, open-source]
description:
image: versioning.jpg
---

So! I'm going to talk about Semantic Versioning today because it's something
that I think *everyone* should be using. Why? Because it takes something that
is largely arbitrary and meaningless and redeems it by giving it meaning. A
side effect of the system is that everyone thinks a little more about how their
software changes affect those who actually use it.

How's this whole Semantic Versioning thing work? Well essentially it's a set of
conventions for how version numbers are changed when software is altered. I
recommend reading the whole description [here](http://semver.org/), but I'll
give you the TL;DR version. The idea is that versions should take the form
X.Y.Z where each letter is an integer (e.g. 2.5.17). Each number has it's own
meaning; MAJOR.MINOR.PATCH

X = MAJOR-version: This is incremented any time the new API is
not back compatible with an API you've previously shipped. It doesn't matter how
different it is, if the API acts differently, change the MAJOR version.

Y = MINOR-version: This is incremented when the API is changed, but it's
completely back compatible with previous versions of this MAJOR release. Use
this when ADDING features to your API.

Z = PATCH-version: This is incremented when you make bugfixes that don't affect
the API.

The idea is to allow devs to reason about when/how to update their
dependencies. Under this system, the dev knows that they can safely update to
any version that changes the MINOR or PATCH versions, but that a change in the
MAJOR version will mean API alterations which may break their application.

It's as simple as that. Read [semver.org](http://semver.org/) for more info on
all of this, and start using this system TODAY!

Cheers!
