---
title: Autoenv Trick
author: Chris Penner
date: Sep 4, 2015
tags: [open-source, programming, workflow, unix]
description: Use Auto-env to streamline your workflow.
image: autoenv.gif
---

Firstly, if you haven't heard of [autoenv](https://github.com/kennethreitz/autoenv) then I suggest you
go check it out now. Basically it allows you to run arbitrary shell scripts any time you enter a directory or any of its
children, it's pretty useful.

You can do all sorts of things with this tool, though most people use it to configure their environment variables (hence
the name). I use it for that as well, but I've added a new trick.

Basically, each time you enter a project it will try to join an existing tmux session for that project, if none exist it
will create one.

Here's what's in each project's '.env' file now:

<script src="https://gist.github.com/ChrisPenner/83ad2665eed3dd5fff15.js"></script>

I use vim with tmux extensively, and so I often set up a workplace with several tmux windows
and splits. Setting all this up and remembering what I was working on every time I context switch can be a bit of
a pain, so now I use autoenv to manage it for me. What would usually happen to me is that I'd set up a tmux session with
all of this, then forget about it next time I went to work on this project, but now every time I enter a project's
directory it automagically puts me back into the session.

Simple! Now I can't forget!

