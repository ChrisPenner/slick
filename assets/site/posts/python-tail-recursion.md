---
title: Tail Recursion In Python
author: Chris Penner
date: Jul 26, 2016
tags: [python, programming]
description: Tail Recursion in python without introspection
image: ouroboros.jpg
---

Some programming languages are [tail-recursive](https://en.wikipedia.org/wiki/Tail_call), essentially this
means is that they're able to make optimizations to functions that return the result of calling themselves. That
is, the function returns **only** a call to itself.

Confusing, I know, but stick with me. It turns out that most recursive functions can be
reworked into the tail-call form. Here's an example of the factorial function in it's original form, then reworked
into the tail-call form.

<script type="text/javascript" src="https://gist.github.com/ChrisPenner/c0b3f4feb054daa2f6370d2e9961d6d3.js"></script>

They both look similar, and in fact the original even **looks** like it's in the tail call form, but since there's
that pesky multiplication which is outside of the recursive call it can't be optimized away.
In the non-tail version the computer needs to keep track of the number you're going to multiply it with, whereas in
the tail-call version the computer can realize that the only work left to do is another function call and it can
forget about all of the variables and state used in the current function (or if it's really smart, it can
re-use the memory of the last function call for the new one)

This is all great, but there's a problem with that example, namely that python doesn't support tail-call
optimization. There's a few reasons for this, the simplest of which is just that python is built more around the idea
of iteration than recursion.

But hey, I don't really care if this is something we should or shouldn't be doing, I'm just curious if we can!
Let's see if we can make it happen.


<script type="text/javascript" src="https://gist.github.com/ChrisPenner/c958afbf6e7a763c188d8b83275751bb.js"></script>

Now, don't get scared by decorators if you haven't seen them before, in fact go [read about them now](http://thecodeship.com/patterns/guide-to-python-function-decorators/), basically
they're functions which are called on other functions and change the behaviour in some way.

This decorator will call the function it's given and will check to see if it wants to 'recurse'. We signal a
'recursion' by simply raising an exception with the arguments we'd like to recurse with. Then our decorator simply
unpacks the variables from the exception and tries calling the function again.

Eventually we'll reach our exit condition (we hope) and the function will **return** instead of raising an
exception. At this point the decorator just passes along that return value to whoever was asking for it.

This particular method helps out with doing recursive calls in python because python has a rather small limit to
how many recursive calls can be made (typically ~1000). The reason for this limit is (among other things) doing
recursive calls takes a lot of memory and resources because each frame in the call stack must be persisted until
the call is complete. Our decorator gets around that problem by continually entering and exiting a single call, so
technically our function isn't actually recursive anymore and we avoid the limits.

I tested out both versions, the normal version hits the tail-recursion limit at factorial(980) whereas the
tail-recursive version will happily compute numbers as large as your computer can handle.

There's an [alternative approach](http://code.activestate.com/recipes/474088-tail-call-optimization-decorator/)
that actually uses stack introspection to do it, but it's a bit more complex than the one
we built here.

Hope you learned something, cheers!
