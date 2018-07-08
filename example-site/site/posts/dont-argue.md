---
title: The Interface We Need
author: Chris Penner
date: Feb 15, 2016
categories: thoughts
tags: [programming, python]
description: Starting with the interface can make things easier for everyone
---

I suffer from a not-so-rare condition where as soon as a problem is presented to me I immediately start trying to solve it with the tools I know well. This may not sound like a bad thing, but what happens is that I end up with a user interface built around supporting the implementation I was planning to write, which is very rarely an interface that anyone would actually like to use. In the worst case I end up changing my implementation plans along the way and now we're stuck with a crappy API designed around an implementation that doesn't even exist! I've been learning that it's usually better to design an interface that's elegant and does what you need, then the implementation will fall into place from there.

Anyway, I ended up learning this lesson once more the other day, here's the story: I was writing a laughably simple script and I wanted it to be able to accept arguments from the command line. I remembered that Python has an argument parsing module in its standard library (argparse), but upon looking it up I remembered how much of a pain it was to get all your arguments set up. Argparse is great in that it allows you to do complex things with arguments, but I think there should be an alternate path to avoid the complexity when all you need is to grab a few arguments from the command line. Sure I could have just used sys.argv manually, but I wanted to use command line '--options' and parsing those out from argv would be a royal pain.

In the end I decided to write my own little helper module for this sort of thing, which you can find here. The goal was to design the simplest possible interface that just does the right thing.

At this point I would usually write out a few example use-cases with an interface that I'd want to use even if I'm not sure it's possible to implement that way. For some reason I avoided my own advice this time and started on the implementation first. Here's an example of the interface that resulted from my original implementation:

<script src="https://gist.github.com/ChrisPenner/1436ac6d9f73dd8a9242.js?file=old_syntax.py"></script>

Hrmm, so it works, but you can see that we're writing each argument out twice. Keyword args are also doubled, and for some reason keywords aren't strings, whereas the other arguments are. The worst offender is the 'args' syntax. In order to specify we want to collect extra arguments into a list we set "args=True", then have an argument named args below. Hrmm, all of this is a little clunky, this definitely isn't an "it just works" scenario and honestly it's just as easy to screw up as using argparse in the first place. It was at this point that I realized I built it this way because it was easy to implement, not because it was easy to use! So back to the drawing board, let's design something we'd like to use first, then see if we can implement it!


<script src="https://gist.github.com/ChrisPenner/1436ac6d9f73dd8a9242.js?file=new_syntax.py"></script>

Whoah, okay that's a lot simpler! No more duplication, I'd use that! But it almost seems a bit too much like magic, is it even possible to implement it this way?
Let's try it out, one of the things we wanted was to be able to handle '--options' on the command line, and argparse has that ability, so we should probably take advantage of that. To that end we need to pass the names of the arguments to argparse to set it up, how can we do that now that we're not passing argument names to our decorator? After a quick dive into the depths of Stack Overflow I discovered what we need in the 'inspect' module from the standard library.

The **inspect** module allows us to peer into code that's running during execution. What I though was impossible is actually pretty easy to do! Using inspect's getargspec function we can get the names of the arguments of a function just like we need! From this point it was just a matter of outfitting the decorator to handle different combinations of arguments, keyword arguments, and splat arguments properly and we can end up with exactly the API we wanted! The code ends up being much cleaner too since we don't have to deal with as many edge cases.

We ended up with a much simpler interface, one that we probably wouldn't have even thought was possible if we'd started thinking about the implementation too early on. This just goes to show that designing a nice interface first can lead to better design, a much improved user experience, and in this case: cleaner code! Remember to put the interface first the next time you're implementing some new feature for your app.

You can find the full decorator [here](https://github.com/chrispenner/dont-argue), it's only a few lines long.
