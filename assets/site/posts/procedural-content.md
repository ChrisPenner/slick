---
title: Procedural Generation is the Future
author: Chris Penner
date: Jan 3, 2015
tags: [programming]
image: no-mans-sky.png
---

I 've been noticing a trend in gaming recently towards procedurally
generated content. [Minecraft](https://minecraft.net/) has it,
[Starbound](http://playstarbound.com/) and [Terraria](http://terraria.org/)
have it, and randomized rogue-likes are running amok. No longer do we just play
the same level over and over again until we memorize it well enough (remember
Super Mario Bros?). The new gaming paradigm is one of discovery and adventure!
While I know from experience that meticulously planned dungeons and carefully
crafted levels can deliver an amazing experience, there are several reasons as
to why I think the trend towards procedurally generated content is a good one.

These are some of the primary benefits I've discovered in using procedural
generation:


1.  **PG Allows you to produce near unlimited amounts of content.**
2.  **PG Saves disk space.**
3.  **PG Spikes Creativity**


##1) pg allows you to produce near unlimited amounts of content.

If [Minecraft](https://minecraft.net/) or [Terraria](http://terraria.org/) had
been built by hand with only one world to explore, people would have figured
out its tricks, read about them online, figured out the quickest way to their
goal and would be done with them by now. A key part of what makes these games
special is that they have amazing replay value because the whole world changes
every time you start over. You can explore as far as you like, the developer
didn't need to put bounds on the world because the computer can just follow its
rules and continue to create! A procedural generation approach lets your world
create and discover itself!


##2) pg saves disk space.

One amazing and mostly unintended benefit of generating your world on the
fly is that an entire game world can be represented as a seed value of just
a few letters or numbers. If a part of the world hasn't been altered, then
any given section of that world can be regenerated as needed from the seed
value. Since math doesn't change, it will turn out the same every time. In
a world like [Minecraft](https://minecraft.net/) where the world morphs and
changes, only the differences from the generated world need to be remembered
and can be applied like a patch. This means that in a game like [No Man's
Sky](http://www.no-mans-sky.com/) with 18 quintillion planets, every one of
those planets can be remembered with a single seed value taking up no more than
a few bits.

##3)  pg spikes creativity

When things are randomly generated, sometimes they don't always go according
to plan. While this is one of the bigger frustrations with creating this sort
of game, it's also one of the best sources of inspiration. Did a bug in your
system accidentally create an entire city under the ocean biome? Cool, that
might be fun! Uh-oh, gorillas are accidentally spawning all over the north
pole, what would a tribe of arctic apes look like? The unexpected nature of
generators like this can spark some interesting ideas. I can't remember how
many times I've been cruising through Spelunky when something so beautifully
unplanned causes my run to come to a hilarious and unpredictable demise.

## Case Study

Let's examine two cases, that of Assassin's Creed and that of No Man's
Sky, which is unreleased at the writing of this article, however most
of its design principles have been announced through various developer
interviews. Assassin's Creed is made by Ubisoft, a corporate giant; ballpark
estimates for the number of employees working on [Assassin's Creed IV: Black
Flag](http://assassinscreed.ubi.com/en-us/games/assassins-creed-black-flag.aspx
) range between 900 and 1000 people. In the other corner we have Hello Games,
a team of less than a dozen people who are developing No Man's Sky, a far
reaching game about space exploration that according to the developers could
have as many as 18 quintillion possible planets. How is it that it takes a
team of over 900 people to craft one world, when a team of 10 can craft 18
quintillion? It's a matter of where they've invested their effort.

Ubisoft is using a more traditional development paradigm. They are designing
their world by hand, carefully crafting graphical assets to fit that world as
it is designed. This means that every window, building, nook and handhold are
intentionally placed, by hand, in spots that a designer chose. This method, while
effective, is clearly time consuming and can sometimes seem too contrived.

Hello Games on the other hand have decided to leverage the full power of their
paradigm and have decided to put their hard work into creating a clever system
that will do the rest of their work for them. They decided that instead of
crafting worlds, they would create a world-crafter. The initial work-load to
do this is substantial, but the payoff is that now they can create as many
worlds as they like with little effort, able to tweak their algorithm as they
go along.

The take-home point here isn't that every game should be using procedural
generation, but rather that every developer should at least **consider**
whether it's appropriate for their current use case. Who knows, could end up
saving you a ton of time and adding some awesome new features.

Cheers everyone!
