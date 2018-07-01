---
title: Caps-lock
author: Chris Penner
date: May 25, 2014
tags: [design, ux]
image: capslock.png
---

Any time I consider decisions of designers past, my mind always drifts towards the keyboard. It is a ubiquitous piece of hardware that most take for granted. If you take more than a second to think about it, it's a very strange design. The letters seem to be placed without rhyme or reason, most layouts have rows staggered by a seemingly arbitrary amount from one another, some keys are mirrored on either side (shift, control, etc) others are not (tab, return). It seems as though no-one in their right mind would ever design such a piece of work! The keyboard is the result of years of 'legacy' design, one or two things get carried over from iteration to iteration and over time the sense of it all is lost.

Even stranger is that the nonsensical layout doesn't tend to slow us down in the slightest. Once learned it is sufficiently fast. Studies have shown that even laboriously designed key layouts (Dvorak) provide only very modest improvements to typing speeds.

The worst offender of design legacy that I have yet to come across is the Caps-lock key. Here's just a few of the many reasons why:

Caps-lock is the only modal key on the keyboard, (i.e. press to engage, again to disengage), this is extremely unintuitive and is a source of constant frustration. Consult the brilliant [Aza Raskin](http://www.azarask.in/blog/post/is_visual_feedback_enough_why_modes_kill/) for more on how modes break things. People begin to type without knowing that Caps-lock is engaged and after writing one or two lines they notice that they've been yelling the whole time. They must then delete the whole thing because for some reason there's STILL no easy way to switch cases on already typed text in most editors.

This would be bad enough as-is, however someone had the gall to place this evil key in prime real-estate where it is easily accessible, and is often pressed by accident.I don't know how it ended up where it is, but I do know that it shouldn't be there. Why not use this position for shift or Ctrl/Cmd ? I can't think of any good reasons, can you?

Caps-lock offends again in the behaviour department. Standard Caps-lock behaviour is nonsensical. At first glance it appears as though engaging Caps-lock simply locks the shift key ON, though when one attempts to type a symbol or number key, one finds that this isn't the case. This destroys the initial mental model that is formed and forces each person to learn a completely new typing paradigm for these few small use-cases.

I don't we ended up here, but I think it's about time to start a trend towards making the Caps-lock key useful again or deprecating it from future designs. You can start right now by rebinding it to something useful to you and encouraging your friends to do the same. Being a faithful Vim user I've bound it to act as Escape on every system I own. If you're not a Vim user, I'd recommend trying Ctrl/Cmd. Rebinding on OSX is as simple as looking through the system settings and changing the modifier keys, for more complex mappings I recommend [PCKeyboardHack](http://pqrs.org/macosx/keyremap4macbook/pckeyboardhack.html.en). For Windows I'd check out [AutoHotKey](http://www.autohotkey.com/). For Linux try googling an appropriate xmodmap command.
