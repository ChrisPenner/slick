---
title: Vim vs. Emacs?
author: Chris Penner
date: Feb 6, 2015
tags: [vim]
image: vim-vs-emacs.png
---

So about a year ago I realized that as someone going into Computer Science as
a career I would be typing for the rest of my life. Somewhere on the vastness
of the internet I read that learning how to properly use a text editor (or, how
to use a proper editor) would not only help me type faster, but also that being
able to get what's in my head onto the screen efficiently would help keep me
focused on the task at hand. These posts all came with the disclaimer that it
would take time, effort, and that learning something new would slow me down at
the start. However, if I can spend a few hours here and there to save myself
countless hours throughout my career, it doesn't take complex mental math to
see that it's a worthwhile thing to do.

I started researching the best editor to learn, there're hundreds out there,
and most shortcuts and advanced techniques tend to be non-transferable (at
least among the more complex editors). After a short period of watching some
videos and reading a thing or two I quickly uncovered the presence of the
everlasting holy war between Emacs and Vi users. Personally I've never been
interested in participating in fanboy-ism. I don't particularly care what
anyone else uses; as long as I'm content and efficient with what I have.
Unfortunately though, due to the holy war it's nearly impossible to get any
sort of objective assessment of the pros and cons of each editor.

Nonetheless I picked one and started spending some time with it. One thing is
for certain, learning a new system definitely changes the way you approach
data entry. I felt pretty much useless and slow at first, but held to my
stubbornness and waited it out. It wasn't long before I had the basics down,
and I realized that having something so amazingly customizable was really an
amazing thing. In fact, thinking of it now I can't come up with a single other
system that I use that offers this level of highly accessible customization. My
car doesn't let me record a macro of me backing out of my driveway, (probably a
good thing), I can't easily get my web-browser to load specific sites dependent
on the time of day, heck I can't even change most keyboard shortcuts in my OS.
The customization quickly became an addiction, I'd think constantly about how I
could improve my work-flow or shave a few keystrokes off of a task I do often.
Granted, all this thought and consideration often caused me to take an hour or
so to figure out something that saved me a total of 30 seconds; but in that
hour I'd also learn 2 or 3 other tricks that would also save me 30 seconds each
time I used them. It became really fun actually to find new tricks and improve
my expertise, and now I consider myself something of a Guru in my editor of
choice (though I still have endless amounts to learn).

This post isn't to tell you which editor to use, I'd sooner help you decide
whether you should bother learning one at all. First, if you're not a
programmer, writer, or typist, I'd say it's probably just not worth the effort.
I absolutely love these editors, but that's because as a programmer I'm often
doing complicated reformatting, refactoring, editing dozens of files at a time,
and testing code alongside it. If all that you do is type up an essay now and
again or write up your grocery list, you're just not going to get a very good
return on your investment. If you fit into one of the typing-centric categories
however it MAY be worth your while. If you spend a lot of time in code then I'd
say it's worth it (even if you're already far along in your career). Note that
Vim and Emacs actually do relatively little in the way of helping with your
TYPING, but rather help almost exclusively with EDITING and ORGANIZATION.

Without further delay, here's a list of objective (see: opinionated) pros and
cons (take with a grain [or boulder] of salt).

## Emacs

Emacs is often mocked by Vim users as "A great operating system, lacking only a
decent editor", while Emacs users would disagree, there's still a shadow of
truth in this statement. Emacs prides itself on being able to organize your
projects, write your email, play Tetris, compile your code, and even tie your
shoes for you in the morning! (Oh and it'll edit text too!)

This means that if you choose Emacs you'll likely end up using Emacs for almost
everything text related, which is great actually because it means you'll only
need to learn one set of shortcuts and one interface.

Emacs is also great (and far ahead of Vim) when it comes to doing more than one
thing at once, and for being able to run and check code as you work on it. It's
the defacto editor of most Lisps (it's also written in a Lisp variant, which
helps) because it's a cinch to get a shell or REPL running alongside your
project. All of these things are possible in Vim too of course, though you'll
be in for more than a few headaches.

The downsides of emacs include the famed 'Emacs-pinky', a reference to the
strain and difficulty of inputting some of Emacs's long mapping sequences.
Since Emacs has decided to leave the keyboard open for typing it means all
editor commands and shortcuts use one or more modifiers like control or alt to
enter. This has the benefit of letting beginners type away on the keyboard as
they expect it to work, but these complicated sequences can get tiresome and
difficult to remember later on.

Emacs has **strong** extensibility in the way of plugins and sheer Lisp
hackability. If there's something you want to do, you can probably find an
Emacs plugin to help you do it, or build one yourself. You'll need to learn a
bit of eLisp to do accomplish anything, but it makes sense and comes with a lot
of power once you get used to it. Though honestly in most cases whichever
functionality you require is probably already a part of some plugin in the
repository.

## Vim

First off, Vim is a modal editor, that is to say that keys on your keyboard
will do different things depending on which state the editor is in. This is
both its weakest and strongest point. Most user interface designers will tell
you that modes should be avoided whenever possible, consult the insightful [Aza
Raskin](http://www.azarask.in/blog/post/is_visual_feedback_enough_why_modes_kill/)
for further study. However, in this case the modes are central to the whole
design, so although they definitely confuse new users, seasoned Vimmers never
forget which mode they're in because they use them very particularly, staying
in 'normal' mode always except when switching to insert or visual mode for a
quick change.

Vim takes a different design philosophy and runs with it. Vim is about having a
dialog with your editor. You tell it what you want it to do and where to do it
and Vim will happily oblige. It's best to think of Vim commands and shortcuts
more as a language than as individual keypresses. For example, to change a
paragraph to something else you position the cursor within the paragraph and
press the keys (ignore the quotes) "cip". This is a small statement in Vim's
'language' that states (c)hange (i)nside this (p)aragraph. It has a verb
(change) and an object to do the verb to (inside the paragraph). This system
makes it very easy to remember Vim commands because you only need to spell out
what you'd like to do (most keys have a pretty good mnemonic associated with
them). Once you learn an action for use in one area you can automatically
assume it'll also work on the other object and motion commands you already
know.

Vim excels at editing the text that's in front of you as quickly and
efficiently as possible. What it lacks in organization it makes up for in
speed. It boots up in milliseconds and works just as well over ssh as it does
locally. It's installed *almost* everywhere and like Emacs has a large userbase
that is constantly adding functionality in the way of plugins.

Vim is very easy to customize, maybe not as easy as ticking boxes in a
preferences panel, but you can get it to do almost anything you'd like if you
think about it. One of the beautiful things about creating vim commands or
mappings is that it uses the same interface as normal editing. The mapping you
need is exactly what you'd type inside the editor. This means that the more you
learn in the main editor, the more customization options you unlock.

Unfortunately if you'd like to write any plugins or more complex functions
you'll need to learn some Vimscript, which honestly is simply an atrocious
language (nearly everyone agrees).

Another area Vim currently has trouble is mostly related to concurrency. Vim is
primarily single-threaded and so can't do more than one thing at a time. This
currently is being addressed in an offshoot called NeoVim, (see my post on that
[here](http://www.chrispenner.ca/post/gem-neovim)), though it's got a bit of a
way to go yet. Vim isn't great at multitasking or doing complex tasks like
email or chat, but it's blazing fast at doing the editing it's designed for.

## Summary

So, there's good and bad to each, though they definitely do fill two different
niches at the end of the day, if only we could have both... oh wait! There's a
way to do that actually. There's a plugin called Evil that emulates Vim's
modal interface almost flawlessly within Emacs. This allows the quick and
effective editing commands of Vim within the adaptability and all-inclusiveness
of Emacs. Some would say this is the way to go, the best of both worlds, but
the jury is still out on this one.

Some things to check out (check back for more posts on getting started soon):

* Type vimtutor on your terminal to get started on Vim.
* Download & open Emacs then press Ctrl + h, t for an Emacs starter.
* Bling has written some great articles on Emacs, Vim, and their intersection
    [here,](http://bling.github.io/blog/2013/10/16/emacs-as-my-leader-evil-mode/)
    [here,](http://bling.github.io/blog/2013/10/27/emacs-as-my-leader-vim-survival-guide/)
    and [here.](http://bling.github.io/blog/2013/10/16/emacs-as-my-leader-evil-mode/)

Anyways, I hope you consider putting in a bit of an investment to save yourself
time in the long run! It's totally worth it, no matter which tool you use
(Sublime Text is pretty good too!). Drop a comment or find me on twitter if you
have any questions. Cheers!
