---
title: The Future of Software
author: Chris Penner
date: Sep 24, 2014
tags: [design]
categories: thoughts
image: modular.png
---

I 've always been annoyed with software, the particular reasons have changed consistently over the years, but I think that for all of us there's always been something that gets on our nerves; whether it's how your word processor never indents your lists properly, or how your computer's calendar starts on Sunday when you'd prefer to have it start on Monday.

Lately what's been bothering me is the closed-ness of most desktop software in general. Don't like the default colour-scheme? Too bad, you can't change it. Don't like the way your notes are laid out? Too bad, can't change that either. Want to use your favourite text editor instead of that tiny box they're giving you? Nope!

This problem has actually already been solved for the most part. The solution exists in *modularity*. For the uninitiated, modular design means that the way a thing works is separated into distinct sections. A home theater system would be a good example to think of: you have a source for your sound and video (A DVD player perhaps) the video signal proceeds to your Television, the sound goes to your amplifier, then continues to your speakers. Each link in this chain has a specific purpose and works independently from the other links. If you'd like to switch out your speakers, you simply unplug the old ones and plug the new ones in. Similarly with the video signal; the DVD player doesn't care where the video ends up. If you'd like to switch your Television out for a Projector, Video Recorder, or even a toaster it doesn't know the difference and continues to happily send video. Whether you'd like to view your movie on a toaster or an IMAX screen is entirely up to the user (though the viewing experience is likely to change dramatically).

>The solution exists in modularity.

Unfortunately, in the world of software the parts aren't as clearly defined. Where should one box end and another box start? Modern web technologies provide insight into this. Websites are complicated these days. On any given news website you'll have writers, designers, programmers and editors all working together to deliver a good experience. To keep these groups from stepping all over each other's toes content, presentation, and behaviour must be separated from each other. These aspects correspond to HTML, CSS, and Javascript respectively. HTML contains the content and gives the content meaning, CSS tells the browser how to present it, which colours to use and what goes where, while Javascript handles any user interaction and responds accordingly. I believe this is how we should be modelling our desktop software.

Whether this means actually using HTML, CSS and Javascript for desktop software I'm not sure (that's certainly a possible solution), but whichever tools are used, the programs created must recognize what they're actually trying to do and should focus solely on that. If a program is an email client it should handle the sending and receiving of email and should do it well, and do no more and no less. Allow the user to patch in and use any text editor they'd like to create those emails. All programs should allow mixing and matching of different program components.

A strong benefit to this approach is that it would allow programs to easily interact with one another and solve problems together. This is something that is nearly impossible to do given the current architecture. Imagine a "Dashboard" plugin that focused only on bringing multiple programs from your computer together in one place. You'd have a column of emails on the right, some favourite music playlists on the left, your favourite text editor in the middle that can send directly to Evernote, Microsoft Word, Email, or a text message. When the user chooses an action to perform the Dashboard sends the appropriate *event* to the corresponding program with any necessary text or user information as parameters. A timer app could send an email, start a song or switch into "work-mode" when certain timer events fire. Users could easily design new facades for their favourite clients so long as it sends any necessary events and data to the program behind the scenes.

Responding to any and every event in any way you like provides extensive hackability to everything.  Note-taking apps and Email clients wouldn't need to go through all the work to (poorly) implement autocompletion or spell-checks because that would be the job of the text editor (which it would do well).

I'm sure that you can see that the possibilities are nearly endless if we can just unlock this method of interaction and modularization. Everyone can work on doing just one thing well and can borrow all the other functionality from other programs.I can only hope we'll end up there eventually.

Until next time.
