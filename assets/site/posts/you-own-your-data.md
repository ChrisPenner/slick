---
title: You Own Your Data
author: Chris Penner
date: Sep 26, 2014
tags: [open-source]
image: security-vs-privacy.jpg
---

First off, thanks for taking the time to read this, please send me a message on twitter [@chrislpenner](http://www.twitter.com/chrislpenner) if you have ideas or comments, and share this post around if you agree with it.

*Comic used with permission of [Clay Bennett](http://www.claybennett.com/).*

Okay, so what's this whole thing about? To put it in a nutshell for you: our data is important, our data isn't safe, therefore something needs to change. What is Facebook doing with your data? Are they giving it to the NSA? What if my employer finds out about such and such? We spend far too much time worrying about whether our data is safe and what would happen if these businesses that we trust with our virtual lives decide to go bad.

Most people you ask would say that Facebook (who I'll be picking on because they're most popular at the time) is free to use, but the reality is far from that, the cost is your data. Facebook doesn't work unless everyone shares data. You can't actively use your FB account unless you take the plunge and decide to give them your pictures, thoughts, buying habits, movie and music likes and dislikes... the list goes on. Everyone knows that they're giving this information away, but when asked, most would say they don't really have much of a choice. They either share their data, or Facebook becomes useless. Heck, I myself have been keenly aware of this for years, but I still participate because having access to my friend's thoughts, contact info and photos is far too convenient to justify giving up. We've grown to a place where every one of us needs social media in some form or another, that's not even a question at this point, the question then becomes: Who do we trust to handle all this data?

*Think about that for a minute, maybe even two...*

No, seriously, stop looking at this screen and actually think: Who do you really trust to handle all of your personal data? Trust is important.

Now I don't know about you, but my answer was simple, I can only trust myself. I propose we address this issue by decentralizing data storage. There's a very important clarification to make: the data is separate from the service. Facebook isn't a collection of data, it's a service that curates and presents a collection of data to you. What this means is that programs like Facebook's "news feed" could exist and be maintained separately from the data itself.

> Who do we trust to handle all this data?

I propose that as an open-source community we devise a generic social media program  which users can download and run on their computers which pulls in data and presents data about users from personal data repositories. Users can choose to host their data wherever they feel comfortable, maybe on their own secure web server, maybe on Dropbox, maybe they trust a third party site with it, maybe they can even host it on their own computer, the point is that this choice is up to the user and the user alone. This data would then be pulled down to the program when requested if and only if the user who is signed into the program has that person's permission to do so, either through a link or some sort of case by case verification system; think of friending on Facebook or sharing a document on Google Drive. These permissions can be revoked by the owner of the data at any time, or the data can simply be deleted from their own storage container.

>The data is separate from the service

Though this system has its own challenges, I believe it solves some major problems. In the new model:

* Data is decentralized. (No one company controls it all)
* Control belongs to the data's owner. (They can change, delete, or revoke the permissions of their own data)
* No middleman. (The open-source software would pull data directly from people's sources to the user's computer, no opportunity for it to be snatched up)
* Extensible. (Once everyone is hosting their own data and the process is standardized in some fashion, new programs and social networks can simply use existing data-stores and people don't need to rebuild their virtual life every 5 years)

*It'll take work, it'll be a tough change, but if we don't demand it, it'll never happen.*
