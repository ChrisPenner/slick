---
title: "BoxKite: Open-Sourced"
author: Chris Penner
date: Mar 25, 2015
tags: [open-source, programming]
description: I've open-sourced the code that I use to generate this blog.
image: boxkite.png
---

When I was first developing interested in web-technologies (almost exactly a
year ago now) I wanted to build some things to test my skills. I've always
believed that book learning will only get you so far, you discover so much
more about a system by building something tangible with it. I decided as a
first project to make a blog for myself. I looked at things like Jekyll and
Wordpress, but I initially had trouble customizing Jekyll (though I'm sure I
could manage it now). I didn't think I'd learn what I wanted to from building
with Wordpress, so I decided to go with a custom solution.

I fiddled around and made a few handlers in a Python Google App Engine site,
adding a bit of logic to convert Markdown files into HTML and insert them
into jinja templates. This worked pretty well so I cleaned it up, added a few
functions to parse metadata about each post, using it to build a table of
contents and a site structure. Pretty soon I had a working blog framework that
I knew from front to back and it was simple enough to extend in any way I could
imagine.

The result is an adaptable and intuitive framework that for some unknown reason
I've decided to call "BoxKite". Check out the Source (and installation
instructions) here: [BoxKite](http://github.com/ChrisPenner/BoxKite)

So why should you try out BoxKite? Well, it depends on what you want to use it
for; but here are some things that I like about it:

* ALL data related to a post is stored plain-as-day in the post's markdown
    file. (I can't stress how nice this is for organizational purposes)
* No managing images or content through clunky CMS systems, just put it in the
    right folder and reference it in your post or template.
* Need to change a post or it's tags/categories/image? Just edit the text file
    and everything dependent on it will be updated when you deploy.
* Want to add something unique to your site? Just edit the jinja template (or
    CSS), everything is available to you.
* The entire site can be exported statically if you have a vendetta against
    using web-servers (or performance concerns, see the README).
* It's responsive and scales to the viewport size. It also reflows content
    properly for a good mobile experience.
* Did I mention that comments and social media connectivity are a breeze?
    They're configured by default. You just need to input your Disqus name.

Who shouldn't use BoxKite?

* People who aren't interested in learning anything about websites
* Companies with hundreds and hundreds of posts.
* Blogs with many authors, this set-up is great for personal blogs, but breaks
    down with more than a few people posting.

In conclusion, I'd highly recommend building something like this from scratch
in whatever web framework you like to use (node, rails, appengine, etc.). It's
a great way to learn, and you'll understand the whole framework better (and web
tech as a whole) as a result. This is actually my first try at open-source and
any sort of distributable project, so take it with a grain of salt, but take a look
at it, mess around with it, and let me know what you think! Cheers!

[BoxKite at Github](http://github.com/ChrisPenner/BoxKite)
