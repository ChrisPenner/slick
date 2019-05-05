---
title: Set the Data Free
author: Chris Penner
date: Aug 2, 2014
tags: [open-source]
image: set-the-data-free.png
---

So lately I've been noticing something. The way we consume our data is changing, suddenly just having data isn't good enough, it's all about presentation. Most modern data formats such as Microsoft Word documents, Powerpoint Slides and PDFs are increasingly focused on making your data LOOK good.

This is a good thing, it's good that consumer computing has developed to a point where we have enough tools to easily format and present our data the way we want, and it's great that as a result we can send messages not only through the text itself, but also through the way we display it, but we're losing something valuable with this transition as well: The ability to manipulate plain-old vanilla text. Most data is now being trapped inside proprietary layers of code. While tools like grep can still sometimes decipher these encodings and still find what you're looking for, piping text from a Powerpoint file, formatting the string with Unix utilities, then compiling it with several other Powerpoint files and their text would not be a pleasent experience.

I understand that all of this formatting is complicated, that it would not be easy to encode a modern day Word document without using special characters and bytes and bytes of stored settings; however, I think it's worth looking for a compromise.

Two options come readily to mind. Perhaps text from Word Documents, Powerpoints,  PDF files, and other proprietary formats could be included in full as a precursor to the needed code which would then use combinations of line-numbers or character addresses to apply formatting to code in chunks. Perhaps transformations could be performed through the use of recognized tags (similar to HTML) which could be parsed or ignored depending on context. These both have the downside of unnecessarily bloating the files and increasing the data stored on disc, and would get complicated very quickly with more complex presentations, however they would allow common command-line programs to be taught to understand them properly and allow full command-line piping and functionality.

Unfortunately, it would be very difficult (see: impossible) to get vendors to agree on a set convention for this and would most-likely lead to big tangled mess of competing standards, but as I learn more and more about Unix utilities and the wealth of functionality that they provide, it seems a crying shame to invalidate them all just because we'd like a bold word or want to position our margins correctly. Ideally there would be a strong way to separate content from formatting a la HTML and CSS.

It is also unfortunate that so few people know and use the command line these days, there are so many shortcuts and so much functionality in their computers that they're missing. Let's hope more will be inspired to explore!
