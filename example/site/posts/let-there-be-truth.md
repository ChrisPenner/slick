---
title: Let there be Truth
author: Chris Penner
date: Jan 2, 2016
tags: [programming]
description: Let's re-visit truthy/falsy.
---

Okay, so here's the deal. The idea of "truthy" and "falsy" values is a pretty
common language pattern these days that saves us all some time and effort. I've
been thinking about it lately and I think we made a pretty big mistake as to
the implementation of this idea in most languages, namely that 0 is usually
considered "falsy". Why did we do that? Yes, I know that it's how false was
represented in C, but modern languages aren't C and they can make their own
choices. We have types and classes and all sorts of nice tools now, we don't
need 0 to represent false.

So what should 0 be then? I think we should revisit this and actually think
about it rather than just allowing old limitations to make our decision for us.
I'm going to make the case that 0 should be truthy, the argument for this is
very simple: 0 is a value. In most cases where I've seen truthyness and
falsyness used in code it's used to check whether a variable has a value;
something like `if(account){do stuff with account}`. We need to check that the
'account' we returned from a function or API call actually exists before we
perform operations on it, ensuring that it isn't 'null' or 'None' or something
like that. This case works great, but what about this:
`if(number_of_accounts){do stuff} else {raise APIError}`? Granted, this is a
simplified case that won't come up often, but in this case if there are 0
accounts, our code will raise an APIError rather than executing our operation.
In this case 0 is clearly a value, and so should be considered truthy.

While the previous example may seem contrived, it comes from a real-life case
that I dealt with at work one day. We were doing some pretty complex work with
web forms using JavaScript and had multiple field-types in the form. Some of
these fields used numerical values, and since
`if (value !== null && value !== undefined)` is a bit wordy, in most cases we
were just using `if (value)`. This worked great in almost all cases, including
checking whether or not the user had typed in a text field (`""` is falsy).
Unfortunately we hadn't handled the case where the value of a numerical field
was 0, and were incorrectly throwing validation errors. We knew 0 was a value,
but JavaScript disagreed and treats it as falsy, causing us a bug or twelve.

I'm sure we're not the only ones to have made that mistake. Clever folks can
probably come up with some case where it makes sense for 0 to be falsy, but I
think that the value-checking scenario I've presented above is the most common
use-case of truthy/falsy by far.

It's unfortunate, but languages are largely undecided on truthy/falsy. Python
has all of `'', 0, {}, []` and `None` as falsy values, in JavaScript
`0, '', null,` and `undefined` are all falsy, but `[]` and `{}` are truthy! PHP
even considers `'0'` to be false! Ruby has the strict definition that only nil
and False are considered falsy, everything else (including `0, '', [], {}`) are
ALL considered true!

I'm still undecided as to the fate of `'', [],` and `{}`, but I think it's time
for 0 to be truthy.
