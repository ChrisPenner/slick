---
title: "ASTs with Fix and Free"
author: Chris Penner
date: Feb 24, 2018
tags: [haskell, programming]
description: Using Fix and Free datatypes to represent compiler ASTs
image: tree.jpg
---

I've been working on a [toy compiler](https://github.com/ChrisPenner/Candor)
lately so I've been thinking about
[ASTs](https://en.wikipedia.org/wiki/Abstract_syntax_tree)! It's a new thing
for me and I've gotten a bit obsessed with the idea of simplifying both the
representation of the tree itself as well as the code to interpret it.

ASTs are (typically) **recursive** data-types; this means that within the
data-type they have an embedded instance of the same type! The simplest version 
of a recursive tree we can look at is actually a simple list! A list is a recursive
(degenerate) tree where every node has 0 or 1 branches. Here's how the definition
of a simple List AST might look in Haskell:

```haskell
data List a = Cons a (List a) | End
```

Contrary to what your kindergarten teacher taught you, this is one case where
it's okay to use the term in its own definition!

A slightly more complex AST for a toy calculator program might look like this:

```haskell
data Op = Add | Mult
data AST =
  BinOp Op AST AST 
  | Num Int
```

In this case we've defined a recursive tree of math operations where you can
add or multiply numbers together. Here's how we'd represent this simple math
expression `(1 + 2) * 3`:

```haskell
simpleExpr :: AST
simpleExpr = BinOp Mult (BinOp Add (Num 1) (Num 2)) (Num 3)
```

Maybe not the easiest for a human to read, but it's easy for the computer to
figure out! We won't bother writing a parser in this post, instead we'll look
at other possible ways we can represent these ASTs with data structures that
give us tools to work with them.

## Recursion Schemes

Recursion schemes are a pretty complex subject peppered with [zygohistomorphic
prepromorphisms](https://www.reddit.com/r/programming/comments/6ml1y/a_pretty_useful_haskell_snippet/c04ako5/)
and things; but don't fret, we won't go too deep into the topic, instead we'll
just touch on how we can use the general recursion folding function `cata` to
interpret generic ASTs in a really clean fashion!

The core notion of the [recursion-schemes
library](https://hackage.haskell.org/package/recursion-schemes) is to factor
out the recursion from data-types so that the library can handle any
complicated recursive cases and make it easy for you to express how the
recursion should behave.

There's a bit of a catch though, we don't get all that for free, we first need
to refactor our data-type to **factor out the recursion**. What's that mean?
Well basically we need to make our **concrete** data-type into a **Functor**
over its recursive bits. It's easier to understand with a concrete example;
let's start with our `List` example from earlier:

```diff
- data List a    = Cons a (List a) | End
+ data ListF a r = Cons a r        | End
```

See the difference? We've replaced any of slots where the type **recursed**
with a new type parameter `r` (for `*r*ecursion`). We've also renamed our new
type to `ListF` as is the convention with recursion schemes. The `F` stands for
`Functor`, representing that this is the version of our data-type with a
Functor over the recursive bits.

How's our AST look if we do the same thing? Let's take a look:

```diff
data Op = Add | Mult
- data AST =
-   BinOp Op AST AST 
-   | Num Int
+ data ASTF r =
+   BinOpF Op r r 
+   | NumF Int
    deriving (Show, Functor)
```

Pretty similar overall! Let's move on to representing some calculations with
our new type!

## Avoiding Infinity using Fix

If you're a bit of a keener you may have already tried re-writing our previous
math formula using our new AST type, and if so probably ran into a bit of a
problem! Let's give it a try together using the same math problem `(1 + 2) * 3`:

```haskell
simpleExpr :: ?
simpleExpr = BinOpF Mult (BinOpF Add (Num 1) (Num 2)) (Num 3)
```

We can write the expression out without too much trouble, but what type is it?

The type of the outer layer is `ASTF r` where `_` represents the recursive
portion of the AST; if we fill it in we get `ASTF (ASTF r)`, but the `r` ALSO
represents `ASTF r`; if we try to keep writing this in we end up with:
`ASTF (ASTF (ASTF (ASTF (ASTF (ASTF ...)))))` which repeats ad nauseum.

We really need some way to tell GHC that the type parameter represents infinite
recursion! Luckily we have that available to us in the form of the `Fix`
newtype!

We'll start out with the short but confusing definition of `Fix` lifted straight from
the [recursion-schemes](https://hackage.haskell.org/package/recursion-schemes-5.0.2/docs/Data-Functor-Foldable.html#t:Fix) library

```haskell
newtype Fix f = Fix (f (Fix f))
```

Short and sweet, but confusing as all hell. What's going on? Well basically
we're just 'cheating' the type system by deferring the definition of our type
signature into a lazily evaluated recursive type. We do this by inserting a new
layer of the `Fix` data-type in between each layer of recursion, this satisfies
the typechecker and saves us from manually writing out an infinite type. There
are [better explanations](https://stackoverflow.com/a/45916939/3907685) of
`Fix` out there, so if you're really set on understanding it I encourage you to
go dig in! That said, we really don't need to fully understand how it works in
order to use it here, so we're going to move on to the fun part.

Here's our expression written out using the `Fix` type, notice how we have a
`Fix` wrapper in between each layer of our recursive type:

```haskell
simpleExprFix :: Fix ASTF
simpleExprFix = Fix (BinOpF Mult (Fix (BinOpF Add (Fix (Num 1)) (Fix (Num 2)))) (Fix (Num 3)))
```

At this point it probably just seems like we've made this whole thing a lot
more complicated, but hold in there! Now that we've factored out the recursion
and are able to represent our trees using `Fix` we can finally reap the benefits
that `recursion-schemes` can provide!

## Using `cata`

The recursion-schemes library provides combinators and tools for working with
recursive datatypes like the `ASTF` type we've just defined. Usually we need to
tell the library about how to convert between our original recursive type
(`AST`) and the version with recursion factored out (`ASTF`) by implementing a
few typeclasses, namely the `Recursive` type and the `Base` type family; but as
it turns out any `Functor` wrapped in `Fix` gets an implementation of these
typeclasses for free! That means we can go ahead and use the
`recursion-schemes` tools right away!

There are all sorts of functions in `recursion-schemes`, but the one we'll be
primarily looking at is the `cata` combinator (short for `catamorphism`). It's
a cryptic name, but basically its a fold function which lets us collapse our
recursive data-types down to a single value using simple functions.

Here's how we can use it:

```haskell
interpret :: Fix ASTF -> Int
interpret = cata algebra
  where
    algebra :: ASTF Int -> Int
    algebra (Num n) = n
    algebra (BinOpF Add a b) = a + b
    algebra (BinOpF Mult a b) = a * b
```

Okay so what's this magic? Basically `cata` knows how to traverse through a
datatype wrapped in `Fix` and "unfix" it by running a function on each level of
the recursive structure! All we need to do is give it an `algebra` (a function
matching the general type `Functor f => f a -> a`).

Notice how we never need to worry about evaluating the subtrees in our AST?
`cata` will automatically dive down to the bottom of the tree and evaluate it
from the bottom up, replacing the recursive portions of each level with
the **result** of evaluating each subtree. It was a lot of setup to get here,
but the simplicity of our algebra makes it worth it!

## Using Free in place of Fix

Using `Fix` and `recursion-schemes` is one way to represent our AST, but there's
another that I'd like to dig into: Free Monads!

Free monads are often used to represent DSLs or to represent a set of commands
which we plan to interpret or run **later on**. I see a few parallels to an AST
in there! While not inherently related to recursion we can pretty easily
leverage Free to represent recursion in our AST. I won't be going into much
detail about how Free works, so you may want to read up on that first before
preceeding if it's new to you.

Let's start by defining a new version of our AST type:

```haskell
data Op = Add | Mult
    deriving Show

data ASTFree a =
  BinOpFree Op a a
  deriving (Show, Functor)
```

Notice that in this case we've removed our `Num Int` branch, that means that
the base `ASTFree` type would recurse forever if we wrapped it in `Fix`, but as
it happens `Free` provides a termination branch via `Pure` that we can use
as a replacement for `Num Int` as our Functor's fixed point (i.e. termination point).

Here's our original expression written using Free:

```haskell
simpleExprFree :: Free ASTFree Int
simpleExprFree = Free (BinOpFree Mult (Free (BinOpFree Add (Pure 1) (Pure 2))) (Pure 3))
```

Notice how in this case we've also extracted the type of our terminal
expression (`Int`) into the outer type rather than embedding it in the `AST`
type. This means we can now easily write expressions over Strings, or Floats
or whatever you like, we'll just have to make sure that our interpreter can
handle it.

Speaking of the interpreter, we can leverage `iter` from `Control.Monad.Free` to 
fill the role that `cata` did with our `Fix` datatype:

```haskell
interpFree :: Free ASTFree Int -> Int
interpFree = iter alg
  where
    alg (BinOpFree Add a b) = a + b
    alg (BinOpFree Mult a b) = a * b
```

Not so tough! This may be a bit of an abuse of the Free Monad, but it works
pretty well! Try it out:

```haskell
>>> interpFree simpleExprFree
9
```

You can of course employ these techniques with more complex ASTs and transformations!

