---
title: "MonadIO Considered Harmful"
author: Chris Penner
date: Sep 11, 2017
tags: [haskell, programming]
description: Avoid using IO or MonadIO directly
image: cables.jpg
---

Now that we've got the click-bait out of the way (sorry about that) we can have
a nice chat! Here's my point: MonadIO, and of course IO, are too
general. This isn't news really, it's has been addressed in many ways by many
people. Options presented in the past include using Free or Free-er monads
(e.g. the [Eff
Monad](https://leanpub.com/purescript/read#leanpub-auto-the-eff-monad)), and
these tend to work pretty well, but they're all-encompassing and intrusive,
they can be pretty tough to work into legacy projects; converting all uses of a
given effect into a Free monad can be tricky and time consuming (though
certainly can be worthwhile!).

What I'm going to talk about here is an alternative which provides most of the
benefits with a very low barrier to entry: splitting up IO into granular monad
type classes. First a quick recap:

## `Monad*` classes

I'm going to assume that most readers are at least passively familiar with
[mtl](http://hackage.haskell.org/package/mtl); if not then maybe come back to
this post later on. [mtl](http://hackage.haskell.org/package/mtl) popularized
the idea of the `Monad*` typeclasses e.g. `MonadReader`, `MonadState`, and of
course `MonadIO`. This pattern has been adopted by most modern monad-based
libraries because it allows abstracting away the concrete monad which is used
in a function to allow greater portability and re-usability.

Here's an example of an action written using type signatures using both concrete
monad types and abstract monad typeclasses:

```haskell
concreteUserIncrement :: StateT Int IO Int
concreteUserIncrement = do
  incAmount <- liftIO readLn
  modify (+incAmount)
  return incAmount

classUserIncrement :: (MonadState Int m, MonadIO m) => m Int
classUserIncrement = do
  incAmount <- liftIO readLn
  modify (+incAmount)
  return incAmount
```

The actions do the same thing, but I'd recommend the class-based approach for a
few reasons.

Firstly, it allows us to re-use this function with other monad stacks, for
instance later on let's say we realize that we'll need to have access to the
options configured for our program in a few spots. To accomodate this we add
`ReaderT Options` to our stack and end up with:
`ReaderT Options (StateT Int IO)`. In the first case we'd need to rewrite all
signatures which use the old concrete type and replace them with the new concrete
type. We could use a type alias of course, but I'm making a point here, so give me
a sec. The class-based signature is already good to go in the new monad since it still
unifies with the given requirements!

A second and perhaps more important benefit to class-based signatures is that
they make it clear which effects a function plans to use. Let's take a look at another
example:

```haskell
concreteReset :: ReaderT Options (StateT Int IO) ()
concreteReset = put 0

classbasedReset :: MonadState Int m => m ()
classbasedReset = put 0
```

Again, both implementations are the same, but what do the types tell us? Well,
the class-based approach tells us clearly that `classbasedReset` intends to
(and in fact can only) interact with the Int which we've got stored in
`StateT`. We're not allowed to do IO or check Options in there without adding
it to the signature. In the concrete case we're not given any hints. We know
which monad the action is intended to be used in; but for all we know the
implementation could take advantage of the `IO` at the base and alter the
file-system or do logging, or read from stdIn, who knows? 

Okay, so I think I've made my case that `Monad*` classes improve both code
re-usability and code clarity, but if I'm not supposed to use `IO` or even `MonadIO`
then how am I supposed to get anything done? Good question, glad you asked!

## Breaking up MonadIO

Having a `MonadState Int m` in the signature was great because it limited the
scope of what the monad could do, allowing us to see the action's intent.
`MonadIO m` is a `Monad*` class, but what does it tell us? Unfortunately it's
so general it tells us pretty much zilch. It says that we need access to IO,
but are we printing something? Reading from the filesystem? Writing to a
database? Launching nuclear missiles? Who knows!? It's making my head spin!
`MonadIO` is too general, its only method is `liftIO` which has absolutely zero
semantic meaning. Compare this to `ask` from `MonadReader` or `modify` from
`MonadState`. We can tell that these transformers have a clear scope because
they have meaningful function names.

Let's bring some semantic meaning into our MonadIO by defining a new, more
meaningful class:

```haskell
class MonadFiles m where
  readAFile :: FilePath -> m String
  writeAFile :: FilePath -> String -> m ()

instance MonadFiles IO where
  readAFile = readFile
  writeAFile = writeFile
```

Now instead of tossing around a `MonadIO` everywhere we can clearly specify that
all we really need is to work with the file system. We've implemented the interface
in the IO monad so we can still use it just like we did before.

```haskell
getDiary :: MonadFiles m => m String
getDiary = readAFile "my-diary.txt"
```

Now no-one can launch those pesky nukes when all I want to do is read my diary!
As a bonus this lets us choose a different underlying `MonadFiles` whenever we
like! For instance we probably don't need our tests to be writing files all
over our system:

```haskell
instance MonadFiles (State (M.Map String String)) where
  readAFile fileName = do
    files <- get
    let contents = fromMaybe "" (M.lookup fileName files)
    return contents

  writeAFile fileName contents = do
    modify (M.insert fileName contents)
```

Now we can substitute a `State (M.Map String String)` for `IO` in our tests to
substitute out the filesystem for a simple Map. Our actions don't care where
they run so long as the interface has files can be read and written somewhere!

I'd probably go a bit further and split this up even more granularly,
separating reading and writing files.

```haskell
class MonadFileReader m where
  readAFile :: FilePath -> m String

class MonadFileWriter m where
  writeAFile :: FilePath -> String -> m ()
```

We can get back our `MonadFiles` type class pretty easily using the
[`ConstraintKinds`](https://kseo.github.io/posts/2017-01-13-constraint-kinds.html)
GHC extension:

```haskell
{-# language ConstraintKinds #-}
type MonadFiles m = (MonadFileReader m, MonadFileWriter m)
```

As an aside, feel free to implement an instance of your interfaces for your
Free Algebras too!

Anyways, that's pretty much it, the next time you find yourself using IO or
MonadIO consider breaking it up into smaller chunks; having a separate
`MonadDB`, `MonadFiles` and `MonadHttp`, will improve your code clarity and
versatility.

Cheers!
