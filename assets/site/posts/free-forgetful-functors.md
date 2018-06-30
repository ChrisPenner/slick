---
title: "Free and Forgetful Functors"
author: Chris Penner
date: Jul 20, 2017
tags: [haskell, programming]
description: Building adjunctions from Free and Forgetful Functors
image: books.jpg
---

Today I'm going to continue the previous topic of Adjunctions, [last
time](/posts/adjunction-battleship) we talked about how
you can build a sensible adjunction from any Representable functor, this time
we're going to talk about a (semantically) different form of adjunction, one
formed by a pair of Free and Forgetful Functors. First I'll describe the relationship
of Free and Forgetful Functors, then we'll see how an Adjunction can making translating
between them slightly easier.

Let's define our terms, hopefully you already know what a Functor is, it's any
type with a `map` method (called `fmap` in Haskell). A Free Functor is a functor
which can embed any element "for free". So any Functor where we could just 'inject'
a value into is considered a Free Functor. If the functor has an Applicative instance
then `inject` is called `pure`.

```haskell
inject :: a -> f a
```

To do this maybe it means we make up some of the structure, or have some
default values we use in certain parts. Let's see some contrived examples of Free
Functors.

Simple single slot functors like Identity: 

```haskell
inject a = Identity a
```

Simple structures like List or Maybe or Either:

```haskell
inject a = [a]
inject a = Just a
inject a = Right a
inject a = Pair a a
inject a = repeat a
```

Or even anything paired with a monoid, since we can 'make up' the monoid's value using mempty.

```haskell
inject :: Monoid t => a -> Tagged t a
inject a = Tagged mempty a
```

Note however that some of these Free functors are unsuitable for use with
adjunctions since _*Sum*_ types like Maybe, List and Either aren't Distributive
because the number of `a` slots in the functor can change between values.

Next we need the forgetful functor, this is a functor which 'loses' or
'forgets' some data about some other functor when we wrap it. The idea is that
for each pair of Free and Forgetful functors there's a Natural Transformation
to the Identity Functor: `Forget (Free a) ~> Identity a`; and since there's an
isomorphism `Identity a ≅ a` we end up with something like
`Forget (Free a) ~> a`. This expresses that when we forget a free functor we
end up back where we started.

Let's see what 'forgetting' the info from a Free functor looks like by implementing
`forget :: Free a -> a` for different Free functors.

```haskell
-- Identity never had any extra info to begin with
forget :: Identity a -> a
forget (Identity a) = a

-- The extra info in a nonempty list is the extra elements
forget :: List.NonEmpty a -> a
forget (a:|_) = a

-- The extra info in a 'Tagged' is the tag
forget :: Tagged t a -> a
forget (Tagged _ a) = a

-- The extra info in a Pair is the duplication
forget :: Pair a -> a
forget (Pair a _) = a
```

You can imagine this sort of thing for many types; for any Comonad type we have
`forget = extract`. Implementations for `Maybe` or `Either` or `List` are a
bit trickier since it's possible that no value exists, we'd have to require a
Monoid for the inner type `a` to do these. Notice that these are the same types for
which we can't write a proper instance of Distributive, so we'll be avoiding them as
we move forwards.

Anyways, enough chatting, let's build something! We're going to do a case study in the `Tagged` type
we showed above.

```haskell
{-# language DeriveFunctor #-}
{-# language TypeFamilies #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}

module Tagged where

import Data.Distributive
import Data.Functor.Rep
import Data.Functor.Adjunction
import Data.Char

newtype Forget a = Forget { getForget :: a } deriving (Show, Eq, Functor)
data Tagged t a = Tagged
  { getTag :: t
  , untag :: a
  } deriving (Show, Eq, Functor)
```

Okay so we've got our two functors! `Tagged` promotes an 'a' to a 'a' which is
tagged by some tag 't'. We'll need a Representable instance for Forget, which
need Distributive, these are pretty easy to write for such simple types. Notice
that we have a Monoid constraint on our tag which makes Distributive possible.

```haskell
instance Distributive Forget where
  distribute fa = Forget (getForget <$> fa)

instance Representable Forget where
  type Rep Forget = ()
  index (Forget a) () = a
  tabulate describe = Forget (describe ())
```

Hopefully this is all pretty easy to follow, we've chosen `()` as the representation since
each data type has only a single slot.

Now for Adjunction! We'll unfortunately have to choose a concrete type for our
tag here since the definition of Adjunction has functional dependencies. This
means that for a given Left Adjoint there can only be one Right Adjoint. We can
see it in the class constraint here:

```haskell
class (Functor f, Representable u) => Adjunction f u | f -> u, u -> f where
```

It's a shame, but we'll just pick a tag type; how about `Maybe String`, a `Just` means we've tagged the value
and a `Nothing` means we haven't. `Maybe String` is a monoid since `String` is a Monoid.

```haskell
type Tag = Maybe String

instance Adjunction (Tagged Tag) Forget where

  unit :: a -> Forget (Tagged Tag a)
  unit a = Forget (Tagged Nothing a)
  counit :: Tagged Tag (Forget a) -> a
  counit (Tagged _ (Forget a)) = a

  -- leftAdjunct and rightAdjunct have default implementations in terms of unit & counit
  leftAdjunct :: (Tagged a -> b) -> a -> Forget b
  rightAdjunct :: (a -> Forget b) -> Tagged a -> b
```

There we go! Here we say that Forget is Right Adjoint to Tagged, which roughly means that we lose information when
we move from `Tagged` to `Forget`. `unit` and `counit` correspond to the `inject` and `forget` that we wrote earlier,
they've just got that extra `Forget` floating around. That's okay though, it's isomorphic to `Identity` so anywhere
we see a `Forget a` we can pull it out into just an `a` and vice versa if we need to embed an `a` to get `Forget a`.

We now have access to helpers which allow us to promote and demote functions from one functor into the other; so if we've
got a function which operates over tagged values we can get a function over untagged values, the same goes for turning
functions accepting untagged values into ones taking tagged values. These helpers are `leftAdjunct` and `rightAdjunct`
respectively! We're going to wrap them up in a small layer to perform the `a ≅ Forget a` isomorphism for
us so we can clean up the signatures a little.

```haskell
overUntagged :: (Tagged Tag a -> b) -> a -> b
overUntagged f = getForget . leftAdjunct f 

overTagged :: (a -> b) -> Tagged Tag a -> b
overTagged f = rightAdjunct (Forget . f)
```

To test these out let's write a small function which takes Strings which are Tagged with a String annotation and
appends the tag to the string:

```haskell
applyTag :: Tagged Tag String -> String
applyTag (Tagged Nothing s) = s
applyTag (Tagged (Just tag) s) = tag ++ ": " ++ s

λ> applyTag (Tagged (Just "Book") "Ender's Game")
"Book: Ender's Game"
λ> applyTag (Tagged Nothing "Steve")
"Steve"
```

Using our helpers we can call `applyTag` over untagged strings too, though the results are expectedly boring:

```haskell
λ> overUntagged applyTag "Boring"
"Boring"
```

Now let's see the other half of our adjunction, we can define a function over strings and run it over Tagged strings!

```haskell
upperCase :: String -> String
upperCase = fmap toUpper

λ> upperCase "Steve"
"STEVE"
λ> overTagged upperCase (Tagged (Just "Book") "Ender's Game")
"ENDER'S GAME"
```

Notice that we lose the tag when we do this, that's the price we pay with a lossy Adjunction!
The utility of the construct seems pretty limited here since `fmap` and `extract` would pretty much
give us the same options, but the idea is that Adjunctions represent a structure which we can generalize
over in certain cases. This post was more about understanding adjunctions and Free/Forgetful functors than it
was about programming anyways :)
