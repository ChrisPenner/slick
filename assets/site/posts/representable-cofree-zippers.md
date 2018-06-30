---
title: "Zippers using Representable and Cofree"
author: Chris Penner
date: Jul 5, 2017
tags: [haskell, programming]
description: Using Representable and Cofree to build a zipper datatype
image: math.jpeg
---

We're going to take a look at an alternative way to define a Zipper Comonad
over a data type. Typically one would define a Zipper Comonad by defining a new
datatype which represents the Zipper; then implementing `duplicate` and
`extract` for it. `extract` is typically straightforward to write, but I've had
some serious trouble writing `duplicate` for some more complex data-types like
trees.

We're looking at a different way of building a zipper, The advantages of this
method are that we can build it up out of smaller instances piece by piece.
Each piece is a easier to write, and we also gain several utility functions
from the Typeclasses we'll be implementing along the way! It's not terribly
practical, but it's a fun experiment.

You can find this post as a literate haskell file
[here](https://gist.github.com/ChrisPenner/4527dd0d9c60983562e03c28731bb3bd) so
that means you can load it up directly in GHC and play around with it if you
want to follow along! Let's get started.

First we'll need a few language extensions, In case you're wondering; TypeFamilies
is used by the `Representable`.

```haskell
{-# language DeriveFunctor #-}
{-# language TypeFamilies #-}
{-# language InstanceSigs #-}
```

We're going to be writing a Zipper into a list. In case you're unfamiliar, a
zipper is essentially a 'view' into a structure which is focused on a single
element. We'll call our focused view into a list a 'Tape'

```haskell
module Tape where
```

Go ahead and import everything we'll need:

```haskell
-- http://hackage.haskell.org/package/comonad-5/docs/Control-Comonad.html#t:Comonad
import Control.Comonad
-- http://hackage.haskell.org/package/free-4.12.4/docs/Control-Comonad-Cofree.html
import Control.Comonad.Cofree
-- https://hackage.haskell.org/package/distributive-0.5.0.2/docs/Data-Distributive.html#t:Distributive
import Data.Distributive
 -- https://hackage.haskell.org/package/adjunctions-4.3/docs/Data-Functor-Rep.html#t:Representable
import Data.Functor.Rep
-- https://hackage.haskell.org/package/containers-0.5.10.2/docs/Data-Sequence.html
import qualified Data.Sequence as S
-- https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-List-NonEmpty.html
import qualified Data.List.NonEmpty as NE
```

Great! At this point one would typically define their zipper data type, which
for lists would look like: `data Tape a = Tape [a] a [a]`, This represents the
idea of having a single element of the list 'under focus' with other elements
to the left and right.

We're trying something different, we're going to define TWO types. One type
which represents all of the POSSIBLE movements, and one which represents a
CHOICE of a specific movement.

First we'll define the possible movements in our tape using the PRODUCT tape
`TPossible`, we'll have a slot in our structure for both leftward and rightward
movements:

```haskell
data TPossible a = TPossible
  { leftward :: a
  , rightward :: a
  } deriving (Show, Eq, Functor)
```

We're deriving Functor here too, that'll come in handy later.

Next we represent a choice of direction as a SUM type, i.e. we can choose to go
either LEFT or RIGHT at any given focus in the list.

```haskell
data TChoice = L | R
  deriving (Show, Eq)
```

Notice that each piece contains a different piece of the information we need,
a value in `TPossible` knows what's to the left or right, a value in `TChoice` knows
which way to move, but not what's there. This sort of relationship shows us that `TPossible`
is a `Representable Functor`. 

Let's talk about what that means.
A Representable Functor is any functor from which you can
extract elements by giving it an index. That is; it's any functor that you can describe completely
using a function from an index to a value. Given `Index -> a` you can build up an `f a` if you have a relationship
between `Index` and `f`!

In our case we have such a relationship; and if we have a function
`TChoice -> a` we could build up a `TPossible a` by calling the function for
the leftward and rightward slots using `L` and `R` respectively.

But we're getting a bit ahead of ourselves; we'll need to build up a
Distributive Instance first, it's a pre-requisite for the Representable class, and in fact every instance of
Representable is also Distributive; If we like we can actually just implement Representative and use `distributeRep`
from `Data.Functor.Rep` as your implementation of Distributive, but we'll do it the long way here.

Distributive can seem strange if you haven't worked with it before, it's the
dual of Traversable; Traversable can pull out other Applicative Effects from
within its structure, and so Distributive can pull its own structure from any
functor to the outside. You can define an instance by implementing either
`distribute` or `collect`. 

Here're the signatures:

```haskell
distribute :: Functor f => f (g a) -> g (f a)
collect :: Functor f => (a -> g b) -> f a -> g (f b)
```

Let's see a few examples to solidify the idea:

```haskell
distribute :: [Identity a] -> Identity [a]
distribute :: [Bool -> a] -> (Bool -> [a])
distribute :: [TPossible a] -> TPossible [a]
```

The list here could be ANY functor, I just used lists because it's something
most people are familiar with. In many cases sequence and distribute are
interchangeable since a lot of types have reasonableApplicative instances, but
it's important to note that `distribute` pulls a distributive functor OUT from
any wrapping functor while `sequence` from Data.Traversable pushes a
traversable INTO a wrapping Applicative.

A good intuition for determining whether a functor is distributive is to ask
whether values of that functor (f a) always have the same number of elements of
'a'. If they do, and they don't have extra information aside from their
structure, then it's probably distributive. Note that this means that we
actually can't define Distributive for finite-length lists, give it a try if
you don't believe me!

We've got exactly two slots in EVERY `TPossible` so we can implement distribute
by creating an outer `TPossible` where the left slot is the functor containing
all the left values and likewise for the right slot.

```haskell
instance Distributive TPossible where
  distribute :: Functor f => f (TPossible a) -> TPossible (f a)
  distribute fga = TPossible (fmap leftward fga) (fmap rightward fga)
```

Now that's out of the way, let's get back to Representable! Remembering our
previous definition `TPossible` is Representable because it has exactly two
slots, a left and a right which can be indexed by `TChoice`!
We need 3 things for an instance of Representable:

-  A type which represents our index (called Rep)
-  `index` which pulls out the value at a given index.
-  `tabulate` which builds up an object from a function.

```haskell
instance Representable TPossible where
  type Rep TPossible = TChoice

  index :: TPossible a -> TChoice -> a
  index (TPossible l _) L = l
  index (TPossible _ r) R = r

  tabulate :: (TChoice -> a) -> TPossible a
  tabulate describe = TPossible (describe L) (describe R)
```

We're moving along quick! We've got the necessary tools to index into our
`TPossible` structure, which we can use to follow a 'path' through the zipper to
find an element, but currently we only have a way to represent a single choice
of direction at once. We can say we want to move right using 'R', but then
we're stuck! Similarly with `TPossible` we have places to store the value to the
left and right, but can't check the value at the current position! We can solve
the problem by wrapping our `TPossible` in Cofree!

Cofree allows us to promote a functor that we have into a Comonad by ensuring
we always have an element in focus and that we have a way to move around
amongst possible options while still maintaining a focus. It does this by using
an infinitely recursive structure which wraps around a given functor (in our
case `TPossible`). Let's build up a few of these structures by combining
`TPossible` with Cofree! (:&lt;) is the Cofree constructor and has the following
structure: `a :< f (Cofree f a)`.

Lucky for us, if we have a Representable instance for a functor f, we get
Representable of Cofree f for free! We can cheat a little and use our
Representable class to build up the data structure for us by simply providing a
describe function to 'tabulate' which returns the value we want to appear at
any given index. Remember, the index we chose for `TPossible` is `TChoice`. The
index for `Cofree TPossible` is a Sequence of `TChoice`!

Let's build our first actual 'Tape' using tabulate with our Cofree
Representable instance! Here's an infinite number-line going out in both
directions from our focus:

```haskell
relativePosition :: S.Seq TChoice -> Int
relativePosition = sum . fmap valOf
  where
    valOf L = (-1)
    valOf R = 1

numberLine :: Cofree TPossible Int
numberLine = tabulate relativePosition
```

Kinda weird to look at eh? Effectively we're saying that if we move left the
position is 1 less than whatever we were at before, and moving right adds
one to the previous total. You could also write a recursive version of
`describe` which calculates the result by pulling each index off of the
sequence and returns the result! Let's look at another example where we want
a zipper into a finite list!

We'll define a function which 'projects' a list into an infinite Cofree;
we define the behaviour such that moving left 'off the edge' just leaves you at the
leftmost element and similar with the right. I'm going to re-use our previous helper
'relativePosition' here, but this time I'll use it to index into a list! We'll
put some checks in place to ensure we never get an index which is out of bounds,
if we're given an out of bounds index we'll just give the first or last element
respectively; i.e. the zipper will never 'fall off the end'

```haskell
project :: NE.NonEmpty a -> Cofree TPossible a
project l = tabulate describe
  where
    describe = (l NE.!!) . foldl go 0
    maxIndex = length l - 1
    minIndex = 0
    go n L = max minIndex (n - 1)
    go n R = min maxIndex (n + 1)

elems :: NE.NonEmpty String
elems = "one" NE.:| ["two", "three"]
```

Now we can write a sequence of directions to form a path and see where we end up!
Remember, the zipper 'sticks' to the ends if we try and go off!

```haskell
path :: S.Seq TChoice
path = S.fromList [R, R, R, R, L]
```

Now we can `index (project elems) path` to get "two"!

All this talk and we still haven't mentioned Comonad yet! Well lucky us;
the 'free' package describes an instance of Comonad for all `(Functor
f => Cofree f a)`! So our `(Cofree TPossible a)` is a Comonad over a
for free! Remember that a Comonad instance gives us access to `extend`,
`extract` and `duplicate` functions. You can see their types in
[Control.Comonad](https://hackage.haskell.org/package/comonad-5/docs/Control-Co
monad.html#t:Comonad).

We already have a way to extract an element at a given position via 'index', but don't
really have a way to move our zipper WITHOUT extracting; don't fret though, we can
describe this behaviour in terms of our new Comonad instance by using extend!

```haskell
moveTo :: S.Seq TChoice -> Cofree TPossible a -> Cofree TPossible a
moveTo ind = extend (\cfr -> index cfr ind)
```

Great! Extend works by duplicating the Comonad meaning we'll have a 
`(Cofree TPossible (Cofree TPossible a))`, then it fmaps over the duplicated parts with
the given function. The function 'move' will move the element in each slot
of the Cofree over by a given amount, which is the same result as 'scanning' our
Tape over to a given position.

Cool stuff! I hope you've learned a little about Distributive, Representable,
Comonads, Cofree, and zippers! If you have any questions find me on twitter @chrislpenner

Cheers!
