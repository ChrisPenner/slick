---
title: "Conway's Game of Life using Representable and Comonads"
author: Chris Penner
date: Aug 8, 2017
tags: [haskell, programming]
description: "Quick ~100 line implementation of Conway's game of life in Haskell"
image: conway.png
---

Just a quick post today, I had some time free this weekend and figured I'd take
a crack at an old classic: "[Conway's Game of
Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life)".

This is an interesting puzzle because it centers around context-based
computations, each cell determines whether it lives or dies in the next
generation based on its nearby neighbours. This is typically considered one of
the trickier things to do in a functional language and solutions often end up
being a bit clunky at best. I think clunkiness usually results when attempting
to port a solution from an imperative language over to a functional language.
To do so you need to figure out some way to iterate over your grid in 2
dimensions at once doing complicated indexing to compute your neighbours and
attempting to store your results somewhere as you go. It definitely can be
done, you can fake loops using folds and traversable, but I feel like there are
better approaches. Allow me to present my take on it.

If you like to load up code and follow along you can find the source
[here](https://github.com/ChrisPenner/conway)!

We'll be using some pretty standard language extensions, and we'll be using
Representable and Comonads, so let's import a few things to get started:

```haskell
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}
module Conway where

import Data.Functor.Compose (Compose(..))
import qualified Data.Vector as V
import Data.Bool (bool)
import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Functor.Identity (Identity(..))
import Control.Arrow ((***))
import Control.Comonad.Representable.Store (Store(..), StoreT(..), store, experiment)
import Control.Comonad (Comonad(..))
```

Conway's game of life runs on a grid, so we'll need to think up some way to
represent that. We'll need to be able to index into that grid and be able to
compute neighbours of a given location, so we can let that guide our
representation.

I've often seen people try to represent grids in Haskell as List Zippers
generalized to higher dimensions, i.e. if we have a structure like
`data Zipper a = Zipper [a] a [a]`, you might try representing a grid as
`Zipper (Zipper a)`.

While this is a totally valid representation, indexing into it and defining
comonad's `extend` becomes prohibitively difficult to reason about. I propose
we try something a little different by extracting the 'index' from the data
structure and holding them together side by side. We'll represent our grid as a
Vector of Vectors just as you'd expect, but then we'll pair that with a set of
`(x, y)` coordinates and set up some indexing logic to take care of our Comonad
instance for us!

If your Category Theory senses are tingling you may recognize this as the
[Store
Comonad](http://hackage.haskell.org/package/comonad-5.0.2/docs/Control-Comonad-Store.html).
`Store` is typically represented as a tuple of `(s -> a, s)`. This means that
you have some index type `s` and you know how to look up an `a` from it. We can
model our grid as `(Vector (Vector a), (Int, Int))` then our `s -> a` is simply
a partially applied Vector lookup! I tried setting this up, but the default
Store comonad does no optimization or memoization over the underling function
so for each progressive step in Conway's game of life it had to compute all
previous steps again! That's clearly pretty inefficient, we can do better!

Enter `Control.Comonad.Representable.Store`! As we just noticed, a Store is
just an indexing function alongside an index, since Representable Functors are
indexable by nature, they make a great companion for the Store comonad. Now
instead of partially applying our index function we can actually just keep the
Representable functor around and do operations over that, so the Store is going
to look something like this: `(Vector (Vector a), (Int, Int))`.

Unfortunately there isn't a Representable instance defined for Vectors (since
they can vary in size), so we'll need to take care of that first. For
simplicity we'll deal with a fixed grid-size of `20x20`, meaning we can enforce
that each vector is of exactly length 20 which lets us write a representable
instance for it!. We'll wrap the Vectors in a `VBounded` newtype to keep things
straight:

```haskell
newtype VBounded a = VBounded (V.Vector a)
  deriving (Eq, Show, Functor, Foldable)

instance Distributive VBounded where
  distribute = distributeRep

gridSize :: Int
gridSize = 20

instance Representable VBounded where
  type Rep VBounded = Int
  index (VBounded v) i = v V.! (i `mod` gridSize)
  tabulate desc = VBounded $ V.generate gridSize desc
```

There's the heavy lifting done! Notice that in the Representable instance for
VBounded we're doing pac-man 'wrap-around' logic by taking the modulus of
indices by grid size before indexing.

Now let's wrap it up in a Store, we're using `store` provided by
`Control.ComonadRepresentable.Store` takes a tabulation function and a starting
index and builds up a representable instace for us. For our starting position
we'll take a list of coordinates which are 'alive'. That means that our
tabulation function can just compute whether the index it's passed is part of
the 'living' list!

```haskell
type Grid a = Store (Compose VBounded VBounded) a
type Coord = (Int, Int)

mkGrid :: [Coord] -> Grid Bool
mkGrid xs = store lookup (0, 0)
  where
    lookup crd = crd `elem` xs
```

Now for the meat and potatoes, we need to compute the successive iterations of
the grid over time. We may want to switch the set of life rules later, so let's
make it generic. We need to know the neighbours of each cell in order to know
how it will change, which means we need to somehow get each cell, find its
neighbours, compute its liveness, then slot that into the grid as the next
iteration. That sounds like a lot of work! If we think about it though,
contextual computations are a comonad's specialty! Our Representable Store is a
comonad, which means it implements
`extend :: (Grid a -> b) -> Grid a -> Grid b`. Each Grid passed to the function
is focused on one of the slots in the grid, and whatever the function returns
will be put into that slot! This makes it pretty easy to write our rule!

```haskell
type Rule = Grid Bool -> Bool

-- Offsets for the neighbouring 8 tiles, avoiding (0, 0) which is the cell itself
neighbourCoords :: [(Int, Int)]
neighbourCoords = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

basicRule :: Rule
basicRule g =
  (alive && numNeighboursAlive `elem` [2, 3]) || (not alive && numNeighboursAlive == 3)
  where
    alive = extract g
    addCoords (x, y) (x', y') = (x + x', y + y')
    neighbours = experiment (\s -> addCoords s <$> neighbourCoords) g
    numNeighboursAlive = length (filter id neighbours)

step :: Rule -> Grid Bool -> Grid Bool
step = extend
```

Two things here, we've defined `step = extend` which we can partially apply
with a rule for our game, turning it into just `Grid Bool -> Grid Bool` which
is perfect for iterating through cycles! The other interesting thing is the use
of `experiment` which is provided by the `ComonadStore` typeclass. Here's the
generalized signature alongside our specialized version:

```haskell
experiment :: (Functor f, ComonadStore s w) => (s -> f s) -> w a -> f a
experiment :: (Coord -> [Coord]) -> Grid a -> [a]
```

Experiment uses a function which turns an index into a functor of indexes, then
runs it on the index in a store and extracts a value for each index using fmap
to replace each index with its value from the store! A bit confusing perhaps, but
it fits our use case perfectly!

Now we need a way to render our board to text!

```haskell
render :: Grid Bool -> String
render (StoreT (Identity (Compose g)) _) = foldMap ((++ "\n") . foldMap (bool "." "#")) g
```

First we're unpacking the underlying `VBounded (VBounded a)`, then we convert
each bool to a representative string, fold those strings into lines, then fold
those lines into a single string by packing newlines in between.

We cleverly defined `mkGrid` earlier to take a list of coords which were alive
to define a board; if we make up some interesting combinators we can make a
little DSL for setting up a starting grid!

```haskell
at :: [Coord] -> Coord -> [Coord]
at xs (x, y) = fmap ((+x) *** (+y)) xs

glider, blinker, beacon :: [Coord]
glider = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
blinker = [(0, 0), (1, 0), (2, 0)]
beacon = [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]

start :: Grid Bool
start = mkGrid $
     glider `at` (0, 0)
  ++ beacon `at` (15, 5)
  ++ blinker `at` (16, 4)
```

That's pretty slick if you ask me!

It's not terribly important, but here's the actual game loop if you're interested:

```haskell
import Conway
import Control.Concurrent

tickTime :: Int
tickTime = 200000

main :: IO ()
main = loop (step basicRule) start

loop :: (Grid Bool -> Grid Bool) -> Grid Bool -> IO ()
loop stepper g = do
  putStr "\ESC[2J" -- Clear terminal screen
  putStrLn (render g)
  threadDelay tickTime
  loop stepper (stepper g)
```

That's about it, hope you found something interesting!
