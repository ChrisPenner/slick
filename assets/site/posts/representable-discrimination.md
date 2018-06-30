---
title: "Radix Sort, Trie Trees, and Maps from Representable Functors"
author: Chris Penner
date: Jul 23, 2017
tags: [haskell, programming]
description: Representable Functors can be used to create Map-like data structures or perform Radix-like sorts.
image: containers.jpg
---

I recommend you follow along in ghci and experiment with things as you go;
There's a Literate Haskell version of this post
[here](https://gist.github.com/ChrisPenner/eb6a4efa0d57f39dc5f3c7bb2d31c2d7),
you can load it straight into ghci!

Looking at my recent posts it's clear I've been on a bit of a Representable
kick lately; turns out there's a lot of cool things you can do with it! We'll
be adding 'sorting' to that list of things today. Representable Functors bring
with them an intrinsic notion of sorting; not in the traditional 'ordered'
sense, but rather a sense of 'structural' sorting. Since every 'slot' in a
Representable Functor `r` can be uniquely identified by some `Rep r` we can
talk about sorting items into some named slot in `r`. If we like we can also
define `Ord (Rep r)` to get a total ordering over the slots, but it's not
required.

I'll preface this post by saying I'm more interested in exploring the
structural 'form' of representable sorting than the performance of the
functions we'll define, in fact the performance of some of them as they're
written here is going to be quite poor as I'm sacrificing speed to gain
simplicity for the sake of pedagogy. The intent is to observe the system from a
high-level to see some interesting new patterns and shapes we gain from using
Representable to do our sorting. Most of the structures we build could quite
easily be optimized to perform reasonably if one was so inclined.

## Building up Sorting over Representable

I'll step through my thought process on this one:

We've got a Representable Functor `r`; If we have a `Rep r` for some `a` we
know which slot to put it into in an `r a`. We can get a `Rep r` for every `a`
by using a function `a -> Rep r`. Now we want to embed the `a` into an `r a`
using the `Rep r`, the tool we have for this is `tabulate`, in order to know
which index is which and put it into the right slot we'll need to require
`Eq (Rep r)`. We know which slot our one element goes to, but we need something
to put into all the other slots. If `a` were a Monoid we could use `mempty` for
the other slots; then if we map that function over every element in an input
list we could build something like this:

```haskell
(Representable r, Monoid a, Eq (Rep r)) => (a -> Rep r) -> [a] -> [r a]
```

We want a single `r a` as a result rather than a list, so we need to collapse
`[r a]`. We could use `mconcat` if `r a` was a Monoid! We can actually write a
monoid instance for any representable if the inner `a` type also has a Monoid
instance, later we'll define a custom newtype wrapper with that instance! This
gives:

```haskell
(Representable r, Monoid a, Eq (Rep r)) => (a -> Rep r) -> [a] -> r a
```

We can generalize the list to any foldable by just calling
`Data.Foldable.toList` on it, and we get:

```haskell
(Representable r, Monoid a, Foldable f, Eq (Rep r)) => (a -> Rep r) -> f a -> r a
```

Nifty! But this requires that every `a` type we want to work is also a Monoid,
that's going to seriously limit the usecases for this. We can increase the
utility by allowing the caller to specifying a way to build a Monoid from an
`a`:

```haskell
(Representable r, Monoid m, Foldable f, Eq (Rep r)) => (a -> Rep r) -> (a -> m) -> f a -> r m
```

And that's our final fully generalized type signature! 

We're going to need a bunch of imports before we start implementing things,
prepare yourself:


```haskell
{-# language DeriveFunctor #-}
{-# language TypeFamilies #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module RepSort where

import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..), Co(..), distributeRep)
import Data.Monoid (Sum(..))
import qualified Data.Stream.Infinite as S (Stream, iterate)
import Control.Comonad.Cofree (Cofree)
import qualified Data.Sequence as Seq (Seq, fromList)
```

So here's my implementation for `repSort`:

```haskell
-- Firstly, the signature we came up with:
repSort :: (Representable r, Monoid m, Foldable f, Eq (Rep r)) => (a -> Rep r) -> (a -> m) -> f a -> r m
repSort indOf toM = unMRep . foldMap (MRep . tabulate . desc)
  where
    -- desc takes an 'a' from the foldable and returns a descriptor function which can be passed to 'tabulate',
    -- The descriptor just returns mempty unless we're on the slot where the 'a's result is supposed to end up.
    desc a i
      | i == indOf a = toM a
      | otherwise = mempty


-- Here's our newtype with a Monoid over Representables
newtype MRep r a = MRep {unMRep ::r a}
  deriving (Show, Eq, Functor)

instance (Monoid a, Representable r) => Monoid (MRep r a) where
  -- The empty Representable is filled with mempty
  mempty = MRep $ tabulate (const mempty)
  -- We can just tabulate a new representable where the value is the `mappend` of
  -- the other two representables. BTW (index a `mappend` index b) depends on
  -- the monoid instance for functions, so go check that out if you haven't seen it!
  (MRep a) `mappend` (MRep b) = MRep . tabulate $ index a `mappend` index b
```

## Using `repSort`

Great! Let's see some examples so we can get a handle on what this does! First I'll
set up a super simple but useful Representable for Pair:

```haskell
data Pair a = Pair a a
  deriving (Show, Eq, Functor)

-- This instance is required, but we can just lean on our Representable instance
-- `Data.Functor.Rep` provides all sorts of these helpers.
instance Distributive Pair where
  distribute = distributeRep

instance Representable Pair where
  -- Bool is a great index for this!
  type Rep Pair = Bool
  index (Pair a _) True = a
  index (Pair _ b) False = b

  tabulate desc = Pair (desc True) (desc False)
```

So since Pair is indexed by a `Bool` the `a -> Rep Pair` is actually just a
predicate `a -> Bool`! Let's try sorting out some odd and even integers!

Remember that `repSort` needs a function from `a -> Rep r`, in this case
`Rep r ~ Bool` (`~` means 'is equal to' when we're talking about types), so we
can use `odd` to split the odd and even integers up! Next it needs a function
which transforms an `a` into a monoid! The simplest one of these is `(:[])`
which just puts the element into a list! Let's see what we get!


```haskell
sortedInts :: Pair [Int]
sortedInts = repSort odd (:[]) [1..10]

λ> sortedInts
Pair [1,3,5,7,9] [2,4,6,8,10]
```

We used lists in the last example, but remember that the function is
generalized over that parameter so we can actually choose any Monoid we like!
Let's say we wanted the sums of all odd and even ints respectively between 1
and 10:

```haskell
oddEvenSums :: Pair (Sum Int)
oddEvenSums = repSort odd Sum [1..10]

λ> oddEvenSums
Pair (Sum {getSum = 25}) (Sum {getSum = 30})
```

Choosing our own monoid and index function gives us a lot of flexibility and power!

This pattern generalizes to any Representable you can think of, and most
Representables which have an interesting `Rep r` will have some sort of cool
structure or use-case! Think about some other Representables and see what you can
come up with!

## Indexing by Integers using Stream

Let's try another Functor and see what happens, here we'll go with an infinite
`Stream` from
[Data.Stream.Infinite](http://hackage.haskell.org/package/streams-3.3/docs/Data-Stream-Infinite.html)
in the [streams package](http://hackage.haskell.org/package/streams), whose
representation is `Int`, that is; `Rep Stream ~ Int`.

With a representation type of `Int` we could do all sorts of things! Note here
how the Functor (`Stream`) is infinite, but the representable is actually
bounded by the size of Int. This is fine as long as we don't try fold the
result or get every value out of it in some way, we'll primarily be using the
`index` function from `Representable` so it should work out okay. 

The streams are infinite, but Haskell's inherent laziness helps us out in
handling this, not only can we represent infinite things without a problem,
Haskell won't actually calculate the values stored in any slots where we don't
look, and since the whole thing is a data structure any computations that do
occur are automatically memoized! This also means that you don't pay the cost
for any value transformation or monoidal append unless you actually look inside
the bucket. Only the initial `a -> Rep r` must be computed for each element.

Let's sort some stuff! See if you can figure this one out:


```haskell
byLength :: S.Stream [String]
byLength = repSort length (:[]) ["javascript", "purescript", "haskell", "python"]

λ> index byLength 10
["javascript","purescript"]
λ> index byLength 7
["haskell"]
λ> index byLength 3
[]
```

We didn't have to change our implementation of repSort at all! `index` knows how
to find values in a `Stream` from an `Int` and all of that complexity is taken care
of for us in the instance of `Representable`.

The fact that `Int` is the `Rep` for Stream provides us with a few quick wins,
any Enumerable type can be injected into `Int` via `fromEnum` from the Prelude:
`fromEnum: (Enum e) => e -> Int`. This means we can turn any Enumerable type
into an index into `Stream` without much trouble and we gain a whole new set of
possibilities:

```haskell
byFirstChar :: S.Stream [String]
-- We get the Int value from the first char of a string and use that as the index!
byFirstChar = repSort (fromEnum . head) (:[]) ["cats", "antelope", "crabs", "aardvarks"]

λ> index byFirstChar . fromEnum $ 'c'
["cats","crabs"]
λ> index byFirstChar . fromEnum $ 'a'
["antelope","aardvarks"]
λ> index byFirstChar . fromEnum $ 'z'
[]
```

So that's all pretty cool, but working with a single infinite stream gets
unwieldy quickly when we start dealing with indexes in the thousands! `Stream`
is effectively a linked list, so we need to step along the nodes until we reach
our index every time we look something up! Maybe we can do better on that
somehow...

Straying a bit from sorting into the idea of data storage let's say we wanted
to store values in a structure where they're keyed by a `String`. The first
step would be to find a Representable whose index type could be a `String`.
Hrmm, our `Stream` representation can index by `Char`, which is close; what if
we nested further representables and used a 'path' to the value as the index?
Something like `Stream (Stream (Stream ...))`. This looks like an infinite tree
of trees at this point; but has the issue that we never actually make it to any
`a`s! Whenever I think of tagging a tree-like structure with values I go
straight to Cofree, let's see how it can help.

## Diving into Tries using Cofree

One way you could think of Cofree is as a Tree where every branch and node has
an annotation `a` and the branching structure is determined by some Functor!
For example, a simple Rose tree is isomorphic to `Cofree [] a`,
`Cofree Maybe a` makes a degenerate tree with only single branches, etc.

We want a tree where the branching structure is indexed by a `String`, so let's
give `Cofree Stream a` a try! Effectively this creates an infinite number of
branches at every level of the tree, but in practice we'll only actually follow
paths which are represented by some string that we're indexing with, the rest
of the structure will be never be evaluated!

As it turns out, we made a good choice! `Cofree r a` is Representable whenever
`r` is Representable, but which type indexes into it? The Representable
instance for Cofree (defined in
[Control.Comonad.Cofree](http://hackage.haskell.org/package/free-4.12.4/docs/Control-Comonad-Cofree.html)
from the [free](http://hackage.haskell.org/package/free) package) It relies on
the underlying Representable Index, but since the tree could have multiple
layers it needs a sequence of those indexes! That's why the index type for
`Cofree` of a Representable is `Seq (Rep r)`, when we index into a `Cofree` we
follow one index at a time from the Sequence of indexes until we reach the end,
then we return the value stored at that position in the tree! Under the hood
the `Rep` for our structure is going to be specialized to `Seq Int`, but we can
easily write `mkInd :: String -> Seq Int` to index by Strings!

```haskell
mkInd :: String -> Seq.Seq Int
mkInd = Seq.fromList . fmap fromEnum
```

Great! Now we can 'sort' values by strings and index into a tree structure
(pseudo) performantly! If this whole sort of structure is looking a bit
familiar you've probably seen it by the name of a ["Trie
Tree"](https://en.wikipedia.org/wiki/Trie), a datastructure often used in
[Radix sorts](https://en.wikipedia.org/wiki/Radix_sort) and search problems.
Its advantage is that it gives `O(m)` lookups where `m` is the length of the
key string, in our case it will be slightly slower than the traditional method
since we don't have `O(1)` random memory access, and have to move through each
child tree to the appropriate place before diving deeper, but you could fix
that pretty easily by using a proper `Vector` as the underlying representation
rather than `Stream`. I'll leave that one as an exercise for the reader.

## Building Maps from Tries

That's a whole lot of explaining without any practical examples, so I bet you're
itching to try out our new Trie-based Sorter/Map! With a few helpers we can build 
something quickly!

```haskell
-- I'm going to specialize the signature to `Cofree Stream` to make it a bit more
-- readable, but you could re-generalize this idea if you wanted to.
trieSort :: (Monoid m, Foldable f) => (a -> String) -> (a -> m) -> f a -> Cofree S.Stream m
trieSort getInd = repSort (mkInd . getInd)

-- Build a map out of some key and a Monoidal value!
trieMap :: Monoid m => [(String, m)] -> Cofree S.Stream m
trieMap = trieSort fst snd

get :: Cofree S.Stream a -> String -> a
get r ind = index r (mkInd ind)

bankAccounts :: Cofree S.Stream (Sum Int)
bankAccounts = trieMap [("Bob", Sum 37), ("Sally", 5)]

λ> get bankAccounts "Bob"
Sum {getSum = 37}
λ> get bankAccounts "Sally"
Sum {getSum = 5}
-- Empty keys are mempty
λ> get bankAccounts "Edward"
Sum {getSum = 0}

withdrawals :: Cofree S.Stream (Sum Int)
withdrawals = trieMap [("Bob", Sum (-10))]

-- And of course we can still make use of our MRep helper to combine maps!
λ> get (unMRep $ MRep bankAccounts `mappend` MRep withdrawals ) "Bob"
Sum {getSum = 27}
```

There are TONS of other possibilities here, we can swap out the underlying
Representable to get new behaviour and performance, we can use
`Data.Functor.Compose` to nest multiple `Representable`s to build new and
interesting structures! We can sort, store, lookup and search using `repSort`!

Let me know which interesting ideas you come up with! Either leave a comment
here or find me on Twitter [\@chrislpenner](https://twitter.com/chrislpenner).
