---
title: "Flux Monoid: Efficient change counting"
author: Chris Penner
date: Jun 20, 2018
tags: [programming, haskell]
description: Description and implementation of a Monoid to effeciently count element equality changes across data structures
image: flux-monoid/flux.jpg
---

This is going to be a post about monoids, finger trees, efficiently splitting up text, and the unending change
of the world that surrounds us; but mostly the monoid bit.

# Monoids

If you're entirely unfamiliar with the concept of monoids, or just need a
refresher, it would be a good idea to get a solid grounding there first;
[here's a good place to
start](https://www.schoolofhaskell.com/user/mgsloan/monoids-tour).

Monoids are incredibly useful; and the more I learn about Category Theory the
more applications I find for monoidal structures. Once you start to think in
monoids you start to realize how many things you once thought were unique and
interesting problems are actually just a monoid and a fold away from some other
well-solved problem. We're going to start off by introducing a new tool
(i.e. data structure) which employs monoids to do amazing things! Enter [Finger
Trees](https://en.wikipedia.org/wiki/Finger_tree)! Finger Trees are an
adaptable purely functional data structure; they're actually an extremely general structure which makes it a bit
difficult to explain without a concrete use case. This is because they utilize a Monoid in the foundation of the data
structure, and the Monoid you choose can drastically affect how the structure behaves. Here's a glance at the sort of
things you could do by choosing different Monoids:

- Random access/sequence slicing using [`Sum`](https://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Monoid.html#t:Sum): see [Data.Sequence](https://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Sequence.html)
-  Heap using [`Max/Min`](https://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Semigroup.html#t:Max): see [Data.PriorityQueue.FingerTree](https://hackage.haskell.org/package/fingertree-0.1.4.1/docs/Data-PriorityQueue-FingerTree.html)
- Ordered Sequence slicing using [Last](https://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Monoid.html#t:Last): see the section on [Ordered Sequences](http://www.staff.city.ac.uk/~ross/papers/FingerTree.pdf)
- Interval Searching using a custom interval expansion Monoid: see [Data.IntervalMap.FingerTree](https://hackage.haskell.org/package/fingertree-0.1.4.1/docs/Data-IntervalMap-FingerTree.html)
- Text slicing and dicing using a product of `Sum`s: see [Yi.Rope](https://hackage.haskell.org/package/yi-rope)
- Performant merge sort using a custom merge monoid: blog post coming eventually!
- Many more! Just use your imagination!

How does it all work? Let's take a quick look at doing a simple random-access
\*air-quotes\* Array \*/air-quotes\* using a Finger Tree so we can see how it all works.

## Random Access Array using a Finger Trees

Let's implement a simple random access list using a Finger Tree! After a quick
glance through the [Data.FingerTree
Docs](https://hackage.haskell.org/package/fingertree-0.1.4.1/docs/Data-FingerTree.html)
it's a bit tough to tell where we might start! The workhorse of the Finger Tree library is
the `split` function:

```haskell
split :: Measured v a => (v -> Bool) -> FingerTree v a -> (FingerTree v a, FingerTree v a)
```

Yikes, let's break this down:

-   `Measured v a`: Measured is a simple typeclass which given an `a` can
    convert it into some monoid `v`
-   `(v -> Bool)`: This is our search predicate, `split` will use it to split a
    sequence into two smaller subsequences: The longest
    subsequence such that running the predicate on the measure of this
    subsequence `False`, and the everything that's left-over.
- `FingerTree v a`: This is the tree we want to split, with a monoidal measure `v` and elements of type `a`.
- `(FingerTree v a, FingerTree v a)`: The two (possibly empty) subsequences split based on the predicate

That's all great, but how can we actually use it to solve our problem? What
does splitting up a sequence actually have to do with indexing into a list?
Finger Trees get their performance characterics by searching through subtrees
using a strategy very similar to a binary search, they run the predicate on
cached "measures" of subtrees recursively honing in on the **inflection point**
where the predicate flips from `False` to `True`. So what we need to do is find
some pairing of a monoid and a predicate on that monoid which finds the place
in the sequence we're looking for. Getting the first or last element of a
Finger Tree is a simple `O(1)` operation, so if we can split the list either
directly *before* or directly *after* the index we're looking for, then we're
pretty much done! 

Building a predicate for this is pretty simple, we just need to be able to
determine whether the index we're looking for is within some prefix of our
total sequence, which phrased simply is just: `length sequence > index`; we can
use this predicate to recursively hone in on the point where adding a single
element inflects the predicate from false to true, and we've found our index!
We do however need this predicate to run on a monoid; so we need to represent
the length of our sequence as a monoid, the combination of the monoidal measure
of two sequences must also match the measure of the combination of the
sequences themselves! Luckily for us the length of the combination of two lists
is just the sum of the lengths! This gives us the hint that we can use the
[`Sum`
Monoid](https://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Monoid.html#t:Sum) as our measure!

A great first step is to find a
monoid which matches our problem! 

when mappended gives us some information about the problem we're
trying to solve. For random access we're really only concerned with the
**count** of objects in the sequence. When choosing your monoid it's helpful to
think about how the monoidal **sum** of **sequences** interact, not just
individual elements. What if we could keep track of the number of elements in
any given subsequence? The size of the combination of any subsequences would
simply be the sum of the counts of the subsequences.

`split`, `dropUntil`, and
`search` all kind of seem like they might help though!

```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.FingerTree
import Data.Monoid
newtype Size a = Size
  { getSize :: a
  } deriving (Show, Eq)

instance Measured (Sum Int) (Size a) where
  measure _ = Sum 1

alphabet :: FingerTree (Sum Int) (Size Char)
alphabet = fromList (fmap Size "abcdefghijklmnopqrstuvwxyz")

atIndex :: Int -> FingerTree (Sum Int) (Size Char) -> Maybe Char
atIndex n t =
  case viewl . snd $ split (> Sum n) t of
    Size c :< _ -> Just c
    _ -> Nothing
```

Some time ago when I was
writing my [text editor](https://github.com/ChrisPenner/rasa) I spent a lot of
time working with . Since most people probably haven't needed to do much
reading on Finger Trees here's a quick summary:

Finger Trees use the properties of a Monoid to provide efficient monoidal
`summing` or `splitting` based on a monoid over an ordered collection which is
undergoing mutations. When working with Finger Trees you provide a predicate
over the sum for the underlying monoid of the Finger Tree. In my case the
monoid in questions was text; where the text was being `summed` via
concatenation and was often `split` into lines based on a predicate over the
`Sum` monoid of the line-count. This is a common approach to working with text
(See the [Rope](https://en.wikipedia.org/wiki/Rope_(data_structure)) data
structure, and the haskell implementation of a Rope using Finger Trees
[here](https://hackage.haskell.org/package/yi-rope)). Since Finger Trees use a
monoid as the interface you get a free implementation for any monoids you dream
up!

Let's take a look at the `split` operation for a Finger Tree; this method takes a predicate over
the "Measure" (i.e. Monoid) of the tree and descends through the tree using cached monoidal sums of subtrees to do something
akin to a binary search. Because we cache the sum of our monoids it's important that the predicate we provide is
'monotonically increasing' over the monoid of the tree; which is just a fancy way of saying it should only have a
**single inflection point**; otherwise it would be possible for inflection points to be hidden within a subtree by
accident.

Elements of the tree will be `mappend`ed together 
for an inflection point in the monoid. If `Sum` was our monoid we could for instance
find the point in a sequence where the sum goes from below 10 to 10 or higher. Or for our
tree over text we could find the spot in the text where the number of newline characters goes
from 15 to 16 (i.e. find the beginning of the 16th line of text). 

Here's a [deeper dive into Finger
Trees](https://abhiroop.github.io/Finger-Trees/) if you're up for some reading.

A limitation of the functions which slice and dice finger trees is that the splitting predicate you give them is only
guaranteed to work if there's exactly **ONE** inflection point within the structure; that is to say that the function
on which we base our predicate must be [monotonic](https://en.wikipedia.org/wiki/Monotonic_function) or else there may
be more than one valid place to split the tree! This requirement doesn't bother us much when our predicate is concerned
about individual values; for instance when each value we consume adds to our sum which is the case with the `Sum`
monoid or when we're summing up newlines; but it becomes problematic when we need to consider sub-sequences of values.

At first this seems at odds with the data-structure; but the data-structure is monoid-agnostic, so as long as we can
find some monoid to do the dirty work then there's no reason we can't make this work!

Here's a description of a simple problem we'd like to solve; given some text we'd like to split it into
subsequences of UPPER_CASE and lower_case characters like so: `"abcDEFgHIjk" -> ["abc", "DEF", "g", "HI", "jk"]`. 
One way we might think to do this is to embed the sequence in a finger tree an use the splitting operation repeatedly
over a simple predicate monoid:

In order to embed any value into the tree we need to specify how we can 'measure' that value into some Monoid.

```haskell
class Monoid v => Measured v a | a -> v where
    measure :: a -> v
```

Here's how we could attempt implementing a measure based on the casing of the character.

```haskell
import Data.Char
import Data.Monoid
-- We'll wrap char for our Measured instance to avoid any issues with functional dependencies
newtype CaseChar = CaseChar Char
instance Show CaseChar where
  show (CaseChar c) = [c]

instance Measured Any (CaseChar Char) where
    measure (CaseChar c) = Any (isUpper c)

```

Let's take a moment to think about how this monoid will behave as we sum across our input;
assuming we scan from left to right we would get the following monoid sum:

```
abCDeF 
fftttt
```

Cool! So in theory we can run `split` over this finger tree and split the tree into the leading lower-case chars and the
rest.

```haskell
import Data.FingerTree

tree :: FingerTree Any CaseChar
tree = fromList (CaseChar <$> "abCDeF")
```

Now let's try splitting it up!

```haskell
> split getAny tree
(fromList [a,b],fromList [C,D,e,F])
```

Cool; so we're properly splitting off the first section; what happens if we want to keep splitting by case? Let's try
splitting the leftovers again:

```haskell
> split getAny (fromList . fmap CaseChar $ "CDeF")
(fromList [],fromList [C,D,e,F])
```

Hrmm, not quite what we wanted. Since our predicate is based on each individual value and not its **relationship** to
its neighbours we actually have to flip the predicate each time we run `split`! This would work, but it's a bit of a
pain and IMHO ruins the elegance of using a monoid for this.

Don't fear though; Flux monoid to the rescue!

The Flux monoid is unique among most monoids in that it has a **memory** of previous `mappend` operations but still
follows the monoid laws! This means that using the Flux monoid we can construct a predicate based on the
**relationships** of elements in a sequence; allowing us to construct a single predicate which will always yield
another chunk of our sequence! 

Let's define a little helper to run `split` repeatedly for as long as we get new results:

```haskell
splits :: Measured m v => (m -> Bool) -> FingerTree m v -> [FingerTree m v]
splits p = unfoldr expand
  where
    expand t =
      case split p t of
        (FT.null -> True, _) -> Nothing
        (a, b) -> Just (a, b)
```

Here's how we'd use it to split a tree based on the **case** of the characters:

```haskell
> splits ((>= 1) . getFlux ) tree
[fromList [a,b],fromList [C,D],fromList [e],fromList [F]]
```

That's all there is to it!




All of the above is background for why we might ever want the monoid we're
about to explore. The "Flux" monoid (as I've deemed it) allows us to keep track of the times a value has CHANGED 
(in terms of its `Eq` instance) across a sequence. This allows us to 



```haskell
-- | 'Flux' is a monoid which counts the number of times an element changes
-- values (according to its Eq instance)
-- This is useful for gaining associativity (and its associated performance improvements)
-- for tasks where you'd otherwise use `group` or `groupBy`
data Flux a = Flux
  -- We keep track of the last value we saw on the left and right sides of the accumulated
  -- sequence; `Nothing` is used in the identity case meaning no elements have yet
  -- been encountered
  { sides :: Maybe (a, a)
  -- We have a counter which increments each time we mappend another Flux who's
  -- left doesn't match our right or vice versa depending on which side it is mappended onto.
  , getFlux :: Int
  } deriving (Show, Eq)


-- | Embed a single value into a Flux;
-- number of changes starts at 0.
flux :: a -> Flux a
flux a = Flux (Just (a, a)) 0

instance (Eq a) => Monoid (Flux a) where
  mempty = Flux Nothing 0
  Flux Nothing _ `mappend` f = f
  f `mappend` Flux Nothing _ = f
  Flux (Just (l, r)) n `mappend` Flux (Just (l', r')) n'
    | r == l' = Flux (Just (l, r')) (n + n')
    | otherwise = Flux (Just (l, r')) (n + n' + 1)

-- Now that it's set up, we can try it out!

-- > getFlux $ foldMap flux ["a", "b", "b", "a"]
-- 2
-- > getFlux $ foldMap flux ["a", "b", "b", "a", "c", "c", "c"]
-- 3

-- I'll be using this to implement a layer on top of the FingerTree used for
-- the Rope data-structure to allow efficient groupings of data by annotations
-- according to some predicate. FingerTrees require a monoidal measure to efficiently
-- segment and look up data, Flux provides this Monoid for the `group` operation.
```
