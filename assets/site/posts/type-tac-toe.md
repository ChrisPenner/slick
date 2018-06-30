---
title: "Type Tac Toe: Advanced Type Safety"
author: Chris Penner
date: Aug 25, 2017
tags: [haskell, programming]
description: Upgrading your type programming game
image: tictactoe.png
---

Today we'll be looking at type programming in Haskell. Programming in type-land
allows us to teach the compiler a few new tricks and to verify additional
constraints at compile-time rather than run-time. The canonical example is that
you can encode the length of a list as a type so that you can verify that
appending an element to a list of `n` elements yields a list of `n + 1`
elements. If you haven't read about or experimented with an example like that
before I'd say to check out [Matt Parson's post on type
programming](http://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html)
first. We're going to go a step further and we'll actually encode the rules of
a game of Tic Tac Toe into types so that we can statically guarantee that
nobody cheats! If you're into spoilers you can see the finished code at the 
[git repo here](https://github.com/ChrisPenner/Type-Tac-Toe).

Type programming is a newly popularized idea, so the tools for it are still a
bit rough (in Haskell at least), check out [Idris](https://www.idris-lang.org/)
if you'd like to see something a bit more polished.

There are some libraries popping up in the Haskell ecosystem which are making the ideas
presented here easier to work with, most notably the [singletons](http://hackage.haskell.org/package/singletons)
library which can generate a lot of the type-level primitives we write here. Check it out if you like, 
but I find it's a bit confusing for people new to this stuff, so I'll be spelling most things out in long-hand.

Let's get moving!

Here's a representation of the de-facto 3x3 Tic Tac Toe board:

```haskell
-- | Either X, O, or Nothing
data PieceT = X | O | N
  deriving (Show, Eq)

data Trip a = Trip a a a
  deriving (Show, Eq, Functor)

newtype Board a = Board (Trip (Trip a))
  deriving (Show, Eq, Functor)

newBoard :: Board PieceT
newBoard = Board $ Trip (Trip N N N)
                        (Trip N N N)
                        (Trip N N N)
```

Note that we'll need  the `{-# language DeriveFunctor #-}` pragma for this.

We'll need a way to refer to individual squares in the grid so our player can
say where they'd like to move. Let's just use simple `(x, y)` coordinates.
We'll use a custom datatype rather than `Int` so that we know the coordinates
are in bounds.

```haskell
data CoordT = A | B | C
  deriving (Show, Eq)
```

Here's a quick function which lets us change a slot inside a Triple:

```haskell
-- | Utility function to alter a value inside a triple
-- Can set values using `const x`
overTrip :: CoordT -> (a -> a) -> Trip a -> Trip a
overTrip A f (Trip a b c) = Trip (f a) b c
overTrip B f (Trip a b c) = Trip a (f b) c
overTrip C f (Trip a b c) = Trip a b (f c)
```

And that gives us everything we need to place pieces on the board:

```haskell
play :: PieceT -> (CoordT, CoordT) -> Board PieceT -> Board PieceT
play p (x, y) (Board b) = Board $ overTrip y (overTrip x (const p)) b
```

Looking good! But wait, there's really no validation going on here! Players
could play on the same square over and over again! Or maybe player `X` just
keeps on playing without giving `O` a turn! We could do error handling at
runtime inside the function, but that would mean throwing runtime exceptions
(Yikes!), running things inside an error monad, or returning an Either. But
those are all boring and involve runtime checks so lets see how types can help
do this work at compile-time!

## Alternating Turns

To start simple and see if there's a way we could make `X` and `O` alternate
turns! In order to do that we're going to need types which represent `X` and
`O`!

Here's a first attempt:

```haskell
data X
data O
data N
```

Now we'd have types for each, but we get a conflict! We have duplicate
definitions of `X`, `O` and `N` because of `PieceT`! Let's introduce our next
GHC extension: `{-# language DataKinds #-}`! DataKinds takes a bit of fiddling
with to understand, don't worry if you have a hard time understanding where the
boundaries are. I still have to shake my head and think it through most of the
time.

DataKinds let's you use constructors from any datatypes (defined with `data` or
`newtype`) as types! To reference the 'type' version of a data constructor you
prefix it with an apostrophe. Since we already defined `PieceT`, by enabling
DataKinds we now have `'X`, `'O`, `'N` in scope at the type level! Technically you
can leave off the `'` if it's clear to the compiler that you're referring to a
type, but I like to be explicit about it for readability.

There's one more bonus that DataKinds gives us, it creates a new `Kind` for each
family of constructors. Kinds are kind of like types for types, most Haskell types
are of Kind `*`, and higher order kinds are `* -> *`, you can check it in ghci:

```haskell
λ> :k Int
Int :: *
λ> :k Maybe
Maybe :: * -> *
λ> :k Maybe Int
Maybe Int :: *
λ> :k 'X
'X :: PieceT
```

That last one is interesting! GHC notices that `'X` isn't quite like other
types, but that it was defined as part of the PieceT data group. This means
that when we're writing functions on types we can actually specify what Kind
of types we want to allow.

The first and easiest thing we could require of our game is that `X` and `O`
always alternate turns. In order to that we'll need to store who's turn it is
as part of the type of our board. Let's edit our `Board` type to have an
additional parameter called `t` for `turn`, we don't actually have to have the
type in our data-structure though, the compiler will do the check at compile-time so
we won't need to store this info at the value level. A type which is used only
on the left side of a data definition is called a "Phantom type". They're
useful for specifying type constraints.

We'll also edit the signature of `newBoard` to show that `X` goes first; we don't
need to change the definition at all though!

```haskell
newtype Board t a = Board (Trip (Trip a))
  deriving (Show, Eq, Functor)

-- | New empty board
newBoard :: Board 'X PieceT
newBoard = -- Unchanged
```

When we do this we'll get a compiler error that GHC was expecting a type, but we
gave it something of Kind `PieceT`. GHC usually expects basic types of kind `*`,
so if we do anything fancy we need to let it know what we're thinking. In this case
we can add a type annotation to the `Board` type:

```haskell
-- Add this new pragma at the top:
{-# language KindSignatures #-}

newtype Board (t :: PieceT) a = Board (Trip (Trip a))
  deriving (Show, Eq, Functor)
```

The KindSignatures pragma lets say what 'kind' we want all our types to be. This
makes GHC happy, and helps us too by allowing us to specify that the 't' parameter
should always be one of our pieces rather than some arbitrary type.

Unfortunately, changing the type of `Board` has broken our `play` function. We
need to put something in as a 'turn' parameter there too. For now it's easiest
to split it up into a `playX` and `playY` function which can specify their
types more concretely.

```haskell
playX :: (CoordT, CoordT) -> Board 'X PieceT -> Board 'O PieceT
playX (x, y) (Board b) = Board $ overTrip y (overTrip x (const X)) b

playO :: (CoordT, CoordT) -> Board 'O PieceT -> Board 'X PieceT
playO (x, y) (Board b) = Board $ overTrip y (overTrip x (const O)) b
```

Don't worry about the duplication, we'll clean that up later. Now you can only
`playX` on a board when it's X's turn! Huzzah! If you try it the wrong way around
GHC will complain:

```haskell
λ> playO (A, B) newBoard
error:
    • Couldn't match type ‘'X’ with ‘'O’
      Expected type: Board 'O PieceT
        Actual type: Board 'X PieceT
    • In the second argument of ‘playO’, namely ‘newBoard’
      In the expression: playO (A, B) newBoard
      In an equation for ‘it’: it = playO (A, B) Prog.newBoard
```

Pretty cool!

## Preventing Replays

Now the real fun starts! Let's see if we can ensure that people don't play
on a space that's already been played!

A simple `X` or `O` in our type isn't going to cut it anymore, let's bulk up
our representation of the board state. Let's keep track of each place someone
has played! We can do this by writing a type level list of coordinates and piece
types:

```haskell
-- | Keep a list of each Piece played and its location
data BoardRep = Empty
              | Cons CoordT CoordT PieceT BoardRep
```

Remember that we're using DataKinds, so now `BoardRep` is a kind and `Empty` is
a type and so is `Cons` when it's applied to two Coordinates, a Piece type and
another `BoardRep`. It'll keep recursing until we hit an `'Empty`.

Now that we have a board representation, let's replace the `t` in our datatype
with the type level representation of the board:

```haskell
newtype Board (b :: BoardRep) a = Board (Trip (Trip a))
  deriving (Show, Eq, Functor)

-- New boards are 'Empty now
newBoard :: Board 'Empty PieceT
```

Now every time we play a piece we'll also represent the change at the type level,
in order to do that we need to be able to get the "type" of the coordinates
of each move. This is a bit tricky, since the coordinate values themselves are all
of the same type `CoordT` and *NOTHING* is a member of the *types* A, B, or C.

This is where we start to introduce some _hacks_ to get things to work. Say hello
to GADTs!

```haskell
-- New pragma for the top
{-# language GADTs #-}

-- | A proxy type which represents a coordinate
data Coord (a :: CoordT) where
  A' :: Coord 'A
  B' :: Coord 'B
  C' :: Coord 'C

-- | Get the coord's actual value from a wrapper type
coordVal :: Coord a -> CoordT
coordVal A' = A
coordVal B' = B
coordVal C' = C
```

This is going to look weird and strangely verbose to most of you; it's
unfortunate that we need to do things this way, maybe someday we'll find a
better way. You can also look into using the `Proxy` type from `Data.Proxy`,
but it suffers similar verbosity issues.

Let me explain how this works, we've written a new type `Coord` which has a
constructor for each of our Coordinate values, but each constructur also sets
the phantom type parameter of the `Coord` to the appropriate type-level version
of the coordinate. We've also written a function `coordVal` which translates from
our wrapper type into the matching `CoordT` value.

Bleh, a little ugly, but now we can write some well-typed `play` functions:

```haskell
-- View patterns help us clean up our definitions a lot:
{-# language ViewPatterns #-}

playX :: (Coord x, Coord y) -> Board b PieceT -> Board ('Cons x y 'X b) PieceT
playX (coordVal -> x, coordVal -> y) (Board b) 
        = Board $ overTrip y (overTrip x (const X)) b

playO :: (Coord x, Coord y) -> Board b PieceT -> Board ('Cons x y 'O b) PieceT
playO (coordVal -> x, coordVal -> y) (Board b) 
        = Board $ overTrip y (overTrip x (const O)) b
```

If ViewPatterns are new to you, check out [Oliver Charles'
post](https://ocharles.org.uk/blog/posts/2014-12-02-view-patterns.html) to
learn more.

Now we get both the type level coordinates AND the value level coordinates!
Awesome. We're storing the played pieces in the type list now, but we still
need to check that it's an unplayed square! We wouldn't be type programming
without type functions, let's dive in! In Haskell type functions are called
Type Families, but really they're just functions on types:

```haskell
-- Another pragma >_>
{-# language TypeFamilies #-}

-- | Has a square been played already?
type family Played (x :: CoordT) (y :: CoordT) (b :: BoardRep) :: Bool where
   --  Nothing is played on the 'Empty board
  Played _ _ 'Empty = 'False
   --  We found a match, so the square has already been played
  Played x y ('Cons x y _ _) = 'True
   --  No match yet, but there might be one in the rest of the list
  Played x y ('Cons _ _ _ rest) = Played x y rest
```

This is implemented as a linear search through the list looking for a match.
If we ever find a matching set of coordinates in the list then we know we've
played there already. Notice that type families also return a type, so we specify
the Kind of that return value, in this case `Bool`, so the returned type will be
either `'True` or `'False`.

Let's use this to write constraints for our play functions:

```haskell
playX :: (Played x y b ~ 'False) 
      => (Coord x, Coord y) -> Board b PieceT -> Board ('Cons x y 'X b) PieceT

playO :: (Played x y b ~ 'False) 
      => (Coord x, Coord y) -> Board b PieceT -> Board ('Cons x y 'O b) PieceT
```

Now we're asserting that in order to call this function the board must not have
played on those coordinates yet! If you haven't seen it before, `~` does an
equality check on two types and creates a constraint which requires them to be
equal.

We're close to done, but unfortunately in our upgrade we forgot to ensure that
`X` and `O` always alternate!

## Rechecking Alterating Turns

Checking whose turn it is with our new representation is easier than you might
think; if the last play was `X` then it's `O`s turn, and in all other cases it's
`X`s turn!

```haskell
type family Turn (b :: BoardRep) :: PieceT where
  Turn ('Cons _ _ 'X _) = 'O
  Turn _ = 'X

playX :: (Played x y b ~ 'False, Turn b ~ 'X) =>
         (Coord x, Coord y) -> Board b PieceT -> Board ('Cons x y 'X b) PieceT

playO :: (Played x y b ~ 'False, Turn b ~ 'O) => 
         (Coord x, Coord y) -> Board b PieceT -> Board ('Cons x y 'O b) PieceT
```

We also altered the constraints of playX and playO to reflect the requirement!

We're in good shape now! We can play a game!

```haskell
λ> import Data.Function ((&))
λ> newBoard
    & playX (A', B')
    & playO (C', C')
    & playX (A', A')
Board (Trip (Trip X N N) (Trip X N N) (Trip N N O))

λ> newBoard
    & playX (A', B')
    & playX (C', C')

error:
    • Couldn't match type ‘'O’ with ‘'X’ arising from a use of ‘playX’
    • In the second argument of ‘(&)’, namely ‘playX (C', C')’
      In the expression: newBoard & playX (A', B') & playX (C', C')

λ> newBoard
    & playX (A', B')
    & playO (A', B')

error:
    • Couldn't match type ‘'True’ with ‘'False’
        arising from a use of ‘playO’
    • In the second argument of ‘(&)’, namely ‘playO (A', B')’
      In the expression: newBoard & playX (A', B') & playO (A', B')
```

Looks good to me! As an exercise try combining `playX` and `playO` into a more
general `play`! Here's a hint, you'll want to make another wrapper type like we
did with `Coord`!

Here's the finished product all at once, it's also available as a stack project
in a [git repo here](https://github.com/ChrisPenner/Type-Tac-Toe).:


```haskell
{-# language DeriveFunctor #-}
{-# language KindSignatures #-}
{-# language DataKinds #-}
{-# language ViewPatterns #-}
{-# language GADTs #-}
{-# language TypeFamilies #-}
module TypeTacToe where

import Data.Function ((&))

-- | Either X, O, or Nothing
data PieceT = X | O | N
  deriving (Show, Eq)

data CoordT = A | B | C
  deriving (Show, Eq)

-- | A proxy type which represents a coordinate
data Coord (a :: CoordT) where
  A' :: Coord 'A
  B' :: Coord 'B
  C' :: Coord 'C

-- | Get the coord's actual value from a wrapper type
coordVal :: Coord a -> CoordT
coordVal A' = A
coordVal B' = B
coordVal C' = C

data Trip a = Trip a a a
  deriving (Show, Eq, Functor)

-- | Utility function to alter a value inside a triple
-- Can build get / set using `flip const ()` and `const x` respectively
overTrip :: CoordT -> (a -> a) -> Trip a -> Trip a
overTrip A f (Trip a b c) = Trip (f a) b c
overTrip B f (Trip a b c) = Trip a (f b) c
overTrip C f (Trip a b c) = Trip a b (f c)

-- | Keep a list of each Piece played and its location
data BoardRep = Empty
              | Cons CoordT CoordT PieceT BoardRep


-- A board is a 3x3 grid alongside its type representation
newtype Board (b :: BoardRep) a = Board (Trip (Trip a))
  deriving (Show, Eq, Functor)

-- | New empty board
newBoard :: Board 'Empty PieceT
newBoard = Board $ Trip (Trip N N N)
                        (Trip N N N)
                        (Trip N N N)

-- | Has a square been played already?
type family Played (x :: CoordT) (y :: CoordT) (b :: BoardRep) :: Bool where
  Played _ _ 'Empty = 'False
  Played x y ('Cons x y _ _) = 'True
  Played x y ('Cons _ _ _ rest) = Played x y rest


-- | Get who's turn it is
type family Turn (b :: BoardRep) :: PieceT where
  Turn ('Cons _ _ 'X _) = 'O
  Turn _ = 'X

-- | Play a piece on square (x, y) if it's valid to do so
playX :: (Played x y b ~ 'False, Turn b ~ 'X)
      => (Coord x, Coord y) -> Board b PieceT -> Board ('Cons x y 'X b) PieceT
playX (coordVal -> x, coordVal -> y) (Board b) 
      = Board $ overTrip y (overTrip x (const X)) b

playO :: (Played x y b ~ 'False, Turn b ~ 'O)
      => (Coord x, Coord y) -> Board b PieceT -> Board ('Cons x y 'O b) PieceT
playO (coordVal -> x, coordVal -> y) (Board b) 
      = Board $ overTrip y (overTrip x (const O)) b

game :: Board ('Cons 'A 'A 'O ('Cons 'A 'B 'X 'Empty)) PieceT
game = newBoard
     & playX (A', B')
     & playO (A', A')
```

That last type there is a doozy! The type actually includes the entire game
board, and it'll only grow as we add moves! This exposes some issues with using
this approach for a real-life tic-tac-toe game. Not only are the types unwieldy if
you ever need to specify them, but the type is actually so well
defined that we can't really write a function to use user input!

Give it a try if you don't believe me, we'd want something along the lines of:

```
String -> Board b PieceT -> Board ? PieceT
```

We'd parse the string into the coords for a move. It's really tough to decide what would go
into the `?` though, we can't give it a type because we don't know what the
Coords will be until after we've already parsed the string! This is the sort of
thing that's sometimes possible in Idris' dependent types, but is pretty tricky
in Haskell. You can see Brian McKenna show how to build a [type-safe
`printf`](https://www.youtube.com/watch?v=fVBck2Zngjo) in Idris if you're
interested.

Thanks for joining me, let me know if you found anything confusing; hope you
learned something!
