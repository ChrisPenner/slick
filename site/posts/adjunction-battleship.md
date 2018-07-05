---
title: "Adjunctions and Battleship"
author: Chris Penner
date: Jul 19, 2017
tags: [haskell, programming]
description: Using Adjunctions and Representable to build a Battleship game
image: battleship.jpg
---

Today we'll be looking into Kmett's
[adjunctions](http://hackage.haskell.org/package/adjunctions) library,
particularly the meat of the library in Data.Functor.Adjunction.

This post as a literate haskell file
[here](https://gist.github.com/ChrisPenner/291038ae1343333fb41523b41181a9d4),
so if you prefer to have the code running in ghci as you read along then go for
it! Like any good haskell file we need half a dozen language pragmas and
imports before we get started.

```haskell
{-# language DeriveFunctor #-}
{-# language TypeFamilies #-}
{-# language MultiParamTypeClasses #-}
{-# language InstanceSigs #-}
{-# language FlexibleContexts #-}
module Battleship where
import Data.Functor (void)
import Data.Functor.Adjunction
import Data.Functor.Rep
import Data.Distributive
import Control.Arrow ((&&&))
```

I've been struggling to understand this library for a little while
now and have been poking at it from different angles trying to gain
some intuition. My previous post on [Zippers using Representable and
Cofree](http://chrispenner.ca/posts/representable-cofree-zippers)
is part of that adventure so I'd suggest you read that first if you haven't yet.

Like most higher-level mathematic concepts Adjunctions themselves are just
an abstract collection of types and shapes that fit together in a certain
way. This means that they have little practical meaning on their own,
but provide a useful set of tools to us if we happen to notice that some
problem we're working on matches their shape. The first time I dug into
adjunctions I went straight to the typeclass to check out which requirements
and methods it had. Here are the signatures straight from the source code in
Data.Functor.Adjunction

```haskell
class (Functor f, Representable u) => Adjunction f u | f -> u, u -> f where
  unit         :: a -> u (f a)
  counit       :: f (u a) -> a
  leftAdjunct  :: (f a -> b) -> a -> u b
  rightAdjunct :: (a -> u b) -> f a -> b
```

Hrmm... not the most illuminating. Unfortunately there's not much in
the way of documentation to help us out, but that's because the type signatures
pretty much explain how to USE adjunctions, but tragically they don't tell us
WHERE or HOW to use them. For this I think examples are the most useful, and
that's where I'll try to help out.

The first place to look for examples is in the 'instances' section of the type-class
itself, let's see what's in there:

```haskell
Adjunction Identity Identity
Adjunction ((,) e) ((->) e)
Adjunction f g => Adjunction (IdentityT f) (IdentityT g)
Adjunction f u => Adjunction (Free f) (Cofree u)
Adjunction w m => Adjunction (EnvT e w) (ReaderT e m)
Adjunction m w => Adjunction (WriterT s m) (TracedT s w)
(Adjunction f g, Adjunction f' g') => Adjunction (Compose f' f) (Compose g g')
(Adjunction f g, Adjunction f' g') => Adjunction (Sum f f') (Product g g')
```

Hrmm, still not the most helpful, most of these instances depend on some
underlying functor ALREADY having an adjunction so those won't tell us how to
implement one. I see one for `Adjunction Identity Identity`, but something
tells me that's not going to provide much depth either. Let's
dive into the one remaining example: `Adjunction ((,) e) ((->) e)`

This one looks a little funny if you're not used to type sigs for functions and
tuples, but it gets a lot easier to read if we substitute it into the typeclass
methods. To specialize for the tuple/function adjunction we'll replace every
`f a` with `(e, a)` and each `u a` with `e -> a`:

```haskell
-- Tuple/Function adjunction specializations:
tfUnit :: a -> (e -> (e, a))
tfCounit :: (e, (e -> a)) -> a
tfLeftAdjunct, tfLeftAdjunct'  :: ((e, a) -> b) -> a -> (e -> b)
tfRightAdjunct, tfRightAdjunct' :: (a -> (e -> b)) -> (e, a) -> b
```

Hrmm, okay! That's a bit confusing but it's something we can work with. Let's
try to implement the functions! We'll implement our specialized versions so as
not to collide with the existing instance.

Unit and Counit are good starting points for understanding an adjunction. The
minimal definition of an adjunction is (unit AND counit) OR (leftAdjunct AND
rightAdjunct). That lets us know that unit and counit can themselves represent
the entire adjunction (i.e. leftAdjunct and rightAdjunct can be implemented in
terms of unit and counit; or vice versa).

Starting with `unit` we see from the type `a -> (e -> (e, a))` that we need to
take an arbitrary 'a' and embed it into a function which returns a tuple of the
same type as the function. Well, there's pretty much only one way I can think
to make this work!

```haskell
tfUnit a = \e -> (e, a)
```

Solid! We just converted the type signature into an implementation.
One down, three to go. This may not provide much insight, but don't worry we'll
get there yet. Next is counit which essentially does the opposite, exactly one
implementation seems clear to me: `(e, (e -> a)) -> a`

```haskell
tfCounit (e, eToA) = eToA e
```

If we stop here for a minute we can notice a few things, we built this
adjunction out of two functors, `(e, a)` and `e -> a`. These functors have a
unique relationship to one another in that they both hold *pieces* of the whole
picture, the tuple has an 'e' but doesn't know what to do with it, while `e ->
a` knows what to do with an 'e' but doesn't have one to work with! Only when we
pair the functors together do we have the full story!

The next thing to notice is that these functors are only readily useful when
nested in a specific ordering, we can write a counit which takes `(e, (e -> a))
-> a`, BUT if we tried to put the function on the outside instead: `(e -> (e,
a)) -> a`; we have no way to get our 'a' out without having more information
since the 'e' is now hidden inside! This non-symmetric relationship shows us
that the nesting of functors matters. This is why we refer to the functors in
an adjunction as either `left adjoint` or `right adjoint`; (`f` and `u` respectively).

In our case `(e,)` is left adjoint and `(e ->)` is right adjoint. This is
probably still a bit confusing and that's okay! Try to hold on until we get to
start playing Battleship and I promise we'll have a more concrete example! One
more thing first, let's see how leftAdjunct and rightAdjunct play out for our
tuple/function adjunction.

Here's a refresher of the types:

```haskell
tfLeftAdjunct :: ((e, a) -> b) -> a -> (e -> b)
tfRightAdjunct :: (a -> (e -> b)) -> (e, a) -> b
```

Now that we've written 'unit' and 'counit' we can implement these other functions
in terms of those. I'll provide two implementations here; one using unit/counit and
one without.

```haskell
tfLeftAdjunct f = fmap f . tfUnit
tfRightAdjunct f = tfCounit . fmap f
```

```haskell
tfLeftAdjunct' eaToB a = \e -> eaToB (e, a)
tfRightAdjunct' aToEToB (e, a) = aToEToB a e
```

We can see from the first set of implementations that `leftAdjunct` somehow
'lifts' a function that we give it from one that operates over the left-hand
functor into a result within the right-hand functor.

Similarly `rightAdjunct` takes a function which results in a value in left-hand
functor, and when given an argument embedded in the left-hand functor gives us
the result. The first set of implementations know nothing about the functors in
specific, which shows that if we write unit and counit we can let the default
implementations take over for the rest.

If you're keen you'll notice that this adjunction represents the curry and
uncurry functions! Can you see it?

```haskell

tfLeftAdjunct :: ((e, a) -> b) -> a -> (e -> b)
curry :: ((a, b) -> c) -> a -> b -> c

tfRightAdjunct :: (a -> (e -> b)) -> (e, a) -> b
uncurry :: (a -> b -> c) -> (a, b) -> c
```

I haven't gotten to a point where I can prove it yet, but I believe all adjunctions
are actually isomorphic to this curry/uncurry adjunction! Maybe someone reading can
help me out with the proof.

Again, it's fun to see this play out, but where are the practical
applications?? Let's play a game. It's time to see if we can match these
shapes and patterns to a real(ish) problem. We're going to make a mini game of
Battleship, an old board game where players can guess where their opponents
ships are hiding within a grid and see if they can hit them! We'll start by
setting up some data-types and some pre-requisite instances, then we'll tie it
all together with an Adjunction!

```haskell
data Row = A | B | C
  deriving (Show, Eq)
data Column = I | II | III
  deriving (Show, Eq)
-- I'm going to define this as a Functor type to save time later, but for now
-- we'll use the alias Coord;
data CoordF a = CoordF Row Column a
  deriving (Show, Eq, Functor)
type Coord = CoordF ()
```

Each cell can hold a Vessel of some kind, maybe a Ship or Submarine;
It's also possible for a cell to be empty.

```haskell
data Vessel = Ship | Sub | Sunk | Empty
  deriving (Show, Eq)
```

We'll start with a 3x3 board to keep it simple, each row is represented by a
3-tuple. We've learned by now that making our types into Functors makes them
more usable, so I'm going to define the board as a functor parameterized over
the contents of each cell.

```haskell
data Board a = Board
  (a, a, a)
  (a, a, a)
  (a, a, a)
  deriving (Eq, Functor)
```

I'm going to add a quick Show instance, it's not perfect but it lets us see the
board!

```haskell
instance (Show a) => Show (Board a) where
  show (Board top middle bottom) =
    "       I  |  II | III\n"
    ++ "A   " ++ show top ++ "\n"
    ++ "B   " ++ show middle ++ "\n"
    ++ "C   " ++ show bottom ++ "\n"
```

Here's a good starting position, the board is completely empty!

```haskell
startBoard :: Board Vessel
startBoard = Board
  (Empty, Empty, Empty)
  (Empty, Empty, Empty)
  (Empty, Empty, Empty)
```

It's at this point we want to start making guesses using a Coord and
seeing what's in each position! How else are we going to sink the
battleship? Well, when we start talking about 'Indexing' into our board
(which is a functor) I think immediately of the Representable typeclass from
[Data.Functor.Rep](https://hackage.haskell.org/package/adjunctions-4.3/docs/Dat
a-Functor-Rep.html#t:Representable). Don't let the name scare you, one of the
things that Representable gives you is the notion of *indexing* into a functor.

```haskell
instance Representable Board where
  -- We index into our functor using Coord
  type Rep Board = Coord

  -- Given an index and a board, pull out the matching cell
  index (Board (a, _, _) _ _) (CoordF A I _) = a
  index (Board (_, a, _) _ _) (CoordF A II _) = a
  index (Board (_, _, a) _ _) (CoordF A III _) = a
  index (Board _ (a, _, _) _) (CoordF B I _) = a
  index (Board _ (_, a, _) _) (CoordF B II _) = a
  index (Board _ (_, _, a) _) (CoordF B III _) = a
  index (Board _ _ (a, _, _)) (CoordF C I _) = a
  index (Board _ _ (_, a, _)) (CoordF C II _) = a
  index (Board _ _ (_, _, a)) (CoordF C III _) = a

  -- Given a function which describes a slot, build a Board
  tabulate desc = Board
      (desc (CoordF A I ()), desc (CoordF A II ()), desc (CoordF A III ()))
      (desc (CoordF B I ()), desc (CoordF B II ()), desc (CoordF B III ()))
      (desc (CoordF C I ()), desc (CoordF C II ()), desc (CoordF C III ()))
```

If you find it easier to implement unit and counit (which we'll explore soon) you
can implement those and then use `indexAdjunction` and `tabulateAdjunction` provided
by Data.Functor.Adjunction as your implementations for your Representable instance.


For Representable we also have a prerequisite of Distributive from
[Data.Distributive](https://hackage.haskell.org/package/distributive-0.5.0.2/do
cs/Data-Distributive.html#t:Distributive), All Representable functors are also
Distributive and this library has decided to make that an explicit requirement.

No problem though, it turns out that since every Representable is Distributive
that Data.Functor.Rep has a `distributeRep` function which provides an appropriate
implementation for us for free! We just need to slot it in:

```haskell
instance Distributive Board where
  distribute = distributeRep
```

Phew! A lot of work there, but now we can do some cool stuff! Let's say that
as a player we want to build a game board with some ships on it. We now have
two choices, we can either define a board and put some ships on it, or define a
function which says what's at a given coordinate and use that to build a board.
Let's do both, for PEDAGOGY!

```haskell
myBoard1 :: Board Vessel
myBoard1 = Board
  (Empty, Empty, Ship)
  (Sub,   Empty, Sub)
  (Ship,  Empty, Empty)

-- Now we'll define the same board using a function
define :: Coord -> Vessel
define (CoordF A III _) = Ship
define (CoordF B I _) = Sub
define (CoordF B III _) = Sub
define (CoordF C I _) = Ship
-- Otherwise it's Empty!
define _ = Empty

-- Now we build up a board using our descriptor function.
-- Notice that (myBoard1 == myBoard2)
myBoard2 :: Board Vessel
myBoard2 = tabulate define
```

Okay this is already pretty cool; but I *DID* promise we'd use an adjunction
here somewhere, but for that we need TWO functors. Remember how CoordF is
actually a functor hidden undernath Coord? We can use that! This functor
doesn't make much sense on its own, but the important bit is that it's a
functor which contains part of the information about our system. Remember that
only one of our functors needs to be Representable in an Adjunction, so we can
take it easy and don't need to worry about Distributive or Representable for
CoordF

Now for the good stuff; let's crack out Adjunction and see if we can write an
instance!


I'm lazy, so I'm going to rely on Representable to do the dirty work,
Embedding an a into a Board filled with coordinates and values
doesn't make a ton of sense, but the most sensible way that I can
think of to do that is to put the a in every slot where the Coord represents
the index of the cell its in.

```haskell
instance Adjunction CoordF Board where
  unit :: a -> Board (CoordF a)
  unit a = tabulate (\(CoordF row col ()) -> CoordF row col a)
```

Counit actually makes sense in this case! We have our two pieces of info
which form the parts of the adjunction; The board contains the values in ALL
positions and the CoordF contains info which tells us exactly WHICH position
we're currently interested in.

For counit I'm just going to use index to pull the value out of the underlying board.

```haskell
  counit :: CoordF (Board a) -> a
  counit (CoordF row col board) = index board (CoordF row col ())
```

Done! We've written our Adjunction, let's keep building to game to see how we
can use the system! Here're the other type sigs for our Adjunction:

```haskell
leftAdjunct  :: (CoordF a -> b) -> a -> Board b
rightAdjunct :: (a -> Board b) -> CoordF a -> b
```

First let's observe unit and co-unit in action!

`unit` Always does the naive thing, so if we pass it a Vessel it'll just
set the whole board to that value; note that each slot is also labelled with its index!


```haskell
λ> unit Ship :: Board (CoordF Vessel)
       A  |  B  | C
I   (CoordF A I Ship,CoordF A II Ship,CoordF A III Ship)
II  (CoordF B I Ship,CoordF B II Ship,CoordF B III Ship)
III (CoordF C I Ship,CoordF C II Ship,CoordF C III Ship)
```

If we already have our game board and also have an index then counit folds down the
structure by choosing the index specified by the outer CoordF Functor.

```haskell
-- Remember our board:
λ> myBoard1
       A  |  B  | C
I   (Empty,Empty,Ship)
II  (Sub,Empty,Sub)
III (Ship,Empty,Empty)

λ> counit . CoordF A III $ myBoard1
Ship
```

So what about leftAdjunct and rightAdjunct? Conceptually you can think of these as
functions which let you operate over one piece of information and the Adjunction
will form the other piece of information for you! For instance leftAdjunct:

```haskell
leftAdjunct  :: (CoordF a -> b) -> a -> Board b
```

lets you build a value in the right adjoint functor by specifying how to handle
each index, this is similar to `tabulate` from from Representable. Earlier we
used tabulate to generate a game board from a shoot function, we can do the same
thing using leftAdjunct, we could re-implement our `shoot` function from above
in terms of leftAdjunct:

```haskell
myBoard3 :: Board Vessel
myBoard3 = leftAdjunct define ()
```

Right adjunct works similarly, but in reverse! Given a way to create
a board from a solitary value we can extract a value from the board matching
some CoordF. Just like leftAdjunct lines up with 'tabulate', rightAdjunct lines
up with 'index', but with a smidge of extra functionality.

```haskell
rightAdjunct :: (a -> Board b) -> CoordF a -> b
```

I don't have any illuminating uses of rightAdjunct for our Battleship example,
but you can use it to reimplement 'index' from Representable if you like!

```haskell
myIndex :: Board a -> CoordF () -> a
myIndex board coord = rightAdjunct (const board) coord
```


Cool, now let's try and make this game a little more functional!

Already we've got most of the basics for a simple game of battleship,
earlier we defined a game board in terms of a 'firing' function, now let's
write a function which takes a game board and mutates it according to a player's
layout.

War has changed over the years so our version of battleship is going to be a
bit more interesting than the traditional version. In our case each player
places ships OR submarines on each square, and when firing on a square they
may fire either a torpedo (hits ships) OR a depth charge (hits subs).

This means that we need a way to check not only if a cell is occupied, but also
if the vessel there can be hit by the weapon which was fired! For this we'll
take a look at the useful but vaguely named `zapWithAdjunction` function.

This function has its roots in an 'Pairing' typeclass which eventually was
absorbed by Adjunction. The idea of a Functor Pairing is that there's a relationship
between the structure of the two paired functors regardless of what's inside. Sounds
like an adjunction right?? `zapWithAdjunction` looks like this:

```haskell
zapWithAdjunction :: Adjunction f u => (a -> b -> c) -> u a -> f b -> c
```

or for our types:

```haskell
zapWithAdjunction :: (a -> b -> c) -> Board a -> CoordF b -> c
```

So it pairs a Board and Coord together, but applies a function *across* the
values stored there. It uses the adjunction to do this, so it will automagically
choose the 'right' value from the Board to apply with the value from the CoordF!

First we need weapons!

```haskell
data Weapon = Torpedo | DepthCharge
  deriving (Show, Eq)
```

Now we can write something like this:

```haskell
checkHit :: Vessel -> Weapon -> Bool
checkHit Ship Torpedo = True
checkHit Sub DepthCharge = True
checkHit _ _ = False

shoot :: Board Vessel -> CoordF Weapon -> Bool
shoot = zapWithAdjunction checkHit
```

And of course we can try that out!

```haskell
λ> myBoard1
       A  |  B  | C
I   (Empty,Empty,Ship)
II  (Sub,Empty,Sub)
III (Ship,Empty,Empty)
λ> shoot myBoard1 (CoordF A III Torpedo)
True
λ> shoot myBoard1 (CoordF A III DepthCharge)
False
```

It's really unique how Adjunctions let us specify our data as a functor like this!

Now what if we want to see what happens at each spot in the board if we hit it
with a Torpedo OR a DepthCharge? No problem;

```haskell
hitMap :: Board (Bool, Bool)
hitMap = fmap (flip checkHit Torpedo &&& flip checkHit DepthCharge) myBoard1
```

We use (&&&) from Control.Arrow which combines two functions which take the same
input and makes a single function which returns a tuple!

```haskell
(&&&) :: Arrow a => a b c -> a b c' -> a b (c, c')
```

Now we've got a `Board (Bool, Bool)`, Since the right adjoint functor (Board) is
distributive, flipping the the tuple to the outside is trivial:

```haskell
hitMap' :: (Board Bool, Board Bool)
hitMap' = unzipR hitMap
```

Now we've got two Boards, showing where we could get a hit if we used a Torpedo
or DepthCharge respectively.

Most of the functions we've written are a bit contrived. Sometimes the
adjunction-based approach was a bit clunkier than just writing a simple
function to do what you needed on a Board, but I hope this provides some form
of intuition for adjunctions. Good luck!


