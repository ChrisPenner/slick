---
title: Kleisli Endomorphisms
author: Chris Penner
date: Apr 8, 2017
tags: [haskell, programming]
description: Discovering an isomorphism between the Kleisli version of Endomorphisms to the mtl StateT transformer.
---


After listening to the latest [Magic
Read-along](https://twitter.com/MagicReadAlong) episode ["You should watch
this"](http://www.magicreadalong.com/episode/45) (which you should go listen to
now) I got caught up thinking about Brian's idea of an Endomorphism version of
Kleisli composition for use with
[Redux](https://github.com/reactjs/react-redux), it's actually a very similar
model to what I'm using in my [event framework](github.com/chrispenner/eve) for
event listeners so I figured I'd try to formalize the pattern and recognize
some of the concepts involved. They talk about the idea of a
Redux-reducer, which is usually of type `s -> Action -> s`, it takes a state
and an action and returns a new state. He then re-arranged the arguments to
`Action -> s -> s`. He then recognized this as `Action -> Endo s` (an
Endo-morphism is just any function from one type to itself: `a -> a`). He
would take his list of reducers and partially apply them with the `Action`,
yielding a list of type `Endo s` where `s` is the state object the reducer
operates over. At this point we can use the Monoid instance `Endo` has defined,
so we foldmap with Endo to combine the list of reducers into a sort of pipeline
where each function feeds into the next; the Endo instance of Monoid is just
function composition over functions which return the same type as their input.

This cleans up the interface of the reducers a fair amount, but what about an
alternate kind of `Endo` which uses Kleisli composition instead of normal
function composition? [Kleisli
composition](http://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Monad.html#v:-62--61--62-)
often referenced as (&gt;=&gt;); takes two functions which return monads and
composes them together using the underlying bind/flatmap of the Monad. The type
of Kleisli composition is:
`(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c`. If we could define
a nice Endo-style monoid over this type then we could compose reducers like we
did above, but also allow the functions to perform monadic effects (which is a
bad idea in Redux, but there are other times this would be useful, imagine
running a user through a pipeline of transformations which interact with a
database or do some error handling). We can easily define this instance like
so:

```haskell
import Control.Monad
newtype KEndo m a = KEndo
  { getKEndo :: (a -> m a) }
  
instance Monad m => Monoid (KEndo m a) where
  mempty = KEndo return
  (KEndo a) `mappend` (KEndo b) = KEndo (a >=> b)  
```

This is great, now if we have a list of functions of some type `[User -> Writer Error User]` or something we can
use foldmap to combine them into a single function! It works like this:

```haskell
actions :: [User -> Writer Error User]
actions = [...]

pipeline :: User -> Writer Error User
pipeline = getKEndo . foldMap KEndo $ actions
```

The whole Kleisli Endo thing is a cool idea; but this thing has actually been
done before! It's actually the same as the
[`StateT`](http://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State-Lazy.html#v:StateT)
state monad transformer from mtl; let's see how we can make the comparison. A
generic Endo is of type `s -> s`, this is isomorphic to `s -> ((), s)`, aka
`State s ()`. The trick is that the Kleisli Endo (`s -> m s` or by isomorphism
`s -> m ((), s)`) can actually be generalized over the `()` to `s -> m (a, s)`
which incidentally matches `runStateT :: StateT s m a -> s -> m (a, s)` from
mtl!

So basically KEndo is isomorphic to StateT, but we'd still like a
monoid instance for it, Gabriel shows a monoid over the IO monad in ["Applied category
theory and abstract algebra"](https://youtu.be/WsA7GtUQeB8), the Monoid he shows actually generalizes to any monad as
this instance:

```haskell
instance (Monad m, Monoid a) => Monoid (m a) where
  mempty = return mempty
  ma `mappend` mb = do
    a <- ma
    b <- mb
    return (a `mappend` b)
```

So that means we can use this instance for StateT (which is a monad). Since
`()` is a trivial monoid (where every mappend just returns `()`) the simple
case is `State s ()` which was our `KEndo` of `s -> ((), s)` but now we have
the Monoid instance, which behaves the same as the `KEndo` instance, so we
don't need `KEndo` anymore. If we want to allow arbitrary effects we use the
Transformer version: `StateT s m ()` where `m` is a monad containing any
additional effects we want. In addition to being able to add additional effects
we also gain the ability to aggregate information as a monoid! If you decided
you wanted your reducers to also aggregate some form of information, then
they'd be: `Monoid a => Action -> s -> (a, s)`, which is `Action -> State s a`,
and if `a` is a monoid, then the monoid instance of `State` acts like Endo, but
also aggregates the 'a's along the way!

Lastly we recognize that in the case of the Redux Reducers, if we have a whole
list of reducers: `Action -> State s ()` then we can rephrase it as the ReaderT
Monad: `ReaderT Action (State s) ()`, which maintains all of the nice monoids
we've set up so far, and becomes even more composable!
