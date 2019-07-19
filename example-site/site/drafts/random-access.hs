#!/usr/bin/env stack
{- stack --resolver lts-11.15 --install-ghc runghc
 --package fingertree
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.FingerTree
import Data.Monoid

main :: IO ()
main = print $ atIndex 0 alphabet

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
