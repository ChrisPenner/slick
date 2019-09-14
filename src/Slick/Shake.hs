{-|
Module      : Slick.Shake
Description : Slick utilities for working with shake
Copyright   : (c) Chris Penner, 2019
License     : BSD3
-}
module Slick.Shake
    ( slick
    , slickWithOpts
    ) where

import Development.Shake
import Development.Shake.Forward

-- | Build your slick site. This is a good candidate for your 'main' function.
--
-- Calls through to 'shakeArgsForward' with extra verbosity
slick :: Action () -> IO ()
slick buildAction =
    slickWithOpts (shakeOptions { shakeVerbosity = Chatty }) buildAction

-- | Build your slick site with the provided shake options. This is a good candidate for your 'main' function.
--
-- | Calls through to 'shakeArgsForward' with the provided options
slickWithOpts :: ShakeOptions -> Action () -> IO ()
slickWithOpts opts buildAction =
    shakeArgsForward opts buildAction
