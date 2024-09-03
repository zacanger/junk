#!/usr/bin/env stack
-- stack runghc --resolver lts-6.24 --install-ghc --package fsnotify
{-# LANGUAGE OverloadedStrings #-}

import System.FSNotify
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

main =
  withManager $ \mgr -> do
    watchDir
      mgr
      "."
      (const True)
      print

    forever $ threadDelay 1000000
