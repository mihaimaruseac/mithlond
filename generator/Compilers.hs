{-|
Module: Compilers
Description: The compilers used to generate various pages on the site.
Copyright: (c) Mihai Maruseac 2020
Stability: experimental
Portability: POSIX

The compilers used to generate various pages on the site.

This module just re-exports the compiler from the internal layer.
-}

module Compilers
  ( postCompiler
  , indexCompiler
  ) where

import Compilers.Index (indexCompiler)
import Compilers.Post (postCompiler)
