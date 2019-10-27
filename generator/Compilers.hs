{-|
Module: Compilers
Description: The compilers used to generate various pages on the site.
Copyright: (c) Mihai Maruseac 2020
Stability: experimental
Portability: POSIX

The compilers used to generate various pages on the site.
-}

module Compilers
  ( postCompiler
  , indexCompiler
  ) where

import Compilers.Index (indexCompiler)
import Compilers.Post (postCompiler)
