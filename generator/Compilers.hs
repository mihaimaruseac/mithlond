{-|
Module: Compilers
Description: The compilers used to generate various pages on the site.
Copyright: (c) Mihai Maruseac 2018
Stability: experimental
Portability: POSIX

The compilers used to generate various pages on the site.
-}

module Compilers
  ( postCompiler
  , indexCompiler
  ) where

import Hakyll

import Compilers.Post (postCompiler)

-- | The compiler for the index page
indexCompiler :: Compiler (Item String)
indexCompiler = getResourceBody
