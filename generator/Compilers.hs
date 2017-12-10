{-|
Module: Compilers
Description: The rules used to generate various pages on the site.
Copyright: (c) Mihai Maruseac 2017
Stability: experimental
Portability: POSIX

The rules used to generate various pages on the site.
-}

{-# LANGUAGE OverloadedStrings #-}

module Compilers
  ( postCompiler
  ) where

import Hakyll
import Text.Pandoc

postCompiler :: Compiler (Item String)
postCompiler = do
  b <- pandocCompilerWithTransformM ro wo f
  applyTemplate (readTemplate templateString) context b
  where
    context = missingField
    templateString = "<html><body>Test</body></html>"
    ro = defaultHakyllReaderOptions
    wo = defaultHakyllWriterOptions
    f :: Pandoc -> Compiler Pandoc
    f p = do
      unsafeCompiler $ print p
      return p
