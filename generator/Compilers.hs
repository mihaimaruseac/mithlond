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
  b <- pandocCompilerWithTransformM readOptions writeOptions f
  loadAndApplyTemplate "templates/default.html" postContext b
  where
    f :: Pandoc -> Compiler Pandoc
    f p = do
      unsafeCompiler $ print p
      return p

readOptions :: ReaderOptions
readOptions = defaultHakyllReaderOptions

writeOptions :: WriterOptions
writeOptions = defaultHakyllWriterOptions

postContext :: Context String
postContext = mconcat
  [ bodyField "body"
  , missingField
  ]
