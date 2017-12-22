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

import qualified Data.Set as Set

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

-- | Explicitly set these up instead of relying on defaults to make sure we
-- have full control and are immune to changes from upstream.
readOptions :: ReaderOptions
readOptions = ReaderOptions
  { readerExtensions            = extensionsFromList readExtensions
  , readerStandalone            = False
  , readerColumns               = 80 -- TODO: check this
  , readerTabStop               = 4 -- TODO: check this
  , readerIndentedCodeClasses   = [] -- TODO: check this
  , readerAbbreviations         = abbreviations
  , readerDefaultImageExtension = ""
  , readerTrackChanges          = AcceptChanges -- definitely not relevant
  , readerStripComments         = False -- TODO: check this for minimizing output? check debug?
  }
  where
    readExtensions = []
    abbreviations = Set.empty

writeOptions :: WriterOptions
writeOptions = defaultHakyllWriterOptions

postContext :: Context String
postContext = mconcat
  [ bodyField "body"
  , missingField
  ]
