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
  { readerExtensions            = readExtensions
  , readerStandalone            = False
  , readerColumns               = 80 -- doesn't seem to have any effect
  , readerTabStop               = 4  -- indentantion for code and continuations
  , readerIndentedCodeClasses   = [] -- works only for code indented by a number of spaces
  , readerAbbreviations         = abbreviations
  , readerDefaultImageExtension = ""
  , readerTrackChanges          = AcceptChanges -- definitely not relevant
  , readerStripComments         = False
  }
  where
    abbreviations = Set.empty

readExtensions :: Extensions
readExtensions = extensionsFromList
  [ Ext_footnotes
  , Ext_inline_notes
  , Ext_pandoc_title_block
  , Ext_yaml_metadata_block
  , Ext_table_captions
  , Ext_implicit_figures
  , Ext_simple_tables
  , Ext_multiline_tables
  , Ext_grid_tables
  , Ext_pipe_tables
  , Ext_citations
  , Ext_raw_tex
  , Ext_raw_html
  , Ext_tex_math_dollars
  , Ext_latex_macros
  , Ext_fenced_code_blocks
  , Ext_fenced_code_attributes
  , Ext_backtick_code_blocks
  , Ext_inline_code_attributes
  , Ext_raw_attribute
  , Ext_markdown_in_html_blocks
  , Ext_native_divs
  , Ext_fenced_divs
  , Ext_native_spans
  , Ext_bracketed_spans
  , Ext_escaped_line_breaks
  , Ext_fancy_lists
  , Ext_startnum
  , Ext_definition_lists
  , Ext_example_lists
  , Ext_all_symbols_escapable
  , Ext_intraword_underscores
  , Ext_blank_before_blockquote
  , Ext_blank_before_header
  , Ext_space_in_atx_header
  , Ext_strikeout
  , Ext_superscript
  , Ext_subscript
  , Ext_auto_identifiers
  , Ext_header_attributes
  , Ext_link_attributes
  , Ext_implicit_header_references
  , Ext_line_blocks
  , Ext_shortcut_reference_links
  , Ext_smart
  , Ext_emoji -- allow emoji's of form :smile:
  , Ext_tex_math_double_backslash -- allow display-style math with \\[..\\]
  ]

writeOptions :: WriterOptions
writeOptions = defaultHakyllWriterOptions

postContext :: Context String
postContext = mconcat
  [ bodyField "body"
  , missingField
  ]
