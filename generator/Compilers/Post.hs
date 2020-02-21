{-|
Module: Compilers.Post
Description: Definitions and compiler for posts
Copyright: (c) Mihai Maruseac 2020
Stability: experimental
Portability: POSIX

Definitions and compiler for posts.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Compilers.Post
  ( postCompiler
  , postContext
  ) where

import Control.Applicative (empty)

import qualified Data.Set as Set
import qualified Data.Text as Text

import Hakyll
import Text.Pandoc

postCompiler :: Compiler (Item String)
postCompiler =
  pandocCompilerWith readOptions writeOptions >>=
  loadAndApplyTemplate "templates/post.html" postContext >>=
  loadAndApplyTemplate "templates/default.html" postContext

-- | The post context for the fields in @templates/post.html@.
postContext :: Context String
postContext = mconcat
  [ postTitleField
  , urlField "url"
  , dateField "date" "%F"
  , bodyField "body"
  ]

-- | Title field for the post metadata.
-- The default @titleField@ returns only the name of the file which is not
-- what we want.
-- The current implementation requires titles to be set explicitly but that's
-- what we want.
postTitleField :: Context String
postTitleField = Context $ \case
  "title" -> \_ -> getTitle
  _ -> \_ _ -> empty
  where
    getTitle i = StringField . maybe empty id <$>
      getMetadataField (itemIdentifier i) "title"

-- | Options for the parser of markdown posts
-- Explicitly set these up instead of relying on defaults to make sure we have
-- full control and are immune to changes from upstream.
readOptions :: ReaderOptions
readOptions = def

-- | Options for the HTML writer for each article
-- We change at least the math format (@writerHTMLMathMethod@) and add support
-- for table of contents (@writerTemplate@ and @writerTableOfContents@).
-- Make sure to set code highlighting format (@writerHighlightStyle@).
-- Also nice to have is email obfuscation (@writerEmailObfuscation@), wrapping
-- sections on an extra <section> tag (for CSS styling, @writerSectionDivs@).
-- Explicitly set all of the remaining ones instead of relying on defaults to
-- make sure we have full control and are immune to changes from upstream.
writeOptions :: WriterOptions
writeOptions = def
  { writerTabStop           = 2 -- converting tabs to spaces
  , writerHTMLMathMethod    = MathML -- seems to be the only one working
  , writerNumberSections    = True -- Use `.unnumbered` or `-` as attribute to not number
  , writerSectionDivs       = True -- wrap <h..> with <section>
  , writerExtensions        = mithlondExtensions
  , writerEmailObfuscation  = ReferenceObfuscation
  , writerHtmlQTags         = True
  }

-- | Enabled extensions for the reader and the writer.
-- Explicitly set these up instead of relying on defaults to make sure we have
-- full control and are immune to changes from upstream.
mithlondExtensions :: Extensions
mithlondExtensions = extensionsFromList
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
