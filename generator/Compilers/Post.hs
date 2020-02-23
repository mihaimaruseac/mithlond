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

import Hakyll

postCompiler :: Compiler (Item String)
postCompiler = getResourceBody >>=
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
