{-|
Module: Compilers.Index
Description: Definitions and compiler for the index page
Copyright: (c) Mihai Maruseac 2018
Stability: experimental
Portability: POSIX

Definitions and compiler for the index page
-}

{-# LANGUAGE OverloadedStrings #-}

module Compilers.Index
  ( indexCompiler
  ) where

import Hakyll

import Compilers.Post (postContext)
import Patterns (patternPosts)

-- | The compiler for the index page
indexCompiler :: Compiler (Item String)
indexCompiler = getResourceBody >>=
  applyAsTemplate indexContext >>=
  loadAndApplyTemplate "templates/default.html" indexContext

-- | The context containing the metadata used to fill in the index page
indexContext :: Context String
indexContext =
  listField "posts" postContext compilePostList `mappend` postContext

-- | Compile the list of posts to show on the index
-- Needs to load all posts and sort them and extract the relevant context
-- TODO: see if we can generate HTML snippets for each post and load only
-- those instead of generating the entire post (see @Hakyll.Web.Template.List@)
compilePostList :: Compiler [Item String]
compilePostList = loadAll patternPosts
