{-|
Module: Compilers.Index
Description: Definitions and compiler for the index page
Copyright: (c) Mihai Maruseac 2018
Stability: experimental
Portability: POSIX

Definitions and compiler for the index page
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compilers.Index
  ( indexCompiler
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)

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
compilePostList = loadAll patternPosts >>= sortById

-- | Sort the posts in decreasing order based on their ID
sortById :: forall m a . MonadMetadata m => [Item a] -> m [Item a]
sortById = fmap (map snd . sortBy (comparing fst)) . mapM prepare
  where
    prepare :: Item a -> m (Int, Item a)
    prepare i = do
      metadata <- fmap reads <$> getMetadataField (itemIdentifier i) "postid"
      case metadata of
        Just [(id'', "")] -> return (id'', i)
        Just _  -> fail "Failed to parse the $postid$ field of a post"
        Nothing -> fail "Found a post with missing $postid$ field"
