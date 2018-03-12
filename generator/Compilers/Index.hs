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

import Data.List (sortOn)
import Data.Maybe (catMaybes)

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
-- We just skip posts which don't have a $postid$ field or the field cannot be
-- parsed to an integer (allowing us to have drafts that don't show up in
-- index).
sortById :: forall m a . MonadMetadata m => [Item a] -> m [Item a]
sortById = fmap (map snd . sortOn fst . catMaybes) . mapM prepare
  where
    prepare :: Item a -> m (Maybe (Int, Item a))
    prepare i = do
      metadata <- fmap reads <$> getMetadataField (itemIdentifier i) "postid"
      case metadata of
        Just [(id'', "")] -> return $ Just (-id'', i)
        _ -> return Nothing
