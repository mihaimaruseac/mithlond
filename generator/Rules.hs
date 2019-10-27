{-|
Module: Rules
Description: The rules used to generate various pages on the site.
Copyright: (c) Mihai Maruseac 2020
Stability: experimental
Portability: POSIX

The rules used to generate various pages on the site.
-}

{-# LANGUAGE OverloadedStrings #-}

module Rules (siteRules) where

import Hakyll

import Compilers
import Patterns

-- | The rules used to build the site
siteRules :: Rules ()
siteRules = do
  match patternIndex indexRules
  match patternPosts postRules
  match patternTemplates templateRules
  match patternCSS cssRules
  match patternFonts fontRules

-- | Rules for the index page
-- Should display links to all of the articles, in a nice tabular format
indexRules :: Rules ()
indexRules = do
  route idRoute
  compile indexCompiler

-- | Rules to compile an individual blog post
-- The meat of the blog, after all.
postRules :: Rules ()
postRules = do
  route $ metadataRoute routeDef
  compile postCompiler
  where
    -- if postid is found, we route to $it/index.html
    -- otherwise, just change extension to allow the possibility of
    -- posts not reachable from the index
    routeDef m = case lookupString "postid" m of
      Just id' -> constRoute $ id' `mappend` "/index.html"
      Nothing -> setExtension "html"

-- | Rules to compile templates
-- Should just use the default, all is good.
templateRules :: Rules ()
templateRules = compile templateCompiler

-- | Rules to compile CSS
-- Default route and compress CSS
cssRules :: Rules ()
cssRules = do
  route idRoute
  compile compressCssCompiler

-- | Rules to compile custom fonts
-- Default route and compress custom fonts
fontRules :: Rules ()
fontRules = do
  route idRoute
  compile copyFileCompiler
