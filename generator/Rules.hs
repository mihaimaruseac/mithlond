{-|
Module: Rules
Description: The rules used to generate various pages on the site.
Copyright: (c) Mihai Maruseac 2018
Stability: experimental
Portability: POSIX

The rules used to generate various pages on the site.
-}

{-# LANGUAGE OverloadedStrings #-}

module Rules (siteRules) where

import Hakyll

import Compilers

-- | The rules used to build the site
siteRules :: Rules ()
siteRules = do
  match (fromRegex "^posts/-?[0-9]+/.*\\.md$") $ postRules
  match "templates/*" $ templateRules
  match "index.html" $ indexRules

-- | Rules to compile an individual blog post
-- The meat of the blog, after all.
postRules :: Rules ()
postRules = do
  route $ setExtension "html"
  compile postCompiler

-- | Rules to compile templates
-- Should just use the default, all is good.
templateRules :: Rules ()
templateRules = compile templateCompiler

-- | Rules for the index page
-- Should display links to all of the articles, in a nice tabular format
-- TODO: investigate other display options
indexRules :: Rules()
indexRules = do
  route idRoute
  compile indexCompiler
