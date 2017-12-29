{-|
Module: Rules
Description: The rules used to generate various pages on the site.
Copyright: (c) Mihai Maruseac 2017
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
  --match "posts/*" $ postRules
  match (fromRegex "^posts/[0-9]+/main.md$") $ postRules
  match "templates/*" $ templateRules

postRules :: Rules ()
postRules = do
  route $ setExtension "html"
  compile postCompiler

-- | Rules to compile templates
-- Should just use the default, all is good.
templateRules :: Rules ()
templateRules = compile templateCompiler
