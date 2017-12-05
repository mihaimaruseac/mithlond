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
siteRules =
  match "posts/*" $ postRules

postRules :: Rules ()
postRules = do
  route $ setExtension "html"
  compile postCompiler
