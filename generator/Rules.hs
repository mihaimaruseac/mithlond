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
  match "index.html" $ indexRules
  match "posts/*" $ postRules
  match "templates/*" $ templateRules
  match "css/*" $ cssRules
  match "javascript/*" $ idRules
  match "images/*" $ idRules
  match "fonts/*" $ idRules

indexRules :: Rules ()
indexRules = route idRoute >> compile indexCompiler

cssRules :: Rules ()
cssRules = route idRoute >> compile compressCssCompiler

idRules :: Rules ()
idRules = route idRoute >> compile copyFileCompiler

templateRules :: Rules ()
templateRules = compile templateCompiler

postRules :: Rules ()
postRules = do
  route $ setExtension "html"
  compile postCompiler
