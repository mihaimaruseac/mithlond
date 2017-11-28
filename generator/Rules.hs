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

-- | The rules used to build the site
siteRules :: Rules ()
siteRules = do
  -- match index.html
  match "posts/*" $ postRules
  match "templates/*" $ compile templateCompiler
  match "css/*" $ cssRules
  match "javascript/*" $ idRules
  match "images/*" $ idRules
  match "fonts/*" $ idRules

cssRules :: Rules ()
cssRules = do
  route idRoute
  compile compressCssCompiler

idRules :: Rules ()
idRules = do
  route idRoute
  compile copyFileCompiler

postRules :: Rules ()
postRules = do
  route $ setExtension "html"
  compile postCompiler

postCompiler :: Compiler (Item String)
postCompiler =
  pandocCompilerWith defaultHakyllReaderOptions defaultHakyllWriterOptions >>=
  loadAndApplyTemplate "templates/post.html" postCtx >>=
  saveSnapshot "teaser" >>=
  loadAndApplyTemplate "templates/post-default.html" postDataCtx >>=
  relativizeUrls

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

postDataCtx :: Context String
postDataCtx = mconcat [blogCtx, authorCtx, mathCtx, defaultContext]

-- TODO: getMetadata $ itemIdentifier item
blogCtx :: Context a
blogCtx = mconcat
  [ field "blog_title" $ \_ -> return "blog title"
  , field "blog_description" $ \_ -> return "blog description"
  , field "about_page_link" $ \_ -> return "about page link"
  , field "banner_image_link" $ \_ -> return "banner image link"
  , field "root_url" $ \_ -> return "root url"
  ]

authorCtx :: Context a
authorCtx = mconcat
  [ field "author_name" $ \_ -> return "Mihai Maruseac"
  , field "author_intro" $ \_ -> return "bla bla"
  ]

-- TODO: style
mathCtx :: Context a
mathCtx = field "mathjax" $ \item -> do
  metadata <- getMetadata $ itemIdentifier item
  return $ case "mathjax" `lookupString` metadata of
    Just _ -> "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
    Nothing -> ""
