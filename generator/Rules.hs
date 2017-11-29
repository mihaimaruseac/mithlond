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

-- TODO: restrict imports
import Control.Applicative
import Data.Maybe

import Hakyll

-- | The rules used to build the site
siteRules :: Rules ()
siteRules = do
  match "index.html" $ indexRules
  match "posts/*" $ postRules
  match "templates/*" $ compile templateCompiler
  match "css/*" $ cssRules
  match "javascript/*" $ idRules
  match "images/*" $ idRules
  match "fonts/*" $ idRules

indexRules :: Rules ()
indexRules = do
  route idRoute
  compile indexCompiler

indexCompiler :: Compiler (Item String)
indexCompiler =
  getResourceBody >>=
  applyAsTemplate indexCtx >>=
  loadAndApplyTemplate "templates/default-chp.html" postCtxChp >>=
  relativizeUrls
  where
    indexCtx = field "posts" $ \_ -> postList recentFirst

postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
  posts <- sortFilter =<< loadAll ("posts/*" .&&. hasNoVersion)
  itemTpl <- loadBody "templates/post-item.html"
  list <- applyTemplateList itemTpl postCtx posts
  return list

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

-- TODO: style, remove duplicates
postCtxChp :: Context String
postCtxChp = mconcat
  [ sourceField "source"
  , htmlTitleField
  , dateField "date" "%F"
  , bodyField "body"
  , betterTitleField
  , defaultContext
  , constField "tags" ""
  , missingField
  ]

-- TODO: style
sourceField :: String -> Context String
sourceField key = field key $ fmap (maybe empty (sourceUrl . toUrl)) . getRoute . itemIdentifier
  where
    sourceUrl xs = (take (length xs - 4) xs) ++ "md"

-- TODO: style
htmlTitleField :: Context String
htmlTitleField = Context $ \k _ i -> 
  if (k /= "htmltitle")
  then do empty
  else do value <- getMetadataField (itemIdentifier i) "title"
          return $ StringField (if isNothing value then "" else fromJust value)

-- TODO: style
betterTitleField :: Context String
betterTitleField = Context $ \k _ i -> 
  if (k /= "title")
  then do empty
  else do value <- getMetadataField (itemIdentifier i) "title"
          return $ StringField (mathdocInline $ if isNothing value then "" else fromJust value)
  where
    mathdocInline = id -- TODO


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
