{-|
Module: Patterns
Description: Patterns used for matching in rules and loading contexts
Copyright: (c) Mihai Maruseac 2022
Stability: experimental
Portability: POSIX

Patterns used for matching in rules and loading contexts. Needed because
contexts sometimes need to load resources from some patterns, in order to
follow the DRY principle.
-}

{-# LANGUAGE OverloadedStrings #-}

module Patterns where

import Hakyll

patternIndex :: Pattern
patternIndex = "content/index.html"

patternPosts :: Pattern
patternPosts = fromRegex "content/posts/-?[0-9]+/.*\\.md$"

patternTemplates :: Pattern
patternTemplates = "content/templates/*"

patternCSS :: Pattern
patternCSS = "content/css/*"

patternFonts :: Pattern
patternFonts = "content/fonts/*.woff"
