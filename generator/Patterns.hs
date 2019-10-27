{-|
Module: Patterns
Description: Patterns used for matching in rules and loading contexts
Copyright: (c) Mihai Maruseac 2020
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
patternIndex = "index.html"

patternPosts :: Pattern
patternPosts = fromRegex "^posts/-?[0-9]+/.*\\.md$"

patternTemplates :: Pattern
patternTemplates = "templates/*"

patternCSS :: Pattern
patternCSS = "css/*"

patternFonts :: Pattern
patternFonts = "fonts/*.woff"
