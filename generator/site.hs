{-|
Module: Main
Description: The entry point of the site generator
Copyright: (c) Mihai Maruseac 2020
Stability: experimental
Portability: POSIX

The entry point of the site generator.

This file just creates the `main` entry point by delegating to Hakyll's one
(`hakyll`). The only changed parts are the site rules which are defined
elsewhere.
-}

import Hakyll

import Rules

main :: IO ()
main = hakyll siteRules
