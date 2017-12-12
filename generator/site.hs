{-|
Module: Main
Description: The entry point of the site generator
Copyright: (c) Mihai Maruseac 2017
Stability: experimental
Portability: POSIX

The entry point of the site generator.
-}

import Hakyll

import Rules

main :: IO ()
main = hakyll siteRules
