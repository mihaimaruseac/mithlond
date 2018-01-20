{-|
Module: Main
Description: The entry point of the site generator
Copyright: (c) Mihai Maruseac 2018
Stability: experimental
Portability: POSIX

The entry point of the site generator.
-}

import Hakyll

import Rules

main :: IO ()
main = hakyll siteRules
-- TODO: validate HTML5 output, once pushed
-- TODO: in site's footer add link to a README which explains blog's
-- technology and is online accessible
