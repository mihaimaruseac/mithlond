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
-- TODO: in site's footer: Source available on <a href=TODO>GitHub</a> together with <a href=TODO>copyright</a>.
-- TODO: choose one of the above
-- TODO: change stability
-- TODO: new domain, new name, move repo
-- TODO: HTML5 doctor
-- TODO: check responsiveness and disable paddings for small screens
-- TODO: for responsiveness should also change point size to still keep same alphabet size on a line of text
-- TODO: handle overflow
-- TODO: CSS for code styling
-- TODO: separate page for comic? or separate repo?
-- TODO: decide if we want to separate sections or use white background and no
-- rounded corners. Maybe use one for HCAR and one for the other?
-- TODO: speaking of which, this should be easy to extend to HCAR output and
-- this includes markers for section novelty too
