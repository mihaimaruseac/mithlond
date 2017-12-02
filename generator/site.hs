{-|
Module: Main
Description: The entry point of the site generator
Copyright: (c) Mihai Maruseac 2017
Stability: experimental
Portability: POSIX

The entry point of the site generator.
-}

import GHC.IO.Encoding
    (setFileSystemEncoding, setForeignEncoding, setLocaleEncoding, utf8)

import Hakyll

import Rules

main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  hakyll siteRules
