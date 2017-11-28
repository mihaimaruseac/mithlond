{-|
Module: Main
Description: The entry point of the site generator
Copyright: (c) Mihai Maruseac 2017
Stability: experimental
Portability: POSIX

The entry point of the site generator.
-}

import Hakyll

import GHC.IO.Encoding
  (setLocaleEncoding, setForeignEncoding, utf8, setFileSystemEncoding)

import Rules

main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  hakyllWith siteConfiguration siteRules

siteConfiguration :: Configuration
siteConfiguration = defaultConfiguration
