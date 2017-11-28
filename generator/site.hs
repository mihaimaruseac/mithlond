import Hakyll

import GHC.IO.Encoding
  (setLocaleEncoding, setForeignEncoding, utf8, setFileSystemEncoding)

main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  hakyllWith siteConfiguration siteRules

siteConfiguration :: Configuration
siteConfiguration = defaultConfiguration

siteRules :: Rules ()
siteRules = undefined
