module Cryptographer.Cmd (cmdMain) where

import Cryptographer.Cmd.Encrypt (encryptCBCGen, twoFishCipher)
import Cryptographer.Cmd.Render (renderIO)
import System.Environment (getArgs)
import Control.Applicative ((<$>))
import Data.ByteString (hGetContents, pack)
import qualified Data.ByteString.Lazy as BL
import System.IO (stdin, stdout, hPutStrLn)
import Data.String

cmdMain = do
  key <- fromString . head <$> getArgs
  text <- hGetContents stdin
  encryptCBCGen twoFishCipher key text >>= renderIO stdout . pack . BL.unpack
