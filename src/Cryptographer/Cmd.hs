module Cryptographer.Cmd (cmdMain, encryptTF) where

import Cryptographer.Cmd.Encrypt (encryptTF)
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
  encryptTF key text >>= renderIO stdout . pack . BL.unpack
