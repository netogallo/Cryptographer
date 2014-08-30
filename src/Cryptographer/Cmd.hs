{-# Language DeriveGeneric #-}
module Cryptographer.Cmd (cmdMain) where

import Cryptographer.Cmd.Encrypt (encryptCBCGen, twoFishCipher)
import Cryptographer.Cmd.Render (renderIO)
import System.Environment (getArgs)
import Control.Applicative ((<$>))
import Data.ByteString (hGetContents, pack)
import qualified Data.ByteString.Lazy as BL
import System.IO (stdin, stdout, hPutStrLn, stderr)
import Data.String
import System.Console.CmdArgs.Generic (kwargs, getBuilders)
import GHC.Generics
import Cryptographer.Cmd.Processors

data Settings = S{

  key :: String,
  extraKey :: Maybe String,
  append :: Maybe String
  } deriving (Generic)
             
cmdMain = do
  settings <- kwargs getBuilders <$> getArgs
  case settings of
    Right s -> do
      text <- hGetContents stdin
      encryptCBCGen twoFishCipher (fromString $ key s) text >>= renderIO stdout . pack . BL.unpack
    Left e -> hPutStrLn stderr (concat e)
