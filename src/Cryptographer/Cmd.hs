{-# Language DeriveGeneric #-}
module Cryptographer.Cmd (cmdMain) where

import Cryptographer.Cmd.Encrypt (encryptCBCGen, twoFishCipher)
import Cryptographer.Cmd.Render (renderIO)
import System.Environment (getArgs)
import Control.Applicative ((<$>))
import System.IO (stdin, stdout, hPutStrLn, stderr)
import Data.String
import System.Console.CmdArgs.Generic (kwargs, getBuilders)
import GHC.Generics
import qualified Pipes.ByteString as PB

data Settings = S{

  key :: String,
  extraKey :: Maybe String,
  append :: Maybe String
  } deriving (Generic)
             
cmdMain = do
  settings <- kwargs getBuilders <$> getArgs
  case settings of
    Right s -> do
      renderIO stdout $ encryptCBCGen twoFishCipher (fromString $ key s) PB.stdin
    Left e -> hPutStrLn stderr (concat e)
