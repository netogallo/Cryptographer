{-# Language DeriveGeneric, RecordWildCards, ScopedTypeVariables #-}
module Cryptographer.Cmd (cmdMain) where

import Cryptographer.Cmd.Encrypt (encryptCBCGen)
import Cryptographer.Cmd.Types (twoFishCipher)
import Cryptographer.Cmd.Render (renderIO)
import System.Environment (getArgs)
import Control.Applicative ((<$>))
import System.IO (stdout, hPutStrLn, stderr)
import Data.String
import System.Console.CmdArgs.Generic (kwargs, getBuilders)
import GHC.Generics (Generic(..))
import qualified Pipes.ByteString as PB
import qualified Pipes as P
import System.IO (IOMode(..))
import Data.Maybe (fromMaybe)
import Cryptographer.Cmd.Processors (decryptFile)
import Control.Monad.Error
import Cryptographer.Format
import Cryptographer.Util

data Settings = S{

  key :: String,
  docKey :: Maybe String,
  append :: Maybe String
  } deriving (Generic)

errorRunner p = do
  p' <- runErrorT p
  case p' of
    Right p'' -> return p''
    Left e -> fail $ show e

processAppend k append =
  case append of
    Nothing -> SPipe $ return $ P.each []
    Just f | isUrl $ fromString f ->
      UrlPipe f $ \h -> errorRunner $ decryptFile k h
    Just f -> FPipe f ReadMode $ \h -> errorRunner $ decryptFile k h

performEncryption S{..} = do
  runPipes [prevPipe, SPipe $ return PB.stdin] $ \ps -> do
    bs <- PB.fromLazy <$> readPipes ps
    renderIO stdout $ encryptCBCGen twoFishCipher k bs

  where
    k = fromString $ fromMaybe key docKey
    prevPipe = processAppend k append

cmdMain :: IO ()
cmdMain = do
  settings <- kwargs getBuilders <$> getArgs
  case settings of
    Right s -> performEncryption s
    Left e -> hPutStrLn stderr (concat e)
