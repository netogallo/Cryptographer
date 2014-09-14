{-# Language DeriveGeneric, RecordWildCards, ScopedTypeVariables #-}
module Cryptographer.Cmd where

import Cryptographer.Cmd.Encrypt (encryptCBCGen)
import Cryptographer.Cmd.Types (twoFishCipher)
import Cryptographer.Cmd.Render (renderIO)
import System.Environment (getArgs)
import Control.Applicative ((<$>))
import System.IO (stdout, hPutStrLn, stderr)
import Data.String
import Data.Maybe (fromMaybe)
import System.Console.CmdArgs.Generic (kwargs, getBuilders)
import GHC.Generics (Generic(..))
import qualified Pipes.ByteString as PB
import qualified Pipes as P
import System.IO (IOMode(..))
import Cryptographer.Cmd.Processors (decryptFile)
import Control.Monad (liftM)
import Control.Monad.Error
import Cryptographer.Format
import Cryptographer.Util
import qualified System.Console.Haskeline as HL

data Settings a = S{

  key :: a,
  docKey :: Maybe String,
  append :: Maybe String
  } deriving (Generic)

fromMaybeM a m =
  case m of
    Just x -> return x
    Nothing -> a

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

getPassword :: IO String
getPassword = do
  p <- HL.runInputT HL.defaultSettings $ HL.getPassword (Just '*') "Encryption key:"
  case p of
    Nothing -> getPassword
    Just p' -> return p'

cmdMain :: IO ()
cmdMain = do
  settings <- kwargs getBuilders <$> getArgs
  case settings of
    Right s -> do
      performEncryption s
    Left e -> hPutStrLn stderr (concat e)
