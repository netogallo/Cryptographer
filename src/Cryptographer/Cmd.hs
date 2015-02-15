{-# Language DeriveGeneric, RecordWildCards, ScopedTypeVariables, DeriveDataTypeable #-}
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
import qualified Data.ByteString as BS
import System.Console.CmdArgs
import Cryptographer.BaseUtil (sha256',fromBits)
import Data.LargeWord (Word256)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Debug.Trace (trace)

data Settings a  = S{
  key :: a,
  docKey :: Maybe String,
  append :: Maybe String,
  file :: Maybe String
  } deriving (Generic, Data, Typeable)

config = S{
  key = def,
  docKey = def,
  append = def,
  file = def
  } :: Settings (Maybe String)

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
    bs <- readPipes ps
    renderIO stdout (encryptCBCGen twoFishCipher k (PB.fromLazy bs)) (fromBits 32 $ [sha256' bs :: Word256])

  where
    k = fromString $ fromMaybe key docKey
    prevPipe = processAppend k append

getPassword :: Bool -> IO String
getPassword False = do
  p <- HL.runInputT HL.defaultSettings $ HL.getPassword (Just '*') "Encryption key:"
  return (Nothing, p)
  case p of
    Nothing -> fail "Unable to read the password" -- getPassword
    Just p' -> return p'

processSettings s = do
  k <- fromMaybeM (getPassword False) $ key s
  let o = case file s of
        Nothing -> SPipe $ (return PB.stdout :: IO (P.Proxy () BS.ByteString () BS.ByteString IO ()))
        Just f -> FPipe (fromString f) WriteMode $ return . PB.fromHandle
  return s{key=k}

cmdMain :: IO ()
cmdMain = do
  settings <- cmdArgs config --kwargs getBuilders <$> getArgs
  processSettings settings >>= performEncryption
  -- case settings of
  --   Right s -> do
  --     processSettings s >>= performEncryption
  --   Left e -> hPutStrLn stderr (concat e)
