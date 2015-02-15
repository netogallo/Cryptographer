{-# Language DeriveGeneric, RecordWildCards, ScopedTypeVariables, DeriveDataTypeable #-}
module Cryptographer.Cmd where

import Cryptographer.Cmd.Encrypt (encryptCBCGen)
import Cryptographer.Cmd.Types (twoFishCipher)
import Cryptographer.Cmd.Render (renderIO)
import System.IO (stdout)
import Data.String
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic(..))
import qualified Pipes.ByteString as PB
import qualified Pipes as P
import System.IO (IOMode(..), openFile, hClose)
import Cryptographer.Cmd.Processors (decryptFile)
import Control.Monad.Error
import Cryptographer.Format
import Cryptographer.Util
import qualified System.Console.Haskeline as HL
import System.Console.CmdArgs
import Cryptographer.BaseUtil (sha256',fromBits)
import Data.LargeWord (Word256)
import Control.Exception (bracket)

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
    let writeFile o = renderIO o (encryptCBCGen twoFishCipher k (PB.fromLazy bs)) (fromBits 32 $ [sha256' bs :: Word256])
    case file of
      Nothing -> writeFile stdout
      Just f -> bracket
                (openFile f WriteMode)
                hClose
                writeFile
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
  return s{key=k}

cmdMain :: IO ()
cmdMain = do
  settings <- cmdArgs config
  processSettings settings >>= performEncryption
