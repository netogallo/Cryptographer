{-# Language RecordWildCards, ScopedTypeVariables #-}
module Cryptographer.Format where

import System.IO (Handle, openFile, IOMode(..), hClose)
import Control.Monad.State.Strict
import System.Process (terminateProcess, createProcess)
import Cryptographer.Common
import qualified Pipes.Safe as PS
import Control.Applicative ((<$>))
import Data.ByteString.Lazy (ByteString)

data PipeData p =
  -- | Pipe from a file
  FPipe FilePath IOMode (Handle -> p)
  -- | Pipe from a standard input or a pure soure
  | SPipe p
  -- | Pipe from an URL
  | UrlPipe String (Handle -> p)

data EncObject a =
  EncInput {
    dataSources :: [a],
    checksum :: ByteString
    }

instance Functor EncObject where
  fmap f i@(EncInput{..}) = i{dataSources=f `fmap` dataSources}

register (FPipe fp m p') = do
  h <- lift $ openFile fp m
  k <- PS.register $ hClose h
  p <- lift $ p' h
  return (Just k,p)
register (UrlPipe url p')  = do
  hs@(pIn,pOut',pErr,pHandle) <- lift $ createProcess $ wget [url]
  k <- PS.register $ terminateProcess pHandle
  p <- case pOut' of
    Just pOut -> lift $ p' pOut
    Nothing -> fail "Unable to read web resource"
  return (Just k,p)
register (SPipe p') = do
  p <- lift p'
  return (Nothing,p)

runPipes ps' f = PS.runSafeT $ do
  ps'' <- foldM (\s p -> (:s) . snd <$> register p) [] ps'
  lift $ f $ reverse ps''      

