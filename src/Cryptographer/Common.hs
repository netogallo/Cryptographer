{-# Language CPP #-}
module Cryptographer.Common where

import Control.Monad.Error
#ifdef CABAL
import Paths_cryptographer
import qualified Paths_cryptographer as P
import Data.Version
import System.Process

allJS = getDataFileName "all.js"

version :: [Int]
version =  versionBranch P.version

wget fs = (proc "wget" $ ["-qO-"] ++ fs){std_out = CreatePipe}
#else
allJS = undefined

version :: [Int]
version =  undefined

#endif

type CryptError m = ErrorT Errors m

data Errors =
  DecodeError String
  | UnrecognizedCipher String
  | XMLParsingError String
  | OtherError String deriving Show

instance Error Errors where
  noMsg = OtherError ""
  strMsg _ = OtherError ""

encTextName = "encText"
keyInputName = "keyInput"
controlsName = "controls"
contentName = "content"
decryptButtonName = "decryptInput"
