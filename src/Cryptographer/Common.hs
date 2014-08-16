{-# Language CPP #-}
module Cryptographer.Common where

#ifdef CABAL
import Paths_cryptographer
import qualified Paths_cryptographer as P
import Data.Version

allJS = getDataFileName "all.js"

version :: [Int]
version =  versionBranch P.version
#else
allJS = undefined

version :: [Int]
version =  undefined

#endif

encTextName = "encText"
keyInputName = "keyInput"
controlsName = "controls"
contentName = "content"
decryptButtonName = "decryptInput"
