{-# Language CPP, ForeignFunctionInterface #-}
module Cryptographer.Util where

import Data.ByteString
import qualified Data.ByteString.Lazy as BL
import GHC.Word (Word8)
import Data.Digest.Pure.SHA as S

hash :: ByteString -> ByteString
hash = pack . BL.unpack . bytestringDigest . S.sha256 . BL.pack . unpack

a >?> b = do
  a' <- a
  case a' of
    Nothing -> return Nothing
    Just a'' -> b a''

