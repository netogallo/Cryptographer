module Cryptographer.Util where

import Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import GHC.Word (Word8)
import Data.Digest.Pure.SHA as S
import Crypto.Cipher.Types as BF (makeIV,IV,BlockCipher)
import Control.Applicative
import Data.ByteString.Base64 (decode)

hash :: ByteString -> ByteString
hash = pack . BL.unpack . bytestringDigest . S.sha256 . BL.pack . unpack

tryDecode str =
  case decode str of
    Right c -> c
    Left errors -> error errors

fromEither e =
  case e of
    Right r -> r
    Left e -> error $ show e

fromJust a opt =
  case opt of
    Just o -> o
    Nothing -> a

randIV :: BlockCipher c => IO (BF.IV c, ByteString)
randIV = do
  (Just iv) <- return $ makeIV bs
  return (iv,bs)
  where
    bs = BS.replicate 8 1

a >?> b = do
  a' <- a
  case a' of
    Nothing -> return Nothing
    Just a'' -> b a''

