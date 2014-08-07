module Cryptographer.Cmd.Encrypt where

import Codec.Crypto.AES.IO
import Codec.Crypto.AES.Random (prandBytes)
import Control.Monad (liftM)
import Data.ByteString.Base64 (encode, decode)
import Control.Applicative ((<$>))
import Data.ByteString
import Cryptographer.Util (hash)

type IV = ByteString
type Key = ByteString

cryptCtx = newCtx OFB

encrypt :: Key -> ByteString -> IO (IV, ByteString)
encrypt key' content = do
  vi <- prandBytes 16
  ctx <- cryptCtx key vi Encrypt
  (,) (encode vi) <$> liftM encode (crypt ctx content)
  where
    key = hash key'

decrypt :: Key -> IV -> ByteString -> IO ByteString
decrypt key' iv' content' = do
  ctx <- cryptCtx key iv Decrypt
  crypt ctx content
  where
    key = hash key'
    iv = tryDecode iv'
    content = tryDecode content'
    tryDecode str =
      case decode str of
        Right c -> c
        Left errors -> error errors
  
