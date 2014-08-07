module Cryptographer.Cmd.Encrypt where

import Control.Monad (liftM)
import Data.ByteString.Base64 (encode, decode)
import Control.Applicative ((<$>))
import Data.ByteString as BS
import Cryptographer.Util (hash, randIV, fromJust, tryDecode, fromEither)
import Crypto.Cipher.Blowfish
import qualified Crypto.Cipher.Types as BF

type IV = ByteString
type Key = ByteString

addPadding blockSize bs 
  | BS.length bs `div` blockSize == 0 = (bs,0)
  | otherwise = (BS.concat [bs,BS.replicate e 0], e)
  where
    e = BS.length bs `mod` blockSize
    
                                   
encrypt :: Key -> ByteString -> IO (IV, ByteString)
encrypt key' content' = do
  (vi,bs) <- randIV
  let
    e = BF.cfbEncrypt cipher vi content
  return (encode bs, encode e)
  where
    (content,_) = addPadding (BF.blockSize cipher) content'
    cipher = BF.cipherInit key
    key :: BF.Key Blowfish256
    key = case BF.makeKey $ hash key' of
      Right k -> k
      Left e -> error $ "Failed to create key: " ++ show e
               

decrypt :: Key -> IV -> ByteString -> ByteString
decrypt key' iv' content' = BF.cfbDecrypt cipher iv content
  where
    cipher = BF.cipherInit key
    key :: BF.Key Blowfish256
    key = fromEither $ BF.makeKey $ hash key'
    iv = fromJust (error "Invalid initialization vector")
         $ BF.makeIV (tryDecode iv')
    content = tryDecode content'  
