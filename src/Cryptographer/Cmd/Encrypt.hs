{-# Language ScopedTypeVariables, MultiWayIf #-}
module Cryptographer.Cmd.Encrypt where

import Control.Monad (liftM)
import Data.ByteString.Base64.Lazy as BE
import Control.Applicative ((<$>), (<*>))
import Data.ByteString as BS
import qualified Codec.Encryption.Twofish as TF
import Cryptographer.Util
import qualified Crypto.Cipher.Types as BF
import Data.Bits as BI
import Data.Binary as BI
import Data.LargeWord (Word256, Word128)

type IV = ByteString
type Key = ByteString

data Cipher =
  Blowfish256Cipher
  | TwoFish256Cipher
  deriving Show

data Encrypted =
  TwoFish256Enc {tfSize :: Int, tfIV :: Word128, tfData :: [Word128]}

instance Binary Encrypted where
  put (TwoFish256Enc s iv d) = do
    put $ show TwoFish256Cipher
    put s
    put iv
    put d
  get = do
    h <- get
    case () of
      _ | h == show TwoFish256Cipher ->
        TwoFish256Enc <$> get <*> get <*> get      

cbcEnc vi enc = Prelude.foldr cata []
  where
    cata b' [] = [enc (vi `xor` b')]
    cata b' (b:bs) = enc (b `xor` b') : b : bs

cbcDec vi dec = snd . Prelude.foldr cata (vi,[])
  where
    cata b' (vi,bs) = (b', dec b' `xor` vi : bs)

encryptTF' k iv = cbcEnc iv $ TF.encrypt (TF.mkStdCipher k)

decryptTF' k iv = cbcDec iv $ TF.decrypt (TF.mkStdCipher k)

encryptTF key' text' = do
  iv <- randIV
  let ct = encryptTF' key iv text
  let enc = BI.encode $ TwoFish256Enc (BS.length text') iv ct
  return $ BE.encode enc
  where
    key :: Word256
    key = Prelude.head . toBits $ hash key'
    text = toBits text'
  
addPadding blockSize bs 
  | BS.length bs `div` blockSize == 0 = (bs,0)
  | otherwise = (BS.concat [bs,BS.replicate e 0], e)
  where
    e = BS.length bs `mod` blockSize

encrypt :: Cipher -> Key -> ByteString -> IO (IV, ByteString)
encrypt = undefined 
        
-- encryptGen :: forall c . BF.BlockCipher c => c -> Key -> ByteString -> IO (IV, ByteString)
-- encryptGen _ key' content' = do
--   (vi,bs) <- randIV
--   let
--     e = BF.cfbEncrypt cipher vi content
--   return (BE.encode bs, BE.encode e)
--   where
--     (content,_) = addPadding (BF.blockSize cipher) content'
--     cipher = BF.cipherInit key
--     key :: BF.Key c
--     key = case BF.makeKey $ hash key' of
--       Right k -> k
--       Left e -> error $ "Failed to create key: " ++ show e
               

-- decrypt :: forall c . BF.BlockCipher c => c -> Key -> IV -> ByteString -> ByteString 
-- decrypt _ key' iv' content' = BF.cfbDecrypt cipher iv content
--   where
--     cipher = BF.cipherInit key
--     key :: BF.Key c
--     key = fromEither $ BF.makeKey $ hash key'
--     iv = fromJust (error "Invalid initialization vector")
--          $ BF.makeIV (tryDecode iv')
--     content = tryDecode content'  
