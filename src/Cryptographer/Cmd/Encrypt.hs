{-# Language ScopedTypeVariables, MultiWayIf, RecordWildCards #-}
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

data BlockEncrypted b = BlockEncrypted {
  iV :: b,
  encText :: [b]
  }

data SizeBlockEncrypted b = SizeBlockEncrypted {
  block :: BlockEncrypted b,
  dataSize :: Int
  }

type TwoFishEncrypted = SizeBlockEncrypted Word128

instance Binary b => Binary (BlockEncrypted b) where
  put BlockEncrypted{..} = put iV >> put encText
  get = BlockEncrypted <$> get <*> get

instance Binary b => Binary (SizeBlockEncrypted b) where
  put SizeBlockEncrypted{..} = put block >> put dataSize
  get = SizeBlockEncrypted <$> get <*> get

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
  let enc = BI.encode $ SizeBlockEncrypted (BlockEncrypted iv ct) (BS.length text')
  return $ BE.encode enc
  where
    key :: Word256
    key = Prelude.head . toBits $ hash key'
    text = toBits text'

decryptTF key' encCtx' =
  fromBits (dataSize encCtx) $ decryptTF' key (iV$block encCtx) (encText$block encCtx)
  where
    encCtx =
      case BE.decode encCtx' >>= BI.decode of
        Right e -> e
        Left s -> error  s
    key :: Word256
    key = Prelude.head $ toBits key'
  
addPadding blockSize bs 
  | BS.length bs `div` blockSize == 0 = (bs,0)
  | otherwise = (BS.concat [bs,BS.replicate e 0], e)
  where
    e = BS.length bs `mod` blockSize

encrypt :: Cipher -> Key -> ByteString -> IO (IV, ByteString)
encrypt = undefined 

decrypt key ctx' = decryptTF key ctx
  where
    ctx = case BE.decode ctx' >>= BI.decode  of
      Right d -> d
      Left e -> error e

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
