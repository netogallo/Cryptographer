{-# Language ScopedTypeVariables #-}
module Cryptographer.Util where

import Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import GHC.Word (Word8)
import Data.Digest.Pure.SHA as S
import Crypto.Cipher.Types as BF (makeIV,IV,BlockCipher)
import Control.Applicative
import Data.ByteString.Base64 (decode)
import Data.Bits
import Data.LargeWord

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

class RandIV iv where
  randIV :: IO iv

instance BlockCipher c => RandIV (BF.IV c, ByteString) where
  randIV = randIVBC

instance RandIV Word128 where
  randIV = return 9

randIVBC :: BlockCipher c => IO (BF.IV c, ByteString)
randIVBC = do
  (Just iv) <- return $ makeIV bs
  return (iv,bs)
  where
    bs = BS.replicate 8 1

a >?> b = do
  a' <- a
  case a' of
    Nothing -> return Nothing
    Just a'' -> b a''

mkBits :: (Num b, FiniteBits b) => ByteString -> b -> b
mkBits bs w
  | BS.length bs == 1 = word
  | BS.length bs > 1 = mkBits (BS.tail bs) (rotateL word (finiteBitSize byte))
  where
    byte = BS.head bs
    word = w .|. fromIntegral byte

toBits :: forall w . (FiniteBits w, Num w) => ByteString -> [w]
toBits bs' = Prelude.reverse . snd $ Prelude.foldl cata (bs',[]) res'
  where
    s = finiteBitSize (undefined :: w)
    b = finiteBitSize (undefined :: Word8)
    r = s `div` b
    l = 1 + (BS.length bs' * b) `div` s
    res' = [1 .. l]
    cata (bs, ws) _
      | BS.length bs > r = (BS.drop r bs, mkBits (BS.take r bs) 0 : ws)
      | otherwise = (bs, mkBits bs 0 : ws)

mkBs :: (Integral w, FiniteBits w) => w -> ByteString -> ByteString
mkBs wi = snd . BS.mapAccumR cata wi
  where
    chopper = fromIntegral $ complement (0 :: Word8)
    cata w b = (rotateR w (finiteBitSize b), fromIntegral (w .&. chopper))

fromBits :: forall w . (FiniteBits w, Integral w) => Int -> [w] -> ByteString
fromBits _ [] = BS.empty
fromBits l (b:bs)
  | l > r = BS.append (mkBs b (BS.replicate r 0)) $ fromBits (l-r) bs
  | otherwise = mkBs b (BS.replicate l 0)
  where
    bl = finiteBitSize (undefined :: Word8)
    r = finiteBitSize b `div` bl

-- toBits bs 
