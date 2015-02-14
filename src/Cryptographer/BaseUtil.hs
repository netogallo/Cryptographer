-- Contains utilities that are known to work with GHCJS
{-# Language ScopedTypeVariables #-}
module Cryptographer.BaseUtil where

import qualified Data.Digest.Pure.SHA as S
import qualified Data.ByteString.Lazy as BL
import Data.Bits
import GHC.Word
import qualified Data.ByteString as B
import Crypto.Random
import Control.Applicative ((<$>))
import Data.Binary (decodeOrFail)
import Cryptographer.Common
import Control.Monad.Error
import Data.ByteString.Base64.Lazy as BE

a >?> b = do
  a' <- a
  case a' of
    Nothing -> return Nothing
    Just a'' -> b a''

bSize :: Int
bSize = finiteBitSize (undefined :: Word8)

toW :: (FiniteBits w, Num w) => BL.ByteString -> w
toW = BL.foldl cata 0
  where
    cata w b = shiftL w bSize .|. fromIntegral b

randomBS :: Int -> IO B.ByteString
randomBS s = 
  rnd <$> newGenIO
  where
    rnd r =
      case genBytes s (r :: SystemRandom) of
        Right bs -> fst bs
        Left _ -> error $ "Random Gen failed with length: " ++ show s

randomW :: forall w . (FiniteBits w, Num w) => IO w
randomW = toW . BL.fromStrict <$> randomBS len
  where
    len = (finiteBitSize (undefined :: w)) `div` bSize
    
sha256 :: (FiniteBits w, Num w) => B.ByteString -> w
sha256 = toW . S.bytestringDigest . S.sha256 . BL.fromStrict

sha256' :: (FiniteBits w, Num w) => BL.ByteString -> w
sha256' = toW . S.bytestringDigest . S.sha256

decodeBase64 x =
  case BE.decode x of
    Left e -> throwError $ DecodeError e
    Right v -> return v

decodeBinary d =
  case decodeOrFail d of
    Right (_,_,v) -> return v
    Left (_,_,e) -> throwError $ DecodeError e

mkBs :: forall w . (Integral w, FiniteBits w) => w -> B.ByteString -> B.ByteString
mkBs wi = snd . B.mapAccumL cata (rotateL wi bSize)
  where
    chopper = fromIntegral $ complement (0 :: Word8)
    cata w b = (rotateL w (finiteBitSize b), fromIntegral (w .&. chopper))
    wSize = finiteBitSize (undefined :: w)

fromBits :: (FiniteBits w, Integral w) => Int -> [w] -> BL.ByteString
fromBits l = BL.fromChunks . fromBits' l . reverse

fromBits' :: forall w . (FiniteBits w, Integral w) => Int -> [w] -> [B.ByteString]
fromBits' _ [] = [B.empty]
fromBits' l (b:bs)
  | l > r = (mkBs b (B.replicate r 0)) : fromBits' (l-r) bs
  | otherwise = mkBs b (B.replicate l 0) : []
  where
    bl = finiteBitSize (undefined :: Word8)
    r = finiteBitSize b `div` bl
