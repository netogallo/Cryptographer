{-# Language ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
module Cryptographer.Util where

import Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import GHC.Word (Word8)
import qualified Data.Digest.Pure.SHA as S
import Crypto.Cipher.Types as BF (makeIV,IV,BlockCipher)
import Control.Applicative
import Data.ByteString.Base64 (decode)
import Data.Bits
import Data.LargeWord
import Data.Binary (decodeOrFail)
import Crypto.Random
import qualified Pipes as P
import qualified Pipes.Prelude as Pr
import Data.Maybe (fromJust)
import Control.Monad
import Data.Either

sha256 :: ByteString -> ByteString
sha256 = pack . BL.unpack . S.bytestringDigest . S.sha256 . BL.pack . unpack

lz = BL.pack . unpack

tryDecode str =
  case decode str of
    Right c -> c
    Left errors -> error errors

fromEither e =
  case e of
    Right r -> r
    Left e -> error $ show e

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

toBits :: forall w m a . (FiniteBits w, Num w, Monad m) => ByteString -> P.Producer w m ()
toBits bs' = P.for (ext l $ bs bs') (\b -> P.yield $ mkBits b 0)  
  where
    bs str
      | BS.length str > r = do
        P.yield $ BS.take r str
        bs $ BS.drop r str
      | otherwise = P.yield str
    ext i p = do
      (v,p') <- either (const (e,p)) id <$> P.next p
      P.yield v
      unless (i <= 0) $ ext (i-1) p'
    e = BS.pack $ Prelude.take r $ repeat 0
    s = finiteBitSize (undefined :: w)
    b = finiteBitSize (undefined :: Word8)
    r = s `div` b
    l = 1 + (BS.length bs' * b) `div` s

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

safeDecode d =
  case decodeOrFail d of
    Right (_,_,v) -> Right v
    Left (_,_,e) -> Left e

randomBS :: Int -> IO ByteString
randomBS s = 
  rnd <$> newGenIO
  where
    rnd r =
      case genBytes s (r :: SystemRandom) of
        Right bs -> fst bs
        Left _ -> error $ "Random Gen failed with length: " ++ show s

randomW128 :: IO Word128
randomW128 = either undefined fst <$> (P.next $ do
  r <- P.lift (randomBS 128)
  toBits r)
