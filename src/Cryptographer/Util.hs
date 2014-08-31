{-# Language ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module Cryptographer.Util where

import Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import GHC.Word (Word8)
import qualified Data.Digest.Pure.SHA as S
import Control.Applicative
import Data.ByteString.Base64 (decode)
import Data.Bits
import Data.LargeWord
import Data.Binary (decodeOrFail)
import Crypto.Random
import qualified Pipes as P
import Control.Monad
import Control.Monad.Trans.State.Strict as Ms
import Debug.Trace (trace)

sha256 :: ByteString -> ByteString
sha256 = pack . BL.unpack . S.bytestringDigest . S.sha256 . BL.pack . unpack

reducePipe :: Monad m => (s -> v -> s) -> s -> P.Producer v m r -> m (r,s)
reducePipe f s pr'' = Ms.runStateT (cata pr'') s
  where
    cata pr = do
      n <- P.lift $ P.next pr
      case n of
        Left r -> return r
        Right (v, pr') -> do
          trace "w2" $ modify (flip f v)
          cata pr'

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

bytes :: (Monad m) => P.Proxy () ByteString () Word8 (StateT Int m) ()
bytes = do
  bs <- P.await
  P.lift $ modify (+ BS.length bs)
  BS.foldl (\s b -> s >> P.yield b) (return ()) bs

bits :: forall w a m . (FiniteBits w, Num w, Monad m) => P.Producer Word8 m a -> P.Producer w m a
bits p = do
  (s,p',done) <- foldM cata (0,p,Right ()) $ Prelude.replicate chunk ()
  trace "w0" $ P.yield s
  case done of
    Left a -> return a
    Right () -> bits p'

  where
    cata (s,pr,_) _ = do
      eByte <- P.lift $ P.next pr
      let (b,pr',done) = case eByte of
            Left a -> (0,pr,Left a)
            Right (b',pr'') -> (b',pr'',Right ())
      return (rotateL s bSize .|. toW b,pr',done)
      
    wSize = finiteBitSize (undefined :: w)
    bSize = finiteBitSize (undefined :: Word8)
    chunk = wSize `div` bSize
    toW :: Word8 -> w
    toW = fromInteger . toInteger

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


pRandomBs :: Int -> P.Producer ByteString (StateT Int IO) ()
pRandomBs l = forever $ do
  bs <- P.liftIO $ randomBS l
  P.yield bs
  pRandomBs l

-- randomW128 :: P.Producer Word128 (StateT Int IO) ()
randomW :: (FiniteBits w, Num w) => IO w
randomW = evalStateT
  (either undefined fst <$> (P.next $ (bits $ pRandomBs 1024 P.>-> bytes)))
  0
  
randomW128 = randomW :: IO Word128
