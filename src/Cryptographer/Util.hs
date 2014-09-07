{-# Language ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module Cryptographer.Util where

import Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import GHC.Word (Word8)
import Control.Applicative
import Data.ByteString.Base64 (decode)
import Data.Bits
import qualified Pipes as P
import qualified Pipes.Parse as Pa
import Control.Monad
import Control.Monad.Trans.State.Strict as Ms
import Cryptographer.BaseUtil
import qualified Pipes.ByteString as Pb
import Debug.Trace
import qualified Pipes.Prelude as Pre

readPipes :: Monad m => [P.Producer Pb.ByteString m ()] -> m BL.ByteString
readPipes = foldM cata BL.empty
  where
    cata s p = do
      bs <- Pb.toLazyM p
      return $ BL.concat [s,bs]

reducePipe :: Monad m => (s -> v -> s) -> s -> P.Producer v m r -> m (r,s)
reducePipe f s pr'' = Ms.runStateT (cata pr'') s
  where
    cata pr = do
      n <- P.lift $ P.next pr
      case n of
        Left r -> return r
        Right (v, pr') -> do
          modify (flip f v)
          cata pr'

tryDecode str =
  case decode str of
    Right c -> c
    Left errors -> error errors

fromEither e =
  case e of
    Right r -> r
    Left e -> error $ show e
    
bytes :: (Monad m) => P.Proxy () ByteString () Word8 (StateT Int m) ()
bytes = do
  bs <- P.await
  P.lift $ modify (+ BS.length bs)
  P.each $ BS.unpack bs
  bytes

bits :: forall w a m . (FiniteBits w, Num w, Monad m, Functor m) => P.Producer Word8 m a -> P.Producer w m ()
bits pr = do
  ((end,r), pr') <- P.lift $ Pa.runStateT (Prelude.foldl cata (False,0) <$> replicateM chunk Pa.draw) $ pr
  P.yield r
  unless end $ bits pr'

  where
    cata (_,s) x' =
      let (end,x) = case x' of
            Nothing -> (True,0)
            Just a-> (False,trace (show a) a)
      in (end, shiftL s bSize .|. fromIntegral x)
      
    wSize = finiteBitSize (undefined :: w)
    chunk = wSize `div` bSize
