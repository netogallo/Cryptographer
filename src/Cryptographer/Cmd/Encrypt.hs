{-# Language ScopedTypeVariables, MultiWayIf, RecordWildCards #-}
module Cryptographer.Cmd.Encrypt where

import Data.ByteString.Base64.Lazy as BE
import Control.Applicative ((<$>))
import Data.ByteString as BS
import qualified Codec.Encryption.Twofish as TF
import Cryptographer.Util
import Data.Bits as BT
import Data.Binary as BI
import qualified Pipes as P
import qualified Pipes.Prelude as Pr
import Pipes.Core
import qualified Data.ByteString.Lazy as BL
import qualified Control.Monad.Trans.State.Strict as S
import Pipes.Binary as Pb
import Pipes.Lift as Pl
import Cryptographer.Cmd.Types


cbcEnc :: (FiniteBits b, Monad m, Show b) => (b -> b) -> b -> S.StateT b m b
cbcEnc enc b = do
  b' <- S.get
  let b'' = enc (b' `xor` b)
  S.put b''
  return b''

encryptCBCGen :: forall k w . (FiniteBits w, Num w, Binary w, Show w) =>
                 CryptographerCipher k w ->
                 ByteString ->
                 P.Producer ByteString (S.StateT Int IO) () ->
                 P.Producer ByteString IO ()
encryptCBCGen CC{..} key' text' = do
  iv <- P.lift $ randIV
  (l,ct) <- P.lift $ crypt iv
  let
    encoded = Pb.encode $ buildData CBC l iv (Prelude.reverse ct)
  encoded P.>-> Pr.map (\s -> BE.encode $ BL.fromStrict s) P.>-> (P.await >>= P.each . BL.toChunks)
  where
    key = hash key'
    text :: Producer w (S.StateT Int IO) ()
    text = bits $ text' P.>-> bytes
    crypt :: w -> IO (Int,[w])
    crypt iv =
      let pr = Pl.execStateP 0 text P.>-> Pl.evalStateP iv (Pr.mapM (cbcEnc (enc key)))
      in reducePipe (\s v -> (s ++ [v])) [] pr
