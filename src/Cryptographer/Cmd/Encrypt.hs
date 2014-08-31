{-# Language ScopedTypeVariables, MultiWayIf, RecordWildCards, GADTs #-}
module Cryptographer.Cmd.Encrypt where

import Data.ByteString.Base64.Lazy as BE
import Control.Applicative ((<$>), (<*>))
import Data.ByteString as BS
import qualified Codec.Encryption.Twofish as TF
import Cryptographer.Util
import qualified Cryptographer.Common as C
import Data.Bits as BT
import Data.Binary as BI
import qualified Pipes as P
import qualified Pipes.Prelude as Pr
import Pipes.Core
import qualified Data.ByteString.Lazy as BL
import qualified Control.Monad.Trans.State.Strict as S
import Data.LargeWord (Word256)
import Pipes.Binary as Pb
import Pipes.Lift as Pl
import Debug.Trace (trace)

type IV = ByteString
type Key = ByteString

data BlockCipher =
  Blowfish256
  | TwoFish256
  deriving (Show, Enum)

data Mode = CBC deriving (Enum, Show)

type CipherText t = [t]

instance Binary BlockCipher where
  put = put . fromEnum
  get = toEnum <$> get

instance Binary Mode where
  put = put . fromEnum
  get = toEnum <$> get

data BlockEncrypted b = BlockEncrypted {
  iV :: b,
  cipher :: BlockCipher,
  version :: [Int],
  mode :: Mode,
  encText :: CipherText b,
  dataSize :: Int
  } deriving Show

instance Binary b => Binary (BlockEncrypted b) where
  put BlockEncrypted{..} = do
    put iV
    put cipher
    put version
    put mode
    put encText
    put dataSize
  get = BlockEncrypted <$> get <*> get <*> get <*> get <*> get <*> get

data CryptographerCipher k b where
  CC :: (FiniteBits b) => {
    hash :: ByteString -> k,
    enc :: k -> b -> b,
    dec :: k -> b -> b,
    randIV :: IO b,
    buildData :: Mode -> Int -> b -> CipherText b -> BlockEncrypted b,
    valid :: BlockEncrypted b -> Bool
    } -> CryptographerCipher k b

twoFishCipher = CC {
  hash = \bs -> S.evalState (either undefined fst <$> hash bs) 0 :: Word256,
  enc = \k -> TF.encrypt (TF.mkStdCipher k),
  dec = \k -> TF.decrypt (TF.mkStdCipher k),
  randIV = randomW128,
  buildData = tfBuild,
  valid = tfValid
  }
  where
    hash bs = P.next $ bits $ P.yield (sha256 bs) P.>-> bytes      
    tfBuild m size b ct =
      BlockEncrypted {
        iV = b,
        cipher = TwoFish256,
        version = C.version,
        mode = m,
        encText = ct,
        dataSize = size
        }
    tfValid BlockEncrypted{..} =
      case cipher of
        TwoFish256 -> True
        _ -> False

cbcEnc :: (FiniteBits b, Monad m, Show b) => (b -> b) -> b -> S.StateT b m b -- -> P.Proxy () b () b (S.StateT b m) ()
cbcEnc enc b = do
  b' <- S.get
  let b'' = enc (b' `xor` b)
  trace (show b') $ S.put b''
  return b''
  -- P.yield b

cbcDec vi dec = snd . Prelude.foldr cata (vi,[])
  where
    cata b' (vi,bs) = (b', dec b' `xor` vi : bs)

-- encryptTF' k iv = cbcEnc iv $ TF.encrypt (TF.mkStdCipher k)

decryptTF' k iv = cbcDec iv $ TF.decrypt (TF.mkStdCipher k)

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
  encoded P.>-> Pr.map (\s -> let r = BE.encode $ BL.fromStrict s in trace (show r) r) P.>-> (P.await >>= P.each . BL.toChunks)
  where
    key = hash key'
    text :: Producer w (S.StateT Int IO) ()
    text = bits $ text' P.>-> bytes
    crypt :: w -> IO (Int,[w])
    crypt iv =
      let pr = Pl.execStateP 0 $ text P.>-> Pl.evalStateP iv (Pr.mapM (cbcEnc (enc key)))
      in reducePipe (\s v -> let x = (s ++ [v]) in trace (show x) x ) [] pr

decryptCBCGen CC{..} key' BlockEncrypted{..} =
  fromBits dataSize $ cbcDec iV (dec key) encText
  where
    key = hash key'

decryptCBC key ctx' = do
  ctx <- BE.decode (lz ctx') >>= safeDecode
  case () of
    _ | valid twoFishCipher ctx ->
      return $ decryptCBCGen twoFishCipher key ctx
