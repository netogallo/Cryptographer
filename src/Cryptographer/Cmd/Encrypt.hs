{-# Language ScopedTypeVariables, MultiWayIf, RecordWildCards, GADTs #-}
module Cryptographer.Cmd.Encrypt where

import Control.Monad (liftM)
import Data.ByteString.Base64.Lazy as BE
import Control.Applicative ((<$>), (<*>))
import Data.ByteString as BS
import qualified Codec.Encryption.Twofish as TF
import Cryptographer.Util
import qualified Cryptographer.Common as C
import qualified Crypto.Cipher.Types as BF
import Data.Bits as BT
import Data.Binary as BI
import Data.LargeWord (Word256, Word128)
import Debug.Trace (trace)
import Data.Either (either)
import qualified Pipes as P
import qualified Pipes.Prelude as Pr
import Control.Monad.Identity (runIdentity)
import Pipes.Core
import qualified Data.ByteString.Lazy as BL

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
  hash = \bs -> (runIdentity $ (either undefined fst <$> hash bs) :: Word256),
  enc = \k -> TF.encrypt (TF.mkStdCipher k),
  dec = \k -> TF.decrypt (TF.mkStdCipher k),
  randIV = randomW128,
  buildData = tfBuild,
  valid = tfValid
  }
  where
    hash bs = P.next $ toBits $ sha256 bs
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

cbcEnc vi enc = cata vi
  where
--    cata b' = yield $ enc (vi `xor` b')
    cata b' =
      let
        b = (enc (b `xor` b'))
      in P.yield b >> cata b

cbcDec vi dec = snd . Prelude.foldr cata (vi,[])
  where
    cata b' (vi,bs) = (b', dec b' `xor` vi : bs)

encryptTF' k iv = cbcEnc iv $ TF.encrypt (TF.mkStdCipher k)

decryptTF' k iv = cbcDec iv $ TF.decrypt (TF.mkStdCipher k)

encryptCBCGen :: forall k w . (FiniteBits w, Num w, Binary w) => CryptographerCipher k w -> ByteString -> ByteString -> IO BL.ByteString
encryptCBCGen CC{..} key' text' = do
  iv <- randIV
  let
    enc = BI.encode $ buildData CBC (BS.length text') iv $ ct iv
  return $ BE.encode enc
  where
    key = hash key'
    text :: Monad m => Producer w m ()
    text = toBits text'
    ct iv = Pr.toList $ text P.>-> cbcEnc iv (enc key)

decryptCBCGen CC{..} key' BlockEncrypted{..} =
  fromBits dataSize $ cbcDec iV (dec key) encText
  where
    key = hash key'

decryptCBC key ctx' = do
  ctx <- BE.decode (lz ctx') >>= safeDecode
  case () of
    _ | valid twoFishCipher ctx ->
      return $ decryptCBCGen twoFishCipher key ctx
