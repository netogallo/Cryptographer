{-# Language RecordWildCards, GADTs #-}
module Cryptographer.Cmd.Types where
import Data.Binary
import Data.ByteString
import Control.Applicative ((<$>), (<*>))
import Data.Bits
import qualified Cryptographer.Common as C
import qualified Codec.Encryption.Twofish as TF
import Cryptographer.BaseUtil
import Data.LargeWord

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

twoFishCipher :: CryptographerCipher Word256 Word128
twoFishCipher = CC {
  hash = sha256,
  enc = \k -> TF.encrypt (TF.mkStdCipher k),
  dec = \k -> TF.decrypt (TF.mkStdCipher k),
  randIV = randomW,
  buildData = tfBuild,
  valid = tfValid
  }
  where
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
