{-# Language RecordWildCards #-}

module Cryptographer.Cmd.Decrypt where

import Cryptographer.Common
import Cryptographer.Cmd.Types
import Cryptographer.BaseUtil
import Data.Bits
import qualified Codec.Encryption.Twofish as TF
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Trans.Class
import qualified Data.ByteString as BS
import Cryptographer.BaseUtil
import Control.Monad.Error

cbcDec vi dec = snd . Prelude.foldr cata (vi,[])
  where
    cata b' (vi,bs) = (b', dec b' `xor` vi : bs)

-- encryptTF' k iv = cbcEnc iv $ TF.encrypt (TF.mkStdCipher k)

decryptTF' k iv = cbcDec iv $ TF.decrypt (TF.mkStdCipher k)

decryptCBCGen CC{..} key' BlockEncrypted{..} =
  fromBits dataSize $ cbcDec iV (dec key) encText
  where
    key = hash key'

decryptCBC key ctx' = do
  ctx <-  decodeBase64 ctx' >>= decodeBinary
  case () of
    _ | valid twoFishCipher ctx ->
      return $ decryptCBCGen twoFishCipher key ctx
    _ -> throwError $ OtherError "No suitable cipher found"
