{-# Language OverloadedStrings, TupleSections, MultiWayIf, ScopedTypeVariables, KindSignatures, FlexibleContexts #-}
module Cryptographer.Cmd.Processors where

import Cryptographer.Common
import Cryptographer.BaseUtil
import Data.String (fromString)
import qualified Pipes.ByteString as PB
import Cryptographer.Cmd.Decrypt
import Control.Applicative ((<$>), (<|>), (<*>), pure, (<*))
import Control.Monad.Error
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.LargeWord (Word256)
import Data.List (find)
import GHC.IO.Handle.Types (Handle)
import Pipes.Internal (Proxy)

data InputFields = Type | Value | Id | Checksum deriving (Show, Enum, Eq)

escaped e d = escaped'
  where
    escaped' = do
      r <- option Nothing $ (Just . (1,) <$> e) <|> (Just . (2,) <$> d)
      case r of
        Just (1,r') -> (\n ns -> BS.cons r' $ BS.cons n ns) <$> anyChar8 <*> escaped'
        Just (2,_) -> pure BS.empty
        _ -> BS.cons <$> anyChar8 <*> escaped'

anyChar8 :: Parser PB.Word8
anyChar8 = fromIntegral . fromEnum <$> anyChar

escapedQuote = do
  d <- char '"' <|> char '\''
  escaped (char8 '\\') (char8 d)

inputFieldParser t = do
  typeIdent
  skipSpace
  char '='
  skipSpace
  (t,) <$> escapedQuote
  where
    typeIdent = string $
      case t of
        Type -> "type"
        Value -> "value"
        Id -> "id"
        Checksum -> "checksum"
    endStr e = do
      notChar '\\'
      char e

inputParsers = foldl (<|>) mzero parsers
  where
    parsers = [inputFieldParser e | e <-[Type,Value,Id,Checksum]]

inputParser = do
  char '<'
  skipSpace
  string "input"
  skipSpace
  manyTill (inputParsers <* skipSpace) (char '>')
    
encInputParser = do
  r <- (Just <$> try inputParser) <|> (anyChar8 >> pure Nothing)
  let
    value = 
      r >>= find (== (Id, fromString encTextName))
      >> r >>= find (\(i,_) -> i == Value)
    checksum = r >>= find ((==) Checksum . fst)
  case value of
    Just (_,r') -> return (r',checksum)
    Nothing -> encInputParser

getEncData bs = go $ parse encInputParser bs
  where
    go r = 
      case r of
        Done _ (ct,cs) -> return $ (PB.fromLazy $ BL.fromStrict ct,cs)
        Partial c -> go $ c ""
        Fail _ _ err -> throwError $ XMLParsingError $ show err
    
pDecryptCBC key ctx = PB.fromLazy <$> decryptCBC key ctx

-- decryptFile :: forall (t :: (* -> *) -> * -> *) (m :: * -> *) x' x.
--                  (MonadError Errors (t IO), MonadTrans t, Functor (t IO),
--                   Monad m) =>
--                  BS.ByteString
--                  -> GHC.IO.Handle.Types.Handle
--                  -> t IO (Pipes.Internal.Proxy x' x () BS.ByteString m ())
decryptFile key h = do
  (ctx,checksum) <- lift (BS.hGetContents h) >>= getEncData
  -- pDecryptCBC key $ PB.toLazy ctx
  dec <- pDecryptCBC key $ PB.toLazy ctx
  v <- lift $ (sha256' :: BL.ByteString -> Word256) <$> PB.toLazyM dec -- :: Word256
  case checksum of
    Nothing -> return dec
    Just (_,cs') -> do
      cs <- toW <$> (decodeBase64 $ BL.fromStrict cs')
      if v == cs then
        return dec
      else
        throwError $ DecodeError "Wrong decryption key."
