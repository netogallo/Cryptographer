{-# Language OverloadedStrings, TupleSections, MultiWayIf #-}
module Cryptographer.Cmd.Processors where

import Cryptographer.Common
import Data.String (fromString)
import qualified Pipes.ByteString as PB
import Cryptographer.Cmd.Decrypt
import Control.Applicative ((<$>), (<|>), (<*>), pure, (<*))
import Control.Monad.Error
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (find)

data InputFields = Type | Value | Id deriving (Show, Enum, Eq)

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
    endStr e = do
      notChar '\\'
      char e

inputParsers = foldl (<|>) mzero parsers
  where
    parsers = [inputFieldParser e | e <- enumFrom Type]

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
  case value of
    Just (_,r') -> return r'
    Nothing -> encInputParser

getEncData bs = do
  case parse encInputParser bs of
    Done _ ct -> return $ PB.fromLazy $ BL.fromStrict ct
    Fail _ _ err -> throwError $ XMLParsingError err
    
pDecryptCBC key ctx = PB.fromLazy <$> decryptCBC key ctx

decryptFile key h = do
  ctx <- lift (BS.hGetContents h) >>= getEncData
  pDecryptCBC key $ PB.toLazy ctx
