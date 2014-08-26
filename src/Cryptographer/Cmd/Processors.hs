{-# Language OverloadedStrings #-}
module Cryptographer.Cmd.Processors where

import Cryptographer.Common
import Data.List (find)
import Data.Maybe (isJust)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor
import Data.String (fromString)
import Data.Text.Encoding (encodeUtf8)

getEncData d = encDataTag $ fromDocument d
  where
    matchEncTxt = element "input" >=> attributeIs "id" (fromString encTextName)
    encDataTag ns =
      case ns $// matchEncTxt of
        [] -> error "Not a valid Cryptographer file!"
        c:_ -> encodeUtf8 . head $ attribute "value" c
           
