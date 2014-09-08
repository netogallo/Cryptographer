module Cryptographer.Web.Utils.Dom where

import Control.Applicative ((<$>))
import GHCJS.DOM (
  runWebGUI,
  postGUISync,
  postGUIAsync,
  webViewGetDomDocument)
import GHCJS.DOM.Document (
  documentCreateElement, 
  documentGetElementById,
  documentGetBody)
import GHCJS.DOM.HTMLElement (
  htmlElementSetInnerText,
  htmlElementSetInnerHTML)
import GHCJS.DOM.Types(
  Node(..),
  castToHTMLElement,
  castToHTMLDivElement,
  castToHTMLInputElement)
import Control.Monad.Trans.Class (lift)
import Cryptographer.BaseUtil
import Cryptographer.Cmd.Decrypt (decryptCBC)
import GHCJS.DOM.HTMLInputElement (htmlInputElementGetValue)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy.Char8 (unpack)
import GHCJS.DOM.Element (elementOnclick)
import Cryptographer.Common
import Data.String
import Control.Monad.Error
import Control.Monad.Identity

pageElement name webUi = do
  c <- webViewGetDomDocument webUi
       >?> flip documentGetElementById name
  case c of
    Nothing -> fail $ "The dom element "
                     ++ name
                     ++ " is missing."
    Just e -> return e

content = pageElement contentName

contentDiv webUi = fmap castToHTMLDivElement $ content webUi

decryptButton = fmap castToHTMLInputElement . pageElement decryptButtonName

keyInput = fmap castToHTMLInputElement . pageElement keyInputName

encText = fmap castToHTMLInputElement . pageElement encTextName

contentDecrypt webUi = do
  key <- fromString <$> (keyInput webUi >>= htmlInputElementGetValue)
  val <- fromString <$> (encText webUi >>= htmlInputElementGetValue)
  c <- contentDiv webUi
  let
    dec' = runIdentity . runErrorT $ decryptCBC key val
  case dec' of
    Right dec -> htmlElementSetInnerHTML c (unpack dec)
    Left e -> error $ show e

mainGui webUi = do
  c <- contentDiv webUi
  b <- decryptButton webUi
  elementOnclick b (lift $ contentDecrypt webUi)
  htmlElementSetInnerHTML c "<p>GHCJS</p>"
  return ()

webMain = runWebGUI mainGui
