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
import Control.Monad.Trans (lift)
import Cryptographer.Util
import Cryptographer.Cmd.Encrypt (decrypt)
import GHCJS.DOM.HTMLInputElement (htmlInputElementGetValue)
import Data.ByteString.Char8 (pack, unpack)
import GHCJS.DOM.Element (elementOnclick)

contentName = "content"
keyInputName = "keyInput"
decryptButtonName = "decryptInput"
ivInputName = "ivInput"
dataInputName = "dataInput"
              
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

ivInput = fmap castToHTMLInputElement . pageElement ivInputName

dataInput = fmap castToHTMLInputElement . pageElement dataInputName

contentDecrypt webUi = do
  key <- pack <$> (keyInput webUi >>= htmlInputElementGetValue)
  iv <- pack <$> (ivInput webUi >>= htmlInputElementGetValue)
  val <- pack <$> (dataInput webUi >>= htmlInputElementGetValue)
  c <- contentDiv webUi
  dec <- decrypt key iv val
  htmlElementSetInnerHTML c (unpack dec)

mainGui webUi = do
  c <- contentDiv webUi
  b <- decryptButton webUi
  elementOnclick b (lift $ contentDecrypt webUi)
  htmlElementSetInnerHTML c "<p>GHCJS</p>"
  return ()

webMain = runWebGUI mainGui
