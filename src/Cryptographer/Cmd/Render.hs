{-# Language OverloadedStrings, RecordWildCards #-}
module Cryptographer.Cmd.Render where
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as As
import qualified Cryptographer.Common as C
import Data.ByteString (ByteString(..), hGetContents)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.String
import System.IO hiding (hGetContents)

data RenderCTX = RenderCTX {
  alljs :: ByteString,
  encText :: ByteString
  }

encTextName = fromString C.encTextName
keyInputName = fromString C.keyInputName
controlsName = fromString C.controlsName
contentName = fromString C.contentName
decryptButtonName = fromString C.decryptButtonName

htmlHead RenderCTX{..} = H.head $ do
  H.script (unsafeByteString alljs) H.! As.type_ "text/javascript"

htmlBody RenderCTX{..} = H.body $ do
  H.input H.! As.type_ "hidden" H.! As.value (unsafeByteStringValue encText) H.! As.id encTextName
  (H.div H.! As.id controlsName) $ do
    H.input H.! As.type_ "text" H.! As.id keyInputName
  H.div "" H.! As.id contentName

render c = H.html $ htmlHead c >> htmlBody c

renderIO encText' = do
  f <- C.allJS
  withFile f ReadMode $ \h -> do
    js <- hGetContents h
    return . renderHtml $ render RenderCTX{alljs=js, encText=encText'}


  
    
