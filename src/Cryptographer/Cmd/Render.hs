{-# Language OverloadedStrings, RecordWildCards #-}
module Cryptographer.Cmd.Render where
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as As
import qualified Cryptographer.Common as C
import Data.ByteString (ByteString(..))
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.String
import System.IO

data RenderCTX = RenderCTX {
  alljs :: String,
  encText :: ByteString
  }

encTextName = fromString C.encTextName
keyInputName = fromString C.keyInputName
controlsName = fromString C.controlsName
contentName = fromString C.contentName
decryptButtonName = fromString C.decryptButtonName

htmlHead RenderCTX{..} = H.head $ do
  H.script (fromString alljs) H.! As.type_ "text/javascript"

htmlBody RenderCTX{..} = H.body $ do
  H.input H.! As.type_ "hidden" H.! As.value (unsafeByteStringValue encText) H.! As.id encTextName
  (H.div H.! As.id controlsName) $ do
    H.input H.! As.type_ "text" H.! As.id keyInputName
  H.div "" H.! As.id contentName

render c = H.html $ htmlBody c

renderIO o encText' = do
  f <- C.allJS
  withFile f ReadMode $ \h -> do
    js <-  hGetContents h
    let out = renderHtml $ render RenderCTX{alljs=js, encText=encText'}
    hPutStrLn o "<html><head><script type=\"text/javascript\">"
    hPutStrLn o js
    hPutStrLn o "</head>"
    hPutStrLn o out
    hPutStrLn o "</html>"
    hFlush o

  
    
