{-# Language OverloadedStrings, RecordWildCards #-}
module Cryptographer.Cmd.Render where
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as As
import qualified Cryptographer.Common as C
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.String
import System.IO
import qualified Pipes.ByteString as Pb
import qualified Pipes as P
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Cryptographer.Util
import Cryptographer.Format

data RenderCTX = RenderCTX {
  alljs :: String,
  encText :: BS.ByteString
  }

encTextName = fromString C.encTextName
keyInputName = fromString C.keyInputName
controlsName = fromString C.controlsName
contentName = fromString C.contentName
decryptButtonName = fromString C.decryptButtonName

renderEncObject EncInput{..} = do
  encText <- readPipes dataSources
  return $ H.div $ do
    H.input H.! As.type_ "hidden" H.! As.value (unsafeLazyByteStringValue encText) H.! As.id encTextName
    H.input H.! As.type_ "password" H.! As.id keyInputName
    H.input H.! As.type_ "submit" H.! As.id decryptButtonName H.! As.value "Decrypt"
    H.div "" H.! As.id contentName

renderIO o encText' = do
  f <- P.liftIO C.allJS
  e <- P.liftIO $ renderEncObject $ EncInput [encText']
  withFile f ReadMode $ \h -> do
    let
      out = Pb.toHandle o
      js = Pb.fromHandle h

    let renderer = do
          P.yield "<html><head>"
          P.yield "</head><body>"

          Pb.fromLazy $ renderHtml e
--           Pb.fromLazy . renderHtml $ render RenderCTX{alljs="", encText=encText'}
          P.yield "<script type=\"text/javascript\">"
          js
          P.yield "</script>"
          Pb.fromLazy . renderHtml $ H.script "window.onload = function(){h$main(h$mainZCMainzimain);}" H.! As.type_ "text/javacript"
          P.yield "</body></html>"
    P.runEffect (renderer P.>-> out)
