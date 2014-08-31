{-# Language OverloadedStrings, RecordWildCards #-}
module Cryptographer.Cmd.Render where
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as As
import qualified Cryptographer.Common as C
import qualified Data.ByteString as BS
import Data.String
import System.IO
import qualified Pipes.ByteString as Pb
import qualified Pipes as P
import qualified Pipes.Prelude as Pr
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Control.Applicative ((<$>))

data RenderCTX = RenderCTX {
  alljs :: String,
  encText :: BS.ByteString
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
    H.input H.! As.type_ "submit" H.! As.id decryptButtonName H.! As.value "Decrypt"
  H.div "" H.! As.id contentName

render c = H.html $ htmlBody c

renderIO o encText'' = do
  f <- P.liftIO C.allJS
  encText' <- BS.concat <$> Pr.toListM encText''
  withFile f ReadMode $ \h -> do
    let
      out = Pb.toHandle o
      js = Pb.fromHandle h

    let renderer = do
          P.yield "<html><head><script type=\"text/javascript\">"
          js
          P.yield "</script></head>"
          Pb.fromLazy . renderHtml $ render RenderCTX{alljs="", encText=encText'}
          P.yield "</html>"
    P.runEffect (renderer P.>-> out)
