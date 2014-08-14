module Cryptographer.Cmd (cmdMain, encrypt) where

import Cryptographer.Cmd.Encrypt (encrypt)
import Cryptographer.Cmd.Render (renderIO)

cmdMain = (return () :: IO ())
