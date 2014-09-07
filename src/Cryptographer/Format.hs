{-# Language RecordWildCards, ScopedTypeVariables #-}
module Cryptographer.Format where

import System.IO (Handle, openFile, IOMode(..), hClose)
import Control.Monad.State.Class as MS
import Control.Monad.State.Strict
import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Exception

data PipeData p =
  FPipe FilePath IOMode (Handle -> p)
  | SPipe p

data EncObject a =
  EncInput {dataSources :: [a]}

instance Functor EncObject where
  fmap f i@(EncInput{..}) = i{dataSources=f `fmap` dataSources}

runPipes :: forall a a1. [PipeData (IO a1)] -> ([a1] -> IO a) -> IO a
runPipes ps' f = flip evalStateT [] $ do
  ps'' <- foldM cata (Right []) ps'
  res <- lift $ case ps'' of
    Right ps -> (try $ f $ reverse ps :: IO (Either IOError a))
    Left e -> throw e
  closeHandles
--  return res
  case res of
     Right v -> return v
     Left e -> throw e

  where
    closeHandle h = do
      _ <- lift $ ((try $ hClose h) :: IO (Either IOError ()))
      return ()
    closeHandles = get >>= mapM_ (closeHandle)
    cata (Left e) _ = return $ Left e
    cata (Right s) v =
      case v of
        FPipe fi m p -> do
          h' <- lift $ (try $ openFile fi m :: IO (Either IOError Handle))
          case h' of
            Right h -> do
              modify (h:)
              p' <- lift $ p h
              return $ Right $ p' : s
            Left e ->
              return $ Left e
        SPipe p -> do
          p' <- lift p
          return $ Right $ p' : s
