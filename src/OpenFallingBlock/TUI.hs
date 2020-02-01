module OpenFallingBlock.TUI where

import Control.Concurrent
import Control.Lens
import Control.Monad.State
import System.IO
import System.Random

import OpenFallingBlock.Game
import OpenFallingBlock.Pieces

main :: MonadIO m => m ()
main = do
  initialise
  void $ flip runStateT (Game emptyBoard Nothing) $ forever $ do
    mainLoop
    liftIO (threadDelay 100000)

initialise :: MonadIO m => m ()
initialise = liftIO (hSetBuffering stdin NoBuffering)

mainLoop :: (MonadIO m, MonadState Game m) => m ()
mainLoop = do
  b <- gets (^. board)
  a <- gets (^. active)
  liftIO $ putStrLn $ printBoard $ case a of
    Nothing -> b
    Just p -> lockIn p b
  case a of
    Nothing -> modify . set active . Just =<< nextPiece
    Just p -> do
      let p' = down p
      if not (overlaps p' b) && inBounds p'
        then modify (set active (Just p'))
        else do modify (over board (lockIn p))
                modify (set active Nothing)

nextPiece :: MonadIO m => m LivePiece
nextPiece = do
  i <- liftIO (randomRIO (0, length pieces - 1))
  pure (pieces !! i)
