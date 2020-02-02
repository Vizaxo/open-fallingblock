module OpenFallingBlock.TUI where

import Control.Concurrent
import Control.Lens
import Control.Monad.State
import System.Console.ANSI
import System.IO
import System.Random

import OpenFallingBlock.Game
import OpenFallingBlock.Pieces

data Game = Game
  { _board :: Board
  , _active :: Maybe LivePiece
  , _frame :: Int
  }
  deriving Show
makeLenses ''Game

main :: MonadIO m => m ()
main = do
  initialise
  liftIO $ replicateM 21 (putStrLn "")
  void $ flip runStateT (Game emptyBoard Nothing 0) $ forever $ do
    runFrame
    liftIO (threadDelay (1000000 `div` 60) )

initialise :: MonadIO m => m ()
initialise = do
  liftIO (hSetBuffering stdin NoBuffering)
  liftIO hideCursor

runFrame :: (MonadIO m, MonadState Game m) => m ()
runFrame = do
  b <- gets (^. board)
  a <- gets (^. active)
  clearBoard
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
  modify (over frame (+1))

clearBoard :: MonadIO m => m ()
clearBoard = liftIO (cursorUp 21)

nextPiece :: MonadIO m => m LivePiece
nextPiece = do
  i <- liftIO (randomRIO (0, length pieces - 1))
  pure (pieces !! i)
