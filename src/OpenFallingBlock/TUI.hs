module OpenFallingBlock.TUI where

import Control.Concurrent
import Control.Concurrent.STM
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

data Input = U | D | L | R | A | B

main :: MonadIO m => m ()
main = do
  chan <- liftIO newTChanIO
  initialise
  liftIO $ forkIO (inputThread chan)
  liftIO $ replicateM 21 (putStrLn "")
  void $ flip runStateT (Game emptyBoard Nothing 0) $ forever $ do
    liftIO (atomically (tryReadTChan chan)) >>= \case
      Nothing -> pure ()
      Just i -> runInput i
    runFrame
    liftIO (threadDelay (1000000 `div` 20) )

tryAction :: (MonadState Game m) => (LivePiece -> LivePiece) -> m ()
tryAction f = do
  b <- gets (^. board)
  gets (^. active) >>= \case
    Nothing -> pure ()
    Just a ->
      let a' = f a
      in if not (overlaps a' b) && inBounds a'
      then modify (set active (Just a'))
      else pure ()

runInput :: (MonadIO m, MonadState Game m) => Input -> m ()
runInput i = tryAction (action i) where
  action U = up
  action D = down
  action L = left
  action R = right
  action A = rotateR
  action B = rotateL

charToInput :: Char -> Maybe Input
charToInput 'a' = Just L
charToInput 'e' = Just R
charToInput ',' = Just U
charToInput 'o' = Just D
charToInput 'h' = Just B
charToInput 't' = Just A
charToInput _ = Nothing

inputThread :: TChan Input -> IO ()
inputThread chan = forever $ do
  charToInput <$> getChar >>= \case
    Nothing -> pure ()
    Just i -> atomically (writeTChan chan i)

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
