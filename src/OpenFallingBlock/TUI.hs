module OpenFallingBlock.TUI where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.State
import System.Console.ANSI
import System.IO
import System.Random

import OpenFallingBlock.Game
import OpenFallingBlock.Input
import OpenFallingBlock.Pieces

runTUI :: MonadIO m => m ()
runTUI = do
  chan <- liftIO newTChanIO
  initialise
  liftIO $ forkIO (inputThread chan)
  liftIO $ replicateM 21 (putStrLn "")
  void $ flip runStateT (Game emptyBoard Nothing 0 0) $ forever $ do
    liftIO (atomically (tryReadTChan chan)) >>= \case
      Nothing -> pure ()
      Just i -> runInput i
    runFrame
    liftIO (threadDelay (1000000 `div` 20) )

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
  gameTick

clearBoard :: MonadIO m => m ()
clearBoard = liftIO (cursorUp 21)
