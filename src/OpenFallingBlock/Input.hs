module OpenFallingBlock.Input where

import Control.Lens
import Control.Monad.State

import OpenFallingBlock.Game
import OpenFallingBlock.Pieces

data Input = U | D | L | R | A | B

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
