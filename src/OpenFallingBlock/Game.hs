module OpenFallingBlock.Game where

import Control.Applicative
import Control.Lens hiding (indices, Empty)
import Control.Monad.State
import Data.Tuple
import Data.Array
import Data.Word
import System.Random

import OpenFallingBlock.Pieces

type Board = Array (Int, Int) Block

data Game = Game
  { _board :: Board
  , _active :: Maybe LivePiece
  , _frame :: Int
  , _platformTicks :: Word64}
  deriving Show
makeLenses ''Game

width, height :: Int
width = 10
height = 20

boardBounds :: ((Int, Int), (Int, Int))
boardBounds = ((0,0), (width-1,height-1))

emptyBoard :: Board
emptyBoard = listArray boardBounds (repeat Empty)

printBoard :: Board -> String
printBoard b = unlines $ do
  y <- [height-1,height-2..0]
  pure $ do
    x <- [0..width-1]
    pure $ case b ! (x, y) of
      Empty -> ' '
      Full -> '#'

lockIn :: LivePiece -> Board -> Board
lockIn p b = array (bounds b) $ do
  bi <- indices b
  let pi = (bimap (subtract (p^.x)) (subtract (p^.y)) bi)
  pure $ if (inRange (bounds (piece p)) pi)
    then case piece p ! pi of
           Full -> (bi, Full)
           Empty -> (bi, b ! bi)
    else (bi, b ! bi)

overlaps :: LivePiece -> Board -> Bool
overlaps p b = or $ do
  bi <- indices b
  case b ! bi of
    Empty -> pure False
    Full -> do
      let pi = (bimap (subtract (p^.x)) (subtract (p^.y)) bi)
      pure $ if (inRange (bounds (piece p)) pi)
        then piece p ! pi == Full
        else False

inBounds :: LivePiece -> Bool
inBounds p = all (inRange boardBounds . bimap (+p^.x) (+p^.y)) (indices (piece p))

rotateR, rotateL, up, down, left, right :: LivePiece -> LivePiece
rotateR = over rot (+1)
rotateL = over rot (subtract 1)
up = over y (+1)
down = over y (subtract 1)
left = over x (subtract 1)
right = over x (+1)

nextPiece :: MonadIO m => m LivePiece
nextPiece = do
  i <- liftIO (randomRIO (0, length pieces - 1))
  pure (pieces !! i)

gameTick :: (MonadIO m, MonadState Game m) => m ()
gameTick = do
  b <- gets (^. board)
  a <- gets (^. active)
  case a of
    Nothing -> modify . set active . Just =<< nextPiece
    Just p -> do
      let p' = down p
      if not (overlaps p' b) && inBounds p'
        then modify (set active (Just p'))
        else do modify (over board (lockIn p))
                modify (set active Nothing)
  modify (over frame (+1))
