module OpenFallingBlock.Game where

import Control.Applicative
import Control.Lens hiding (indices, Empty)
import Data.Array
import Data.Tuple

data Block = Empty | Full
  deriving (Eq, Show)
type Board = Array (Int, Int) Block

width, height :: Int
width = 10
height = 20

boardBounds :: ((Int, Int), (Int, Int))
boardBounds = ((0,0), (width-1,height-1))

emptyBoard :: Board
emptyBoard = listArray boardBounds (repeat Empty)

type Piece = Array (Int, Int) Block

line :: Piece
line = listArray ((-2,0), (1,0)) [Full,  Full,  Full,  Full]
lineStart :: LivePiece
lineStart = LivePiece line 5 19

square :: Piece
square = listArray ((-1,-1), (0,0)) [ Full,  Full
                                    , Full,  Full]
squareStart :: LivePiece
squareStart = LivePiece square 5 19


data LivePiece = LivePiece
  { _piece :: Piece
  , _x :: Int
  , _y :: Int
  }
  deriving Show
makeLenses ''LivePiece

data Game = Game
  { _board :: Board
  , _active :: Maybe LivePiece
  }
  deriving Show
makeLenses ''Game

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
  pure $ if (inRange (bounds (p^.piece)) pi)
    then (bi, (p^.piece) ! pi)
    else (bi, b ! bi)

overlaps :: LivePiece -> Board -> Bool
overlaps p b = or $ do
  bi <- indices b
  case b ! bi of
    Empty -> pure False
    Full -> do
      let pi = (bimap (subtract (p^.x)) (subtract (p^.y)) bi)
      pure $ if (inRange (bounds (p^.piece)) pi)
        then (p^.piece) ! pi == Full
        else False

inBounds :: LivePiece -> Bool
inBounds p = all (inRange boardBounds . bimap (+p^.x) (+p^.y)) (indices (p^.piece))
