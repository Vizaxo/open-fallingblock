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

lineHoriz, lineVert :: Piece
lineHoriz = listArray ((-2,0), (1,0)) [Full,  Full,  Full,  Full]
lineVert = listArray ((0,-1), (0,2)) [Full,  Full,  Full,  Full]
line :: LivePiece
line = LivePiece [lineHoriz, lineVert] 0 5 19

square' :: Piece
square' = listArray ((-1,-1), (0,0)) [ Full,  Full
                                    , Full,  Full]
square :: LivePiece
square = LivePiece [square'] 0 5 19


data LivePiece = LivePiece
  { _rotations :: [Piece]
  , _rot :: Int
  , _x :: Int
  , _y :: Int
  }
  deriving Show
makeLenses ''LivePiece

piece :: LivePiece -> Piece
piece (LivePiece rotations rot _ _) = rotations !! mod rot (length rotations)

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
  pure $ if (inRange (bounds (piece p)) pi)
    then (bi, piece p ! pi)
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
