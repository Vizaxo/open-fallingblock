module OpenFallingBlock.Game where

import Control.Applicative
import Control.Lens hiding (indices, Empty)
import Data.Array
import Data.Tuple

data Block = Empty | Full
  deriving Show
type Board = Array (Int, Int) Block

width, height :: Int
width = 10
height = 20

emptyBoard :: Board
emptyBoard = listArray ((0,0), (width-1,height-1)) (repeat Empty)

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
printBoard b = unlines $ do y <- [height-1,height-2..0]
                            pure $ do x <- [0..width-1]
                                      pure $ case b ! (x, y) of
                                        Empty -> ' '
                                        Full -> '#'

exampleGame :: Game
exampleGame = Game emptyBoard (Just squareStart)

lockIn :: Game -> Game
lockIn (Game b Nothing) = Game b Nothing
lockIn (Game b (Just p)) = Game b' Nothing where
  b' = array (bounds b) $ do
    bi <- indices b
    let pi = (bimap (subtract (p^.x)) (subtract (p^.y)) bi)
    pure $ if (inRange (bounds (p^.piece)) pi)
      then (bi, (p^.piece) ! pi)
      else (bi, b ! bi)
