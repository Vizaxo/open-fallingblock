module OpenFallingBlock.Pieces where

import Control.Lens hiding (Empty)
import Data.Array

data Block = Empty | Full
  deriving (Eq, Show)

type Piece = Array (Int, Int) Block

ih, iv :: Piece
ih = listArray ((-2,0), (1,0)) [Full,  Full,  Full,  Full]
iv = listArray ((0,-1), (0,2)) [Full,  Full,  Full,  Full]
i :: LivePiece
i = LivePiece [ih, iv] 0 5 19

o1 :: Piece
o1 = listArray ((-1,-1), (0,0)) [ Full,  Full , Full,  Full]
o :: LivePiece
o = LivePiece [o1] 0 5 19

zh, zv :: Piece
zh = listArray ((-1,-1), (1,0)) [Empty, Full, Full, Full, Full, Empty]
zv = listArray ((0,-1), (1,1)) [Full, Full, Empty, Empty, Full, Full]
z :: LivePiece
z = LivePiece [zh, zv] 0 5 19

sh, sv :: Piece
sh = listArray ((-1,-1), (1,0)) [Full, Empty, Full, Full, Empty, Full]
sv = listArray ((0,-1), (1,1)) [Empty, Full, Full, Full, Full, Empty]
s :: LivePiece
s = LivePiece [sh, sv] 0 5 19

td, tu, tl, tr  :: Piece
td = listArray ((-1,-1), (1,0)) [Empty, Full, Full, Full, Empty, Full]
tu = listArray ((-1,0), (1,1)) [Full, Empty, Full, Full, Full, Empty]
tl = listArray ((-1,-1), (0,1)) [Empty, Full, Empty, Full, Full, Full]
tr = listArray ((0,-1), (1,1)) [Full, Full, Full, Empty, Full, Empty]
t :: LivePiece
t = LivePiece [td, tl, tu, tr] 0 5 19

ld, ll, lu, lr :: Piece
ld = listArray ((-1,-1), (1,0)) [Full, Full, Empty, Full, Empty, Full]
ll = listArray ((-1,-1), (0,1)) [Empty, Empty, Full, Full, Full, Full]
lu = listArray ((-1,0), (1,1)) [Full, Empty, Full, Empty, Full, Full]
lr = listArray ((0,-1), (1,1)) [Full, Full, Full, Full, Empty, Empty]
l :: LivePiece
l = LivePiece [ld, ll, lu, lr] 0 5 19

jd, jl, ju, jr :: Piece
jd = listArray ((-1,-1), (1,0)) [Empty, Full, Empty, Full, Full, Full]
jl = listArray ((-1,-1), (0,1)) [Full, Empty, Empty, Full, Full, Full]
ju = listArray ((-1,0), (1,1)) [Full, Full, Full, Empty, Full, Empty]
jr = listArray ((0,-1), (1,1)) [Full, Full, Full, Empty, Empty, Full]
j :: LivePiece
j = LivePiece [jd, jl, ju, jr] 0 5 19

pieces :: [LivePiece]
pieces = [i, o, s, z, t, l, j]

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
