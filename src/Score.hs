module Score where

import Chess
import Data.Map hiding (map, foldl)

score :: Chess -> Color -> Int
score chess color =
    sum [if color == c then val pt else - (val pt) | (pos, (Piece c pt)) <- toList $ getBoard chess]

val :: PieceType -> Int
val pt = case pt of
    Pawn -> 1
    Knight -> 3
    Bishop -> 3
    Rook -> 5
    Queen -> 9
    King -> 100
