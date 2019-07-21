module Chess where

import Prelude hiding (lookup)
import Data.Map hiding (map)
import Data.Char

data Chess = Chess Board Color

type Board = Map Pos Piece

data Piece = Piece Color PieceType

data Color = Black | White deriving (Eq)

data PieceType = Pawn | Knight | Rook | Bishop | Queen | King deriving Eq

-- a1 = (1, 1)
type Pos = (Int, Int)

type Move = (Pos, Pos)

getBoard :: Chess -> Board
getBoard (Chess board color) = board

-- NOTE : Data.Map.lookup is O(lg(n))
getPiece :: Chess -> Pos -> Maybe Piece
getPiece (Chess board _) pos = lookup pos board

setPiece :: Chess -> Piece -> Pos -> Chess
setPiece (Chess board color) piece pos = Chess (insert pos piece board) color

hasPiece :: Chess -> Pos -> Bool
hasPiece (Chess board _) pos = case lookup pos board of
    Just piece -> True
    Nothing -> False

doMove :: Chess -> Move -> Chess
doMove chess@(Chess board color) (p1, p2) =
    let Just piece = getPiece chess p1
    in Chess (insert p2 piece $ delete p1 board) (opposite color)

changeColor :: Chess -> Chess
changeColor (Chess board color) = Chess board $ opposite color

opposite :: Color -> Color
opposite Black = White
opposite White = Black

nextMove :: Chess -> Color
nextMove (Chess _ color) = color

withinBounds :: Pos -> Bool
withinBounds (x, y) = x > 0 && y > 0 && x < 9 && y < 9

emptyBoard :: Chess
emptyBoard = Chess empty White

instance Show Chess where
    show chess = 
        "\n" ++ 
        concat [
            (show y) ++ " " ++ concat [ " " ++ (prettyPiece (getPiece chess (x, y))) ++ " " | 
            x <- [1..8] ] ++ " \n\n" | y <- reverse [1..8]]
        ++ "  " ++ concat [' ':(chr $ i + 96):' ':[] | i <- [1..8]]
        ++ "\n\n"

prettyPiece :: Maybe Piece -> String
prettyPiece piece = case piece of
    Just (Piece color pieceType) ->
        let c = case pieceType of
                    Pawn -> "p"                 
                    Knight -> "n"
                    Rook -> "r"
                    Bishop -> "b"
                    Queen -> "q"
                    King -> "k"
        in case color of
                    White -> map toUpper c
                    _ -> c
    Nothing -> "_"

showMove :: Move -> String
showMove (p1, p2) = (showPos p1) ++ (showPos p2)

showPos :: Pos -> String
showPos (x, y) = chr (x + 96) : (show y)
