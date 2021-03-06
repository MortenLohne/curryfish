module Fen where

import Chess
import Data.Char

readUCIPosition :: [String] -> Chess
readUCIPosition ("startpos":[]) = readFen fen_start
readUCIPosition ("startpos":"moves":xs) = doMoves xs $ readFen fen_start
readUCIPosition (fen:"moves":xs) = doMoves xs $ readFen fen
readUCIPosition (fen:xs) = readFen fen

doMoves :: [String] -> Chess -> Chess
doMoves [] chess = chess
doMoves (x:xs) chess = let (move, newPiece) = readMove x 
    in case newPiece of
        Nothing -> doMoves xs $ doMove chess move
        Just piece -> 
            let (_:_:x':y':_) = x 
                pos = readPos x' y'
            in doMoves xs $ doMove (setPiece chess piece pos) move

readMove :: String -> (Move, Maybe Piece)
readMove (x1:y1:x2:y2:[]) = ((readPos x1 y1, readPos x2 y2), Nothing)
readMove str@(x1:y1:x2:y2:newPiece:[]) = (fst $ readMove $ init str, Just $ readPiece newPiece)

readPos :: Char -> Char -> Pos
readPos x y = (((ord x) - 96), read [y])

readFen :: String -> Chess
readFen fen = let board:rest = words fen
              in readBoard board 1 8 emptyBoard

readBoard :: String -> Int -> Int -> Chess -> Chess
readBoard [] _ _ chess = chess
readBoard (c:cs) x y chess | isAlpha c = let piece = readPiece c
                                         in readBoard cs (x + 1) y (setPiece chess piece (x, y))
                           | isDigit c = let i = (read [c] :: Int) in readBoard cs (x + i) y chess
                           | c == '/'  = readBoard cs 1 (y - 1) chess
                           | otherwise = error "invalid fen string"

readPiece :: Char -> Piece
readPiece c =
    let color = if isUpper c then White else Black
        pieceType = case toLower c of
            'r' -> Rook
            'n' -> Knight
            'b' -> Bishop
            'q' -> Queen
            'k' -> King
            'p' -> Pawn
    in Piece color pieceType

fen_start = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

anPassantBugCase = readUCIPosition $ words "startpos moves e2e4 b8c6 d2d4 e7e6 g1f3 f8b4 c2c3 b4d6 f1b5 d6e7 e1g1 c6b8 b1d2 g8f6 e4e5 f6d5 d2e4 f7f5 e5f6"