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

bugCase = readUCIPosition $ words "startpos moves e2e4 h7h5 f2f3 h8h6 d1e2 h6h8 d2d3 e7e5 b1c3 f8b4 c1d2 h8h7 e1c1 h7h8 c3a4 d8e7 e2e1 b4d2 e1d2 e7e6 g2g3 h8h7 f1h3 e6a2 d2e3 a2a4 b2b3 a4c6 f3f4 h5h4 g1f3 h4g3 h2g3 h7h3 h1h2 h3h2 d1d2 h2h1 c1b2 f7f6 f4e5 h1h8 e5f6 g8f6 f3g5 h8h5 g5f3 h5h1 d2h2 h1h2 f3h2 g7g5 e4e5 f6h7 d3d4 h7f8 c2c3 c6g2 b2a3 g2h2 e5e6 g5g4 d4d5 d7e6 c3c4 h2h1 d5e6 h1a1 a3b4 f8g6 e3d3 g6e7 d3d4 a1d4 b4a3"