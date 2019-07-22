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
fen_ex_1 = "rnbQkbnr/ppp2ppp/8/4p3/4P3/8/PPP2PPP/RNB1KBNR b KQkq - 0 1"
fen_ex_2 = "RRRRRRRK/pPBBBBPP/pbPBBPBP/pbbPPBBP/pbbppBBP/pbpbbpBP/ppbbbbpP/krrrrrrr w - - 0 1"
fen_ex_3 = "rnbqkbnr/pp1ppppp/8/8/8/2p5/PPPPPPPP/RNBQKBNR w KQkq -"
fen_ex_4 = "rnbqkbnr/pp1ppppp/8/8/8/2P5/PP1PPPPP/RNBQKBNR w KQkq -"
fen_ex_5 = "rnbqkbnr/pp1ppppp/8/2P5/8/8/PPP1PPPP/RNBQKBNR b KQkq - 0 2"

-- SAMPLE BOARDS
blackCheckedBoard = readUCIPosition $ words "startpos moves e2e3 h7h5 f1b5 h8h6 e3e4 h6h8 e4e5 h8h6 e5e6 h6h8 e6f7"

-- Illegal move for black : e8e7
bugCase = readUCIPosition $ words "startpos moves d2d4 h7h5 e2e4 h8h7 g1f3 h7h8 f1e2 h8h7 f3g5 h7h6 e2h5 h6f6 e4e5 g8h6 e5f6 e7e5 d4e5 d8f6 e5f6 h6f5 h5f7"