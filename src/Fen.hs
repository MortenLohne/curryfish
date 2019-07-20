module Fen where

import Chess
import Data.Char

readUCIPosition :: [String] -> Chess
readUCIPosition ("startpos":"moves":xs) = doMoves xs $ readFen fen_start
readUCIPosition (fen:"moves":xs) = doMoves xs $ readFen fen
readUCIPosition (fen:xs) = readFen fen

doMoves :: [String] -> Chess -> Chess
doMoves [] chess = chess
doMoves (x:xs) chess = let move = readMove x in doMoves xs $ doMove chess move

readMove :: String -> Move
readMove (x1:y1:x2:y2:[]) = (readPos x1 y1, readPos x2 y2)

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
