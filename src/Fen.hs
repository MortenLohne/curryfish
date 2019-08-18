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

bugCase2 = readUCIPosition $ words "startpos moves e2e3 h7h5 g1f3 h8h6 d2d4 h6h8 f1d3 h8h6 e3e4 h6h8 e1g1 h8h7 f3g5 h7h8 d1f3 g8f6 e4e5 h8h6 e5f6 h6f6 f3h5 g7g6 h5h8 f6b6 g5h7 d7d5 h7f8 d8d6 f8e6 e8d7 h8d8 d7e6 " --f1e1"

bugCase3 = readUCIPosition $ words "startpos moves g1f3 f7f5 g2g3 h7h5 f1g2 h8h6 d2d3 h6h8 c1g5 e8f7 f3e5"

bugCase4 = readUCIPosition $ words "startpos moves h2h4 e7e5 h4h5 d7d5 h5h6 g8h6 h1h5 b8c6 h5h1 f8e7 h1h5 d8d6 h5h1 e8g8"

bugCase5 = readUCIPosition $ words "startpos moves e2e4 h7h5 f2f3 h8h6 d2d3 h6b6 b1a3 h5h4 g2g3 h4g3 h2g3 e7e6 f3f4 g7g6 f4f5 g6f5 e4f5 f8a3 b2a3 d8f6 a1b1 b6b1 c1d2 b1d1 e1d1 f6a1 d1e2 e6f5 g3g4 f5g4 h1h4 f7f5 h4h5 g8e7 h5g5 d7d6 g5g8 e7g8 c2c4 a1a2 e2e3 a2a1 g1e2 a1f1 e2g3 f5f4 e3e4 g8f6 e4d4 c7c5 d4c3 f1a1 c3b3 a1d1 b3a2 d1d2 a2b3 f4g3 b3a4 d2d3 a4b5"
