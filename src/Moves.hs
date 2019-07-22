module Moves where

import           Chess
import           Prelude                 hiding ( lookup )
import           Data.Map                hiding ( filter )

-- NOTE : Data.Map.keys => O(n)
allMoves :: Chess -> [Move]
allMoves chess = concat [ pieceMoves chess pos | pos <- keys $ getBoard chess ]

pieceMoves :: Chess -> Pos -> [Move]
pieceMoves chess pos =
    let Just piece@(Piece pc pieceType) = lookup pos $ getBoard chess
    in  if pc /= (nextMove chess)
            then []
            else
                filter (\(_, newPos) -> validNewPos chess newPos piece)
                    $ case pieceType of
                          Pawn   -> pawn chess pos piece
                          Knight -> knight chess pos piece
                          Rook   -> rook chess pos piece
                          Bishop -> bishop chess pos piece
                          Queen  -> queen chess pos piece
                          King   -> king chess pos piece

pawn :: Chess -> Pos -> Piece -> [Move]
pawn chess pos@(x, y) piece@(Piece color _) = case color of
    White -> (pawnAttack chess pos piece 1) ++ (pawnMove chess pos piece 1 2)
    Black ->
        (pawnAttack chess pos piece (-1)) ++ (pawnMove chess pos piece (-1) 7)

pawnMove :: Chess -> Pos -> Piece -> Int -> Int -> [Move]
pawnMove chess pos@(x, y) piece@(Piece color _) delta start =
    zip (repeat pos) $ case getPiece chess (x, y + delta) of
        Nothing -> if y == start
            then case getPiece chess (x, y + (2 * delta)) of
                Nothing -> [(x, y + delta), (x, y + (2 * delta))]
                _       -> [(x, y + delta)]
            else [(x, y + delta)]
        _ -> []

pawnAttack :: Chess -> Pos -> Piece -> Int -> [Move]
pawnAttack chess pos@(x, y) piece delta =
    let va (Piece color _) newPos = case getPiece chess newPos of
            Just (Piece c _) -> c /= color
            Nothing          -> False
    in  [ (pos, p) | p <- [(x + 1, y + delta), (x - 1, y + delta)], va piece p ]

knight :: Chess -> Pos -> Piece -> [Move]
knight chess pos@(x, y) piece =
    let possible =
                [ (1 , 2)
                , (1 , -2)
                , (-1, 2)
                , (-1, -2)
                , (2 , 1)
                , (2 , -1)
                , (-2, 1)
                , (-2, -1)
                ]
    in  [ (pos, (x + x', y + y')) | (x', y') <- possible ]

rook :: Chess -> Pos -> Piece -> [Move]
rook chess pos@(x, y) piece =
    rows chess piece pos [(1, 0), (-1, 0), (0, 1), (0, -1)]

bishop :: Chess -> Pos -> Piece -> [Move]
bishop chess pos@(x, y) piece =
    rows chess piece pos [(1, 1), (-1, 1), (1, -1), (-1, -1)]

queen :: Chess -> Pos -> Piece -> [Move]
queen chess pos@(x, y) piece = rows
    chess
    piece
    pos
    [(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (-1, 1), (1, -1), (-1, -1)]

king :: Chess -> Pos -> Piece -> [Move]
king chess pos@(x, y) piece =
    let possible =
                [ (1 , -1)
                , (1 , 0)
                , (1 , 1)
                , (0 , -1)
                , (0 , 1)
                , (-1, -1)
                , (-1, 0)
                , (-1, 1)
                ]
    in  [ (pos, (x + x', y + y')) | (x', y') <- possible ]

validNewPos :: Chess -> Pos -> Piece -> Bool
validNewPos chess pos (Piece color _) =
    withinBounds pos && case getPiece chess pos of
        Just (Piece destColor _) -> destColor /= color
        Nothing                  -> True

rows :: Chess -> Piece -> Pos -> [(Int, Int)] -> [Move]
rows chess piece pos deltas =
    let destinations = concat [ row chess piece pos d | d <- deltas ]
    in  zip (repeat pos) destinations

row :: Chess -> Piece -> Pos -> (Int, Int) -> [Pos]
row chess piece@(Piece pColor _) curr@(currX, currY) delta@(deltaX, deltaY) =
    let nextPos = (currX + deltaX, currY + deltaY)
    in  if withinBounds nextPos
            then case getPiece chess nextPos of
                Just (Piece color _) ->
                    if color /= pColor then [nextPos] else []
                Nothing -> nextPos : row chess piece nextPos delta
            else []

isChecked :: Chess -> Color -> Bool
isChecked chess color =
    let kingPos          = getKingPos chess color
        moveDestinations = getMoveDestinations chess
    in  elem kingPos moveDestinations

getKingPos :: Chess -> Color -> Pos
getKingPos chess color =
    case
            [ pos
            | (pos, Piece c pt) <- toList $ getBoard chess
            , pt == King
            , c == color
            ]
        of
            [x] -> x 
            []  -> error "could'nt find king (game should have ended)"
            _   -> error "found multiple kings with the same color"

getMoveDestinations :: Chess -> [Pos]
getMoveDestinations chess = [ dest | (_, dest) <- allMoves $ changeColor chess ]
