module MinMax where

import           Chess
import           Moves
import           Fen
import           Score
import           Data.List

import           Prelude                 hiding ( max
                                                , min
                                                )

minmax :: Chess -> Int -> Move
minmax chess depth =
    let first  = nextBoards chess
        first' = filter
            (\(chess, _) -> not $ isChecked chess $ opposite $ nextMove chess)
            first
        result = map
            (\(chess', move') ->
                minmax' (chess', move') depth 1 (nextMove chess)
            )
            first'
        (_, bestMove) = max result
    in  bestMove

minmax' :: (Chess, Move) -> Int -> Int -> Color -> (Int, Move)
minmax' (chess, move) depth currentDepth color = if depth == currentDepth
    then (score chess color, move)
    else
        let next   = nextBoards chess
            result = map
                (\(chess', _) ->
                    minmax' (chess', move) depth (currentDepth + 1) color
                )
                next
        in  if nextMove chess == color then max result else min result

nextBoards :: Chess -> [(Chess, Move)]
nextBoards chess = [ (doMove chess move, move) | move <- allMoves chess ]

max :: [(Int, Move)] -> (Int, Move)
max = maximumBy (\(a, _) (b, _) -> compare a b)

min :: [(Int, Move)] -> (Int, Move)
min = minimumBy (\(a, _) (b, _) -> compare a b)
