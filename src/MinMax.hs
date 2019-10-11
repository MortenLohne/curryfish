module MinMax where

import           Chess
import           Moves
import           Fen
import           Score
import           Data.List

import           Prelude                 hiding ( max
                                                , min
                                                )

minmax :: Chess -> Int -> (Int, Move)
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
    in max result

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
        in  if nextMove chess == color
                then 
                    if null next -- this would meen no moves left
                    then (-1000, move) 
                    else max result
                else 
                    if null next 
                    then (1000, move)
                    else min result 

nextBoards :: Chess -> [(Chess, Move)]
nextBoards chess = [ (doMove chess move, move) | move <- allMoves chess ]

max :: [(Int, Move)] -> (Int, Move)
max = maximumBy (\(a, _) (b, _) -> compare a b)

min :: [(Int, Move)] -> (Int, Move)
min = minimumBy (\(a, _) (b, _) -> compare a b)
