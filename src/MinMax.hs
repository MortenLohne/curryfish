module MinMax where

import Chess
import Moves
import Fen
import Score

test = score (readFen fen_ex_5) White

minmax :: Chess -> Int -> Move
minmax = undefined

evaluate :: Chess -> Int -> Int -> Int
evaluate chess depth currentDepth = undefined

nextBoards :: Chess -> [Chess]
nextBoards chess = [doMove chess move | move <- allMoves chess]
