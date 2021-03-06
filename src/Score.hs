module Score where

import           Chess
import           Data.Map                hiding ( map
                                                , foldl
                                                )

score :: Chess -> Color -> Int
score chess color = sum
    [ if color == c then val pt pos c else -(val pt pos c)
    | (pos, (Piece c pt)) <- toList $ getBoard chess
    ]

val :: PieceType -> Pos -> Color -> Int
val pt pos color =
    let (x, y) = translatePos pos color
    in  case pt of
            Pawn   -> 100 + (pawnBonus !! y) !! x
            Knight -> 320 + (knightBonus !! y) !! x
            Bishop -> 330 + (bishopBonus !! y) !! x
            Rook   -> 500 + (rookBonus !! y) !! x
            Queen  -> 900 + (queenBonus !! y) !! x
            King   -> 20000 + (kingBonus !! y) !! x

translatePos :: Pos -> Color -> Pos
translatePos (x, y) color = case color of
    Black -> (x - 1, y - 1)
    White -> (x - 1, 8 - y)

pawnBonus =
    [ [0, 0, 0, 0, 0, 0, 0, 0]
    , [50, 50, 50, 50, 50, 50, 50, 50]
    , [10, 10, 20, 30, 30, 20, 10, 10]
    , [5, 5, 10, 25, 25, 10, 5, 5]
    , [0, 0, 0, 20, 20, 0, 0, 0]
    , [5, -5, -10, 0, 0, -10, -5, 5]
    , [5, 10, 10, -20, -20, 10, 10, 5]
    , [0, 0, 0, 0, 0, 0, 0, 0]
    ]

knightBonus =
    [ [-50, -40, -30, -30, -30, -30, -40, -50]
    , [-40, -20, 0, 0, 0, 0, -20, -40]
    , [-30, 0, 10, 15, 15, 10, 0, -30]
    , [-30, 5, 15, 20, 20, 15, 5, -30]
    , [-30, 0, 15, 20, 20, 15, 0, -30]
    , [-30, 5, 10, 15, 15, 10, 5, -30]
    , [-40, -20, 0, 5, 5, 0, -20, -40]
    , [-50, -40, -30, -30, -30, -30, -40, -50]
    ]

bishopBonus =
    [ [-20, -10, -10, -10, -10, -10, -10, -20]
    , [-10, 0, 0, 0, 0, 0, 0, -10]
    , [-10, 0, 5, 10, 10, 5, 0, -10]
    , [-10, 5, 5, 10, 10, 5, 5, -10]
    , [-10, 0, 10, 10, 10, 10, 0, -10]
    , [-10, 10, 10, 10, 10, 10, 10, -10]
    , [-10, 5, 0, 0, 0, 0, 5, -10]
    , [-20, -10, -10, -10, -10, -10, -10, -20]
    ]

rookBonus =
    [ [0, 0, 0, 0, 0, 0, 0, 0]
    , [5, 10, 10, 10, 10, 10, 10, 5]
    , [-5, 0, 0, 0, 0, 0, 0, -5]
    , [-5, 0, 0, 0, 0, 0, 0, -5]
    , [-5, 0, 0, 0, 0, 0, 0, -5]
    , [-5, 0, 0, 0, 0, 0, 0, -5]
    , [-5, 0, 0, 0, 0, 0, 0, -5]
    , [0, 0, 0, 5, 5, 0, 0, 0]
    ]


queenBonus =
    [ [-20, -10, -10, -5, -5, -10, -10, -20]
    , [-10, 0, 0, 0, 0, 0, 0, -10]
    , [-10, 0, 5, 5, 5, 5, 0, -10]
    , [-5, 0, 5, 5, 5, 5, 0, -5]
    , [0, 0, 5, 5, 5, 5, 0, -5]
    , [-10, 5, 5, 5, 5, 5, 0, -10]
    , [-10, 0, 5, 0, 0, 0, 0, -10]
    , [-20, -10, -10, -5, -5, -10, -10, -20]
    ]

kingBonus =
    [ [-20, -10, -10, -5, -5, -10, -10, -20]
    , [-10, 0, 0, 0, 0, 0, 0, -10]
    , [-10, 0, 5, 5, 5, 5, 0, -10]
    , [-5, 0, 5, 5, 5, 5, 0, -5]
    , [0, 0, 5, 5, 5, 5, 0, -5]
    , [-10, 5, 5, 5, 5, 5, 0, -10]
    , [-10, 0, 5, 0, 0, 0, 0, -10]
    , [-20, -10, -10, -5, -5, -10, -10, -20]
    ]
