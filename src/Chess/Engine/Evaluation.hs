module Chess.Engine.Evaluation (evaluate) where

import qualified Data.Vector.Unboxed as U

import Chess.Types
import Chess.Bitboard
import qualified Chess.Board.Base as Base
import Chess.Board.GameState (GameState(..))
import Chess.Board (Board(..))

-- | Evaluation score in centipawns.
type Score = Int

-- | Piece values (centipawns).
valuePawn, valueKnight, valueBishop, valueRook, valueQueen :: Score
valuePawn   = 100
valueKnight = 320
valueBishop = 330
valueRook   = 500
valueQueen  = 900

-- | Evaluate the board position from the perspective of the side to move.
evaluate :: Chess.Board.Board -> Score
evaluate (Chess.Board.Board b gs _) =
    let score = evalMaterial b + evalPositional b
    in if turn gs == White then score else -score

-- | Calculate material difference.
evalMaterial :: Base.Board -> Score
evalMaterial b =
    (popcount (Base.whitePawns b)   * valuePawn)   +
    (popcount (Base.whiteKnights b) * valueKnight) +
    (popcount (Base.whiteBishops b) * valueBishop) +
    (popcount (Base.whiteRooks b)   * valueRook)   +
    (popcount (Base.whiteQueens b)  * valueQueen)  -
    (popcount (Base.blackPawns b)   * valuePawn)   -
    (popcount (Base.blackKnights b) * valueKnight) -
    (popcount (Base.blackBishops b) * valueBishop) -
    (popcount (Base.blackRooks b)   * valueRook)   -
    (popcount (Base.blackQueens b)  * valueQueen)

-- | Calculate positional score using Piece-Square Tables.
evalPositional :: Base.Board -> Score
evalPositional b =
    evalPSTO (Base.whitePawns b)   pawnTable   +
    evalPSTO (Base.whiteKnights b) knightTable +
    evalPSTO (Base.whiteBishops b) bishopTable +
    evalPSTO (Base.whiteRooks b)   rookTable   +
    evalPSTO (Base.whiteQueens b)  queenTable  +
    evalPSTO (Base.whiteKings b)   kingTable   -
    evalPSTO (Base.blackPawns b)   pawnTableFlip   -
    evalPSTO (Base.blackKnights b) knightTableFlip -
    evalPSTO (Base.blackBishops b) bishopTableFlip -
    evalPSTO (Base.blackRooks b)   rookTableFlip   -
    evalPSTO (Base.blackQueens b)  queenTableFlip  -
    evalPSTO (Base.blackKings b)   kingTableFlip

evalPSTO :: Bitboard -> U.Vector Score -> Score
evalPSTO bb table = sum [table U.! sq | sq <- scanForward bb]

-- | Flip a PSTO table for Black (mirror ranks).
flipTable :: U.Vector Score -> U.Vector Score
flipTable v = U.generate 64 $ \i ->
    let r = i `div` 8
        f = i `mod` 8
    in v U.! ((7 - r) * 8 + f)

-- Simplified Piece-Square Tables (Midgame)
-- Source: Simplified evaluation functions or common engines (e.g. PeSTO adapted)

pawnTable :: U.Vector Score
pawnTable = U.fromList
  [  0,  0,  0,  0,  0,  0,  0,  0
  , 50, 50, 50, 50, 50, 50, 50, 50
  , 10, 10, 20, 30, 30, 20, 10, 10
  ,  5,  5, 10, 25, 25, 10,  5,  5
  ,  0,  0,  0, 20, 20,  0,  0,  0
  ,  5, -5,-10,  0,  0,-10, -5,  5
  ,  5, 10, 10,-20,-20, 10, 10,  5
  ,  0,  0,  0,  0,  0,  0,  0,  0
  ]

pawnTableFlip :: U.Vector Score
pawnTableFlip = flipTable pawnTable

knightTable :: U.Vector Score
knightTable = U.fromList
  [ -50,-40,-30,-30,-30,-30,-40,-50
  , -40,-20,  0,  0,  0,  0,-20,-40
  , -30,  0, 10, 15, 15, 10,  0,-30
  , -30,  5, 15, 20, 20, 15,  5,-30
  , -30,  0, 15, 20, 20, 15,  0,-30
  , -30,  5, 10, 15, 15, 10,  5,-30
  , -40,-20,  0,  5,  5,  0,-20,-40
  , -50,-40,-30,-30,-30,-30,-40,-50
  ]

knightTableFlip :: U.Vector Score
knightTableFlip = flipTable knightTable

bishopTable :: U.Vector Score
bishopTable = U.fromList
  [ -20,-10,-10,-10,-10,-10,-10,-20
  , -10,  0,  0,  0,  0,  0,  0,-10
  , -10,  0,  5, 10, 10,  5,  0,-10
  , -10,  5,  5, 10, 10,  5,  5,-10
  , -10,  0, 10, 10, 10, 10,  0,-10
  , -10, 10, 10, 10, 10, 10, 10,-10
  , -10,  5,  0,  0,  0,  0,  5,-10
  , -20,-10,-10,-10,-10,-10,-10,-20
  ]

bishopTableFlip :: U.Vector Score
bishopTableFlip = flipTable bishopTable

rookTable :: U.Vector Score
rookTable = U.fromList
  [  0,  0,  0,  0,  0,  0,  0,  0
  ,  5, 10, 10, 10, 10, 10, 10,  5
  , -5,  0,  0,  0,  0,  0,  0, -5
  , -5,  0,  0,  0,  0,  0,  0, -5
  , -5,  0,  0,  0,  0,  0,  0, -5
  , -5,  0,  0,  0,  0,  0,  0, -5
  , -5,  0,  0,  0,  0,  0,  0, -5
  ,  0,  0,  0,  5,  5,  0,  0,  0
  ]

rookTableFlip :: U.Vector Score
rookTableFlip = flipTable rookTable

queenTable :: U.Vector Score
queenTable = U.fromList
  [ -20,-10,-10, -5, -5,-10,-10,-20
  , -10,  0,  0,  0,  0,  0,  0,-10
  , -10,  0,  5,  5,  5,  5,  0,-10
  ,  -5,  0,  5,  5,  5,  5,  0, -5
  ,   0,  0,  5,  5,  5,  5,  0, -5
  , -10,  5,  5,  5,  5,  5,  0,-10
  , -10,  0,  5,  0,  0,  0,  0,-10
  , -20,-10,-10, -5, -5,-10,-10,-20
  ]

queenTableFlip :: U.Vector Score
queenTableFlip = flipTable queenTable

kingTable :: U.Vector Score
kingTable = U.fromList
  [ -30,-40,-40,-50,-50,-40,-40,-30
  , -30,-40,-40,-50,-50,-40,-40,-30
  , -30,-40,-40,-50,-50,-40,-40,-30
  , -30,-40,-40,-50,-50,-40,-40,-30
  , -20,-30,-30,-40,-40,-30,-30,-20
  , -10,-20,-20,-20,-20,-20,-20,-10
  ,  20, 20,  0,  0,  0,  0, 20, 20
  ,  20, 30, 10,  0,  0, 10, 30, 20
  ]

kingTableFlip :: U.Vector Score
kingTableFlip = flipTable kingTable
