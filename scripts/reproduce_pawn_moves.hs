module Main where

import Chess.Board.MoveGen
import Chess.Board.Base
import Chess.Board.GameState
import Chess.Types
import Chess.Bitboard
import Data.Bits

-- Helper to set up a board
makeBoard :: [(Square, Piece)] -> Board
makeBoard pieces = foldl (\b (sq, p) -> putPiece b sq p) empty pieces

-- Helper to print moves
printMoves :: [GenMove] -> IO ()
printMoves moves = mapM_ (\(GenMove m pt cap) -> putStrLn $ show m ++ " " ++ show pt ++ " " ++ show cap) moves

main :: IO ()
main = do
    putStrLn "--- Test 1: White Pawn Push & Double Push ---"
    -- White pawn at E2, blocked at E4. Should generate E2-E3. E2-E4 is blocked.
    let b1 = makeBoard [(E2, Piece White Pawn), (E4, Piece Black Pawn)]
        gs1 = initialGameState
    printMoves $ pawnMoves b1 gs1

    putStrLn "\n--- Test 2: White Pawn Capture ---"
    -- White pawn at E4, Black pawns at D5, F5.
    let b2 = makeBoard [(E4, Piece White Pawn), (D5, Piece Black Pawn), (F5, Piece Black Pawn)]
        gs2 = initialGameState
    printMoves $ pawnMoves b2 gs2

    putStrLn "\n--- Test 3: Promotion ---"
    -- White pawn at A7, empty A8.
    let b3 = makeBoard [(A7, Piece White Pawn)]
        gs3 = initialGameState
    printMoves $ pawnMoves b3 gs3

    putStrLn "\n--- Test 4: En Passant ---"
    -- White pawn at E5, Black pawn just moved D7->D5 (EP square D6).
    let b4 = makeBoard [(E5, Piece White Pawn), (D5, Piece Black Pawn)]
        gs4 = initialGameState { epSquare = Just D6 }
    printMoves $ pawnMoves b4 gs4

    putStrLn "\n--- Test 5: Black Pawn Moves ---"
    -- Black pawn at E7.
    let b5 = makeBoard [(E7, Piece Black Pawn)]
        gs5 = initialGameState { turn = Black }
    printMoves $ pawnMoves b5 gs5
