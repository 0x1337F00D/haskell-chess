module GameTerminationSpec (spec) where

import Test.Hspec
import Chess.Pgn
import Chess.Board
import Control.Monad (foldM)

-- | Play a sequence of moves from a PGN game and return the final board.
playGame :: Board -> Game -> IO Board
playGame startBoard game = do
    foldM playMove startBoard (map plySan (plies game))
  where
    playMove :: Board -> String -> IO Board
    playMove b sanStr = do
        let mMove = parseSan b sanStr
        case mMove of
            Nothing -> fail $ "Failed to parse SAN: " ++ sanStr ++ " FEN: " ++ fen b
            Just move -> return $ applyMove b move

spec :: Spec
spec = do
  describe "Game Termination Tests (from pychess)" $ do

    it "detects 3-fold repetition (3fold.pgn)" $ do
        pgnContent <- readFile "test/gamefiles/3fold.pgn"
        let parsed = parsePgn pgnContent
        case parsed of
            Left err -> expectationFailure $ "PGN Parse Error: " ++ err
            Right [] -> expectationFailure "No games found in 3fold.pgn"
            Right games -> do
                if length games < 2
                   then expectationFailure $ "Expected at least 2 games in 3fold.pgn, found " ++ show (length games)
                   else do
                       -- First game: 3-fold repetition
                       let game1 = head games
                       finalBoard <- playGame initialBoard game1
                       outcome finalBoard `shouldBe` Just (Outcome ThreefoldRepetition Nothing)

                       -- Second game: 3-fold repetition
                       let game2 = games !! 1
                       finalBoard2 <- playGame initialBoard game2
                       outcome finalBoard2 `shouldBe` Just (Outcome ThreefoldRepetition Nothing)

    it "detects 50-move rule (bilbao.pgn)" $ do
        pgnContent <- readFile "test/gamefiles/bilbao.pgn"
        let parsed = parsePgn pgnContent
        case parsed of
            Left err -> expectationFailure $ "PGN Parse Error: " ++ err
            Right [] -> expectationFailure "No games found in bilbao.pgn"
            Right (game:_) -> do
                let allMoves = map plySan (plies game)
                let movesExceptLast = init allMoves
                let lastMoveSan = last allMoves

                -- Play until just before last move
                boardBeforeLast <- foldM (\b m -> do
                    let mMove = parseSan b m
                    case mMove of
                        Nothing -> fail $ "Failed to parse SAN: " ++ m ++ " FEN: " ++ fen b
                        Just move -> return $ applyMove b move) initialBoard movesExceptLast

                -- Check before last move: Should NOT be FiftyMoves
                -- Note: It could be something else if checkmate happens, but in this game it shouldn't.
                outcome boardBeforeLast `shouldSatisfy` (/= Just (Outcome FiftyMoves Nothing))

                -- Apply last move
                let mLastMove = parseSan boardBeforeLast lastMoveSan
                case mLastMove of
                     Nothing -> fail $ "Failed to parse last move: " ++ lastMoveSan
                     Just lastMove -> do
                        let finalBoard = applyMove boardBeforeLast lastMove
                        outcome finalBoard `shouldBe` Just (Outcome FiftyMoves Nothing)

    it "detects insufficient material (material.pgn)" $ do
        pgnContent <- readFile "test/gamefiles/material.pgn"
        let parsed = parsePgn pgnContent
        case parsed of
            Left err -> expectationFailure $ "PGN Parse Error: " ++ err
            Right [] -> expectationFailure "No games found in material.pgn"
            Right (game:_) -> do
                finalBoard <- playGame initialBoard game
                outcome finalBoard `shouldBe` Just (Outcome InsufficientMaterial Nothing)
