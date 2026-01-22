module PyChessData where

data PyChessCase = PyChessCase {
    variant :: String
  , fen :: String
  , depth :: Int
  , nodes :: Int
  , moves :: [String]
  } deriving (Show, Eq)

cases :: [PyChessCase]
cases = [
  PyChessCase "Standard" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 3 8902 ["a2a3","a2a4","b1a3","b1c3","b2b3","b2b4","c2c3","c2c4","d2d3","d2d4","e2e3","e2e4","f2f3","f2f4","g1f3","g1h3","g2g3","g2g4","h2h3","h2h4"],
  PyChessCase "Standard" "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1" 2 2039 ["a1b1","a1c1","a1d1","a2a3","a2a4","b2b3","c3a4","c3b1","c3b5","c3d1","d2c1","d2e3","d2f4","d2g5","d2h6","d5d6","d5e6","e1c1","e1d1","e1f1","e1g1","e2a6","e2b5","e2c4","e2d1","e2d3","e2f1","e5c4","e5c6","e5d3","e5d7","e5f7","e5g4","e5g6","f3d3","f3e3","f3f4","f3f5","f3f6","f3g3","f3g4","f3h3","f3h5","g2g3","g2g4","g2h3","h1f1","h1g1"],
  PyChessCase "Atomic" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 3 8902 ["a2a3","a2a4","b1a3","b1c3","b2b3","b2b4","c2c3","c2c4","d2d3","d2d4","e2e3","e2e4","f2f3","f2f4","g1f3","g1h3","g2g3","g2g4","h2h3","h2h4"],
  PyChessCase "Atomic" "rnbqkbnr/pppppppp/8/8/8/4P3/PPPP1PPP/RNBQKBNR b KQkq - 0 1" 3 13145 ["a7a5","a7a6","b7b5","b7b6","b8a6","b8c6","c7c5","c7c6","d7d5","d7d6","e7e5","e7e6","f7f5","f7f6","g7g5","g7g6","g8f6","g8h6","h7h5","h7h6"],
  PyChessCase "KingOfTheHill" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 3 8902 ["a2a3","a2a4","b1a3","b1c3","b2b3","b2b4","c2c3","c2c4","d2d3","d2d4","e2e3","e2e4","f2f3","f2f4","g1f3","g1h3","g2g3","g2g4","h2h3","h2h4"],
  PyChessCase "KingOfTheHill" "8/8/8/4k3/3K4/8/8/8 w - - 0 1" 1 6 ["d4c3","d4c4","d4c5","d4d3","d4e3","d4e5"],
  PyChessCase "RacingKings" "8/8/8/8/8/8/krbnNBRK/qrbnNBRQ w - - 0 1" 2 421 ["e1c2","e1d3","e1f3","e2d4","e2f4","e2g3","f2a7","f2b6","f2c5","f2d4","f2e3","f2g3","f2h4","g2g3","g2g4","g2g5","g2g6","g2g7","g2g8","h2g3","h2h3"],
  PyChessCase "ThreeCheck" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 3 8902 ["a2a3","a2a4","b1a3","b1c3","b2b3","b2b4","c2c3","c2c4","d2d3","d2d4","e2e3","e2e4","f2f3","f2f4","g1f3","g1h3","g2g3","g2g4","h2h3","h2h4"],
  PyChessCase "Crazyhouse" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 3 8902 ["a2a3","a2a4","b1a3","b1c3","b2b3","b2b4","c2c3","c2c4","d2d3","d2d4","e2e3","e2e4","f2f3","f2f4","g1f3","g1h3","g2g3","g2g4","h2h3","h2h4"],
  PyChessCase "Crazyhouse" "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1" 2 600 ["a7a5","a7a6","b7b5","b7b6","b8a6","b8c6","c7c5","c7c6","d7d5","d7d6","e7e5","e7e6","f7f5","f7f6","g7g5","g7g6","g8f6","g8h6","h7h5","h7h6"]
  ]
