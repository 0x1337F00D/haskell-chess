module PyChessData where

data PyChessCase = PyChessCase {
    fen :: String
  , depth :: Int
  , nodes :: Int
  , moves :: [String]
  } deriving (Show, Eq)

cases :: [PyChessCase]
cases = [
  PyChessCase "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 4 197281 ["a2a3","a2a4","b1a3","b1c3","b2b3","b2b4","c2c3","c2c4","d2d3","d2d4","e2e3","e2e4","f2f3","f2f4","g1f3","g1h3","g2g3","g2g4","h2h3","h2h4"],
  PyChessCase "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1" 3 97862 ["a1b1","a1c1","a1d1","a2a3","a2a4","b2b3","c3a4","c3b1","c3b5","c3d1","d2c1","d2e3","d2f4","d2g5","d2h6","d5d6","d5e6","e1c1","e1d1","e1f1","e1g1","e2a6","e2b5","e2c4","e2d1","e2d3","e2f1","e5c4","e5c6","e5d3","e5d7","e5f7","e5g4","e5g6","f3d3","f3e3","f3f4","f3f5","f3f6","f3g3","f3g4","f3h3","f3h5","g2g3","g2g4","g2h3","h1f1","h1g1"],
  PyChessCase "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1" 3 2812 ["a5a4","a5a6","b4a4","b4b1","b4b2","b4b3","b4c4","b4d4","b4e4","b4f4","e2e3","e2e4","g2g3","g2g4"],
  PyChessCase "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1" 3 9467 ["b4c5","c4c5","d2d4","f1f2","f3d4","g1h1"],
  PyChessCase "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8" 3 62379 ["a2a3","a2a4","b1a3","b1c3","b1d2","b2b3","b2b4","c1d2","c1e3","c1f4","c1g5","c1h6","c2c3","c4a6","c4b3","c4b5","c4d3","c4d5","c4e6","c4f7","d1d2","d1d3","d1d4","d1d5","d1d6","d7c8b","d7c8n","d7c8q","d7c8r","e1d2","e1f1","e1f2","e1g1","e2c3","e2d4","e2f4","e2g1","e2g3","g2g3","g2g4","h1f1","h1g1","h2h3","h2h4"]
  ]
