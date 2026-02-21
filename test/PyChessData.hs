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
  PyChessCase "Standard" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 4 197281 ["a2a3","a2a4","b1a3","b1c3","b2b3","b2b4","c2c3","c2c4","d2d3","d2d4","e2e3","e2e4","f2f3","f2f4","g1f3","g1h3","g2g3","g2g4","h2h3","h2h4"],
  PyChessCase "Standard" "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1" 3 97862 ["a1b1","a1c1","a1d1","a2a3","a2a4","b2b3","c3a4","c3b1","c3b5","c3d1","d2c1","d2e3","d2f4","d2g5","d2h6","d5d6","d5e6","e1c1","e1d1","e1f1","e1g1","e2a6","e2b5","e2c4","e2d1","e2d3","e2f1","e5c4","e5c6","e5d3","e5d7","e5f7","e5g4","e5g6","f3d3","f3e3","f3f4","f3f5","f3f6","f3g3","f3g4","f3h3","f3h5","g2g3","g2g4","g2h3","h1f1","h1g1"],
  PyChessCase "Standard" "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1" 3 2812 ["a5a4","a5a6","b4a4","b4b1","b4b2","b4b3","b4c4","b4d4","b4e4","b4f4","e2e3","e2e4","g2g3","g2g4"],
  PyChessCase "Standard" "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1" 3 9467 ["b4c5","c4c5","d2d4","f1f2","f3d4","g1h1"],
  PyChessCase "Standard" "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8" 3 62379 ["a2a3","a2a4","b1a3","b1c3","b1d2","b2b3","b2b4","c1d2","c1e3","c1f4","c1g5","c1h6","c2c3","c4a6","c4b3","c4b5","c4d3","c4d5","c4e6","c4f7","d1d2","d1d3","d1d4","d1d5","d1d6","d7c8b","d7c8n","d7c8q","d7c8r","e1d2","e1f1","e1f2","e1g1","e2c3","e2d4","e2f4","e2g1","e2g3","g2g3","g2g4","h1f1","h1g1","h2h3","h2h4"],
  PyChessCase "Atomic" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 3 8902 ["a2a3","a2a4","b1a3","b1c3","b2b3","b2b4","c2c3","c2c4","d2d3","d2d4","e2e3","e2e4","f2f3","f2f4","g1f3","g1h3","g2g3","g2g4","h2h3","h2h4"],
  PyChessCase "Atomic" "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2" 3 20038 ["a2a3","a2a4","b1a3","b1c3","b2b3","b2b4","c2c3","c2c4","d1e2","d1f3","d1g4","d1h5","d2d3","d2d4","e1e2","e4e5","f1a6","f1b5","f1c4","f1d3","f1e2","f2f3","f2f4","g1e2","g1f3","g1h3","g2g3","g2g4","h2h3","h2h4"],
  PyChessCase "King of the Hill" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 3 8902 ["a2a3","a2a4","b1a3","b1c3","b2b3","b2b4","c2c3","c2c4","d2d3","d2d4","e2e3","e2e4","f2f3","f2f4","g1f3","g1h3","g2g3","g2g4","h2h3","h2h4"],
  PyChessCase "Racing Kings" "8/8/8/8/8/8/krbnNBRK/qrbnNBRQ w - - 0 1" 3 11264 ["e1c2","e1d3","e1f3","e2d4","e2f4","e2g3","f2a7","f2b6","f2c5","f2d4","f2e3","f2g3","f2h4","g2g3","g2g4","g2g5","g2g6","g2g7","g2g8","h2g3","h2h3"],
  PyChessCase "Three-check" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 3 8902 ["a2a3","a2a4","b1a3","b1c3","b2b3","b2b4","c2c3","c2c4","d2d3","d2d4","e2e3","e2e4","f2f3","f2f4","g1f3","g1h3","g2g3","g2g4","h2h3","h2h4"],
  PyChessCase "Three-check" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 2+2 1" 3 8902 ["a2a3","a2a4","b1a3","b1c3","b2b3","b2b4","c2c3","c2c4","d2d3","d2d4","e2e3","e2e4","f2f3","f2f4","g1f3","g1h3","g2g3","g2g4","h2h3","h2h4"],
  PyChessCase "Crazyhouse" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 3 8902 ["a2a3","a2a4","b1a3","b1c3","b2b3","b2b4","c2c3","c2c4","d2d3","d2d4","e2e3","e2e4","f2f3","f2f4","g1f3","g1h3","g2g3","g2g4","h2h3","h2h4"],
  PyChessCase "Crazyhouse" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR[PNBRQ] w KQkq - 0 1" 3 539119 ["B@a3","B@a4","B@a5","B@a6","B@b3","B@b4","B@b5","B@b6","B@c3","B@c4","B@c5","B@c6","B@d3","B@d4","B@d5","B@d6","B@e3","B@e4","B@e5","B@e6","B@f3","B@f4","B@f5","B@f6","B@g3","B@g4","B@g5","B@g6","B@h3","B@h4","B@h5","B@h6","N@a3","N@a4","N@a5","N@a6","N@b3","N@b4","N@b5","N@b6","N@c3","N@c4","N@c5","N@c6","N@d3","N@d4","N@d5","N@d6","N@e3","N@e4","N@e5","N@e6","N@f3","N@f4","N@f5","N@f6","N@g3","N@g4","N@g5","N@g6","N@h3","N@h4","N@h5","N@h6","P@a3","P@a4","P@a5","P@a6","P@b3","P@b4","P@b5","P@b6","P@c3","P@c4","P@c5","P@c6","P@d3","P@d4","P@d5","P@d6","P@e3","P@e4","P@e5","P@e6","P@f3","P@f4","P@f5","P@f6","P@g3","P@g4","P@g5","P@g6","P@h3","P@h4","P@h5","P@h6","Q@a3","Q@a4","Q@a5","Q@a6","Q@b3","Q@b4","Q@b5","Q@b6","Q@c3","Q@c4","Q@c5","Q@c6","Q@d3","Q@d4","Q@d5","Q@d6","Q@e3","Q@e4","Q@e5","Q@e6","Q@f3","Q@f4","Q@f5","Q@f6","Q@g3","Q@g4","Q@g5","Q@g6","Q@h3","Q@h4","Q@h5","Q@h6","R@a3","R@a4","R@a5","R@a6","R@b3","R@b4","R@b5","R@b6","R@c3","R@c4","R@c5","R@c6","R@d3","R@d4","R@d5","R@d6","R@e3","R@e4","R@e5","R@e6","R@f3","R@f4","R@f5","R@f6","R@g3","R@g4","R@g5","R@g6","R@h3","R@h4","R@h5","R@h6","a2a3","a2a4","b1a3","b1c3","b2b3","b2b4","c2c3","c2c4","d2d3","d2d4","e2e3","e2e4","f2f3","f2f4","g1f3","g1h3","g2g3","g2g4","h2h3","h2h4"],
  PyChessCase "Antichess" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1" 3 8067 ["a2a3","a2a4","b1a3","b1c3","b2b3","b2b4","c2c3","c2c4","d2d3","d2d4","e2e3","e2e4","f2f3","f2f4","g1f3","g1h3","g2g3","g2g4","h2h3","h2h4"],
  PyChessCase "Horde" "rnbqkbnr/pppppppp/8/1PP2PP1/PPPPPPPP/PPPPPPPP/PPPPPPPP/PPPPPPPP w kq - 0 1" 3 1274 ["a4a5","b5b6","c5c6","d4d5","e4e5","f5f6","g5g6","h4h5"],
  PyChessCase "Chess960" "rnbq1rk1/pppppppp/5n2/8/8/5N2/PPPPPPPP/RNBQ1RK1 w HFhf - 2 2" 3 14394 ["a2a3","a2a4","b1a3","b1c3","b2b3","b2b4","c2c3","c2c4","d1e1","d2d3","d2d4","e2e3","e2e4","f1e1","f3d4","f3e1","f3e5","f3g5","f3h4","g1h1","g2g3","g2g4","h2h3","h2h4"]
  ]
