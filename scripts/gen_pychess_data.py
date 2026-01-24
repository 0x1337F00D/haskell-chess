import sys
import os

sys.path.append(os.path.abspath("pychess_repo/lib"))

from pychess.Utils.lutils.LBoard import LBoard, FEN_START
from pychess.Utils.lutils.lmovegen import genAllMoves
from pychess.Utils.lutils.lmove import toAN
from pychess.Utils.const import CASTLE_KK

def get_moves_clean(fen):
    board = LBoard()
    board.applyFen(fen)
    moves = []
    for move in genAllMoves(board):
        # Check legality
        board.applyMove(move)
        if board.opIsChecked():
            board.popMove()
            continue
        board.popMove()

        # Now get UCI. board is at state BEFORE move.
        uci = toAN(board, move, short=True, castleNotation=CASTLE_KK)
        moves.append(uci)
    return sorted(moves)

def do_perft(board, depth):
    nodes = 0
    if depth == 0:
        return 1

    for move in genAllMoves(board):
        board.applyMove(move)
        if board.opIsChecked():
            board.popMove()
            continue

        nodes += do_perft(board, depth - 1)
        board.popMove()
    return nodes

def get_perft(fen, depth):
    board = LBoard()
    board.applyFen(fen)
    return do_perft(board, depth)

positions = [
    ("Start", FEN_START, 4),
    ("KiwiPete", "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 3),
    ("Pos3", "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 3),
    ("Pos4", "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 3),
    ("Pos5", "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", 3),
]

with open("test/PyChessData.hs", "w") as f:
    f.write("module PyChessData where\n\n")
    f.write("data PyChessCase = PyChessCase {\n")
    f.write("    fen :: String\n")
    f.write("  , depth :: Int\n")
    f.write("  , nodes :: Int\n")
    f.write("  , moves :: [String]\n")
    f.write("  } deriving (Show, Eq)\n\n")
    f.write("cases :: [PyChessCase]\n")
    f.write("cases = [\n")

    num_positions = len(positions)
    for i, (name, fen, depth) in enumerate(positions):
        print(f"Processing {name}...")
        nodes = get_perft(fen, depth)
        moves = get_moves_clean(fen)
        fen_esc = fen.replace('"', '\\"')
        moves_str = ",".join('"' + m + '"' for m in moves)
        comma = "," if i < num_positions - 1 else ""
        f.write(f"  PyChessCase \"{fen_esc}\" {depth} {nodes} [{moves_str}]{comma}\n")

    f.write("  ]\n")

print("test/PyChessData.hs generated.")
