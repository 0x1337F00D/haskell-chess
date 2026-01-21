import sys
import os
import time

sys.path.append(os.path.abspath("pychess_repo/lib"))

from pychess.Utils.lutils.LBoard import LBoard, FEN_START
from pychess.Utils.lutils.lmovegen import genAllMoves

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

def run_bench(name, fen, depth):
    board = LBoard()
    board.applyFen(fen)

    start = time.time()
    nodes = do_perft(board, depth)
    end = time.time()

    duration = end - start
    nps = nodes / duration if duration > 0 else 0

    print(f"PyChess | {name:10} | Depth {depth} | Nodes: {nodes:10} | Time: {duration:6.3f}s | NPS: {nps:10.0f}")

positions = [
    ("Start", FEN_START, 5),
    ("KiwiPete", "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 4),
]

for name, fen, depth in positions:
    run_bench(name, fen, depth)
