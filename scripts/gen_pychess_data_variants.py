import sys
import os

# Ensure we can import pychess
sys.path.append(os.path.abspath("pychess_repo/lib"))

from pychess.Utils.Board import Board
from pychess.Utils.Move import Move
from pychess.Utils import logic
from pychess.Utils.lutils import lmovegen
from pychess.Utils.lutils.lmove import FCORD
from pychess.Utils.const import reprSign, DROP, FEN_START
from pychess.Variants import name2variant

# List of variants to test
VARIANTS = [
    "Standard",
    "Atomic",
    "King of the Hill",
    "Racing Kings",
    "Three-check",
    "Crazyhouse",
    "Antichess",
    "Horde",
    "Chess960"
]

# Standard positions
POSITIONS_STANDARD = [
    ("Start", FEN_START, 4),
    ("KiwiPete", "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 3),
    ("Pos3", "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 3),
    ("Pos4", "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 3),
    ("Pos5", "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", 3),
]

POSITIONS_VARIANTS = {
    "Standard": POSITIONS_STANDARD,
    "Atomic": [
        ("Start", FEN_START, 3),
        ("Explosion", "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2", 3)
    ],
    "King of the Hill": [
        ("Start", FEN_START, 3),
    ],
    "Three-check": [
        ("Start", FEN_START, 3),
        ("Checks", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 2+2 1", 3)
    ],
    "Crazyhouse": [
        ("Start", FEN_START, 3),
        ("Pockets", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR[PNBRQ] w KQkq - 0 1", 3)
    ],
    "Antichess": [
        ("Start", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1", 3),
    ],
    "Racing Kings": [
        ("Start", "8/8/8/8/8/8/krbnNBRK/qrbnNBRQ w - - 0 1", 3),
    ],
    "Horde": [
        ("Start", "rnbqkbnr/pppppppp/8/1PP2PP1/PPPPPPPP/PPPPPPPP/PPPPPPPP/PPPPPPPP w kq - 0 1", 3),
    ],
    "Chess960": [
        ("Start_518", "rnbq1rk1/pppppppp/5n2/8/8/5N2/PPPPPPPP/RNBQ1RK1 w HFhf - 2 2", 3),
    ]
}

def move_to_uci(move_obj):
    if move_obj.flag == DROP:
        piece_int = FCORD(move_obj.move)
        piece_char = reprSign[piece_int]
        dest = str(move_obj.cord1)
        return f"{piece_char}@{dest}"
    else:
        return move_obj.as_uci()

def get_legal_moves_and_perft(board_class, fen, depth):
    # Initialize board
    try:
        board = board_class(setup=fen)
    except Exception as e:
        print(f"    Failed to init board: {e}")
        raise

    # Get legal moves at root
    root_moves = []

    # We must ensure we get moves from lmovegen suitable for this board
    # genAllMoves works on lboard.

    for m_int in lmovegen.genAllMoves(board.board):
        move_obj = Move(m_int)
        if logic.validate(board, move_obj):
            try:
                uci = move_to_uci(move_obj)
                root_moves.append(uci)
            except Exception as e:
                print(f"    Failed to convert move to UCI: {move_obj} - {e}")
                pass

    root_moves.sort()

    # Calculate Perft
    nodes = recursive_perft(board, depth)

    return nodes, root_moves

def recursive_perft(board, depth):
    if depth == 0:
        return 1

    nodes = 0
    lboard = board.board

    moves = list(lmovegen.genAllMoves(lboard))

    for m_int in moves:
        move_obj = Move(m_int)
        if logic.validate(board, move_obj):
            if depth == 1:
                nodes += 1
            else:
                try:
                    new_board = board.move(move_obj)
                    nodes += recursive_perft(new_board, depth - 1)
                except AssertionError as e:
                    # Debugging info
                    print(f"AssertionError in board.move: {e}")
                    print(f"Move: {move_obj} (Flag: {move_obj.flag})")
                    print(f"Board FEN: {board.asFen()}")
                    print(f"Cord0: {move_obj.cord0} - Piece at Cord0: {board[move_obj.cord0]}")
                    raise

    return nodes

def main():
    print("Generating PyChess Data...")

    with open("test/PyChessData.hs", "w") as f:
        f.write("module PyChessData where\n\n")
        f.write("data PyChessCase = PyChessCase {\n")
        f.write("    variant :: String\n")
        f.write("  , fen :: String\n")
        f.write("  , depth :: Int\n")
        f.write("  , nodes :: Int\n")
        f.write("  , moves :: [String]\n")
        f.write("  } deriving (Show, Eq)\n\n")
        f.write("cases :: [PyChessCase]\n")
        f.write("cases = [\n")

        all_cases = []

        for variant in VARIANTS:
            print(f"Processing Variant: {variant}")
            if variant not in name2variant:
                print(f"  WARNING: Variant {variant} not found in PyChess")
                continue

            board_class = name2variant[variant]
            positions = POSITIONS_VARIANTS.get(variant, [])

            for name, fen, depth in positions:
                print(f"  - {name} (Depth {depth})")
                try:
                    nodes, legal_moves = get_legal_moves_and_perft(board_class, fen, depth)

                    fen_esc = fen.replace('"', '\\"')
                    moves_str = ",".join('"' + m + '"' for m in legal_moves)

                    case_str = f"  PyChessCase \"{variant}\" \"{fen_esc}\" {depth} {nodes} [{moves_str}]"
                    all_cases.append(case_str)
                except Exception as e:
                    print(f"    ERROR: {e}")
                    # import traceback
                    # traceback.print_exc()

        f.write(",\n".join(all_cases))
        f.write("\n  ]\n")

    print("test/PyChessData.hs generated.")

if __name__ == "__main__":
    main()
