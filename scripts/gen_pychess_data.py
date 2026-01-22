import sys
import os

# Add pychess_repo to path
sys.path.append(os.path.abspath("pychess_repo/lib"))

from pychess.Utils.lutils.LBoard import LBoard
from pychess.Utils.lutils.lmovegen import genAllMoves
from pychess.Utils.lutils.lmove import toAN
from pychess.Utils.const import (
    FEN_START, CASTLE_KK,
    NORMALCHESS, ATOMICCHESS, KINGOFTHEHILLCHESS, RACINGKINGSCHESS, THREECHECKCHESS, CRAZYHOUSECHESS
)

# Racing Kings Start FEN (from pychess source)
RACINGKINGS_START = "8/8/8/8/8/8/krbnNBRK/qrbnNBRQ w - - 0 1"

def get_moves_clean(fen, variant):
    board = LBoard(variant)
    try:
        board.applyFen(fen)
    except Exception as e:
        print(f"Error applying FEN {fen} for variant {variant}: {e}")
        return []

    moves = []
    for move in genAllMoves(board):
        # Check legality
        # PyChess genAllMoves generates pseudo-legal moves?
        # LBoard has applyMove which updates state.
        # We need to verify if the move is legal (doesn't leave king in check etc).

        # Note: LBoard logic might handle some legality but let's be safe
        board.applyMove(move)

        is_illegal = False
        if board.opIsChecked(): # If we moved and our opponent is now "checked" (meaning we left ourself in check or illegal state)
             # Wait, opIsChecked() checks if the side TO MOVE (opponent of who just moved) is checked.
             # If I move, and then it's opponent's turn. If *I* am in check, that's illegal.
             # LBoard maintains 'color' as side to move.
             # After applyMove, color switches.
             # So opIsChecked() checks if the PREVIOUS mover (us) is checked.
             is_illegal = True

        # Special Variant Rules that might not be fully covered by opIsChecked or need specific handling?
        # Racing Kings: cannot give check.
        if variant == RACINGKINGSCHESS:
             # Logic: cannot give check.
             # After I move, it is opponent's turn.
             # If opponent is checked, then I gave check.
             if board.isChecked(): # isChecked checks if current side (opponent) is checked
                 is_illegal = True

             # Also cannot move King into check (covered by opIsChecked usually)

        board.popMove()

        if is_illegal:
            continue

        # Now get UCI. board is at state BEFORE move.
        # Crazyhouse uses P@e4 notation. toAN should handle it if set up right.
        uci = toAN(board, move, short=True, castleNotation=CASTLE_KK)
        moves.append(uci)
    return sorted(moves)

def do_perft(board, depth, variant):
    nodes = 0
    if depth == 0:
        return 1

    for move in genAllMoves(board):
        board.applyMove(move)

        is_illegal = False
        if board.opIsChecked():
             is_illegal = True

        if variant == RACINGKINGSCHESS:
             if board.isChecked():
                 is_illegal = True

        if is_illegal:
            board.popMove()
            continue

        nodes += do_perft(board, depth - 1, variant)
        board.popMove()
    return nodes

def get_perft(fen, depth, variant):
    board = LBoard(variant)
    board.applyFen(fen)
    return do_perft(board, depth, variant)

# Define Cases
# (Name, VariantConst, [(CaseName, FEN, Depth)])
cases_def = [
    ("Standard", NORMALCHESS, [
        ("Start", FEN_START, 3),
        ("KiwiPete", "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 2),
    ]),
    ("Atomic", ATOMICCHESS, [
        ("Start", FEN_START, 3),
        ("Explosion", "rnbqkbnr/pppppppp/8/8/8/4P3/PPPP1PPP/RNBQKBNR b KQkq - 0 1", 3), # Black moves d5, e5 etc
    ]),
    ("KingOfTheHill", KINGOFTHEHILLCHESS, [
        ("Start", FEN_START, 3),
        ("CenterWin", "8/8/8/4k3/3K4/8/8/8 w - - 0 1", 1), # White King can step into center
    ]),
    ("RacingKings", RACINGKINGSCHESS, [
        ("Start", RACINGKINGS_START, 2),
    ]),
    ("ThreeCheck", THREECHECKCHESS, [
        ("Start", FEN_START, 3),
    ]),
    ("Crazyhouse", CRAZYHOUSECHESS, [
        ("Start", FEN_START, 3),
        ("Drop", "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1", 2),
    ])
]

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

    all_cases_str = []

    for var_name, var_const, position_list in cases_def:
        for name, fen, depth in position_list:
            print(f"Processing {var_name} - {name}...")
            nodes = get_perft(fen, depth, var_const)
            moves = get_moves_clean(fen, var_const)
            fen_esc = fen.replace('"', '\\"')
            moves_str = ",".join(['"' + m + '"' for m in moves])

            case_str = f"  PyChessCase \"{var_name}\" \"{fen_esc}\" {depth} {nodes} [{moves_str}]"
            all_cases_str.append(case_str)

    f.write(",\n".join(all_cases_str))
    f.write("\n  ]\n")

print("test/PyChessData.hs generated.")
