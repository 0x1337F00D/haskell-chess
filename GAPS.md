# Identified Gaps to Pychess

## Missing Variants
The following variants supported by Pychess are not implemented in `haskell-chess`:
*   **Giveaway** (Antichess)
*   **Suicide**
*   **Losers**
*   **Fischerandom** (Chess960)
*   **Horde**

## Architectural Limitations

### King Presence Enforcement
The core `Board` structure (`Chess.Core.Board.Internal`) enforces the presence of exactly one King per side:
```haskell
data Board = Board
  { whiteKing   :: Square
  , blackKing   :: Square
  ...
  }
```
This prevents the implementation of **Giveaway**, **Suicide**, and **Losers**, where Kings can be captured and the game ends when a player loses all pieces (or cannot move). To support these, the `Board` type must be refactored to allow missing Kings or Kings to be treated as non-royal pieces.

### Castling Logic
The castling logic in `Chess.Core.Rules` is hardcoded for Standard Chess positions (e.g., King on e1, Rooks on a1/h1):
```haskell
getCastlingRookMove (Square FileE Rank1) (Square FileG Rank1) = ...
```
**Fischerandom** (Chess960) requires flexible castling logic where the Rook's destination depends on the specific starting position and the target square. The `CastlingRights` data structure may also need adjustment to track specific Rooks rather than side-based rights.

### Racing Kings Checks
While **Racing Kings** is implemented, the check detection logic currently relies on standard `Val.isCheck`. Racing Kings forbids checks entirely. The implementation in `Rules.hs` filters out moves that give check, but `gameFromFEN` uses `Val.isCheck` to determine initial status. If a FEN is loaded that contains a check, `gameFromFEN` might incorrectly mark it as `Checked` (which is an illegal state in RK).

## Validation Coverage
Congruency tests have been enabled for `Standard`, `Atomic`, `KingOfTheHill`, `RacingKings`, `ThreeCheck`, and `Crazyhouse`. These tests pass for standard starting positions and selected critical positions. However, edge case coverage (especially for `Crazyhouse` drops and `Atomic` explosions) could be expanded.
