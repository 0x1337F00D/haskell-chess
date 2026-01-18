# haskell-chess

A Haskell port of the [PyChess](https://github.com/pychess/pychess) library. This project aims to provide a robust, efficient chess library in Haskell, mirroring the features of PyChess while leveraging Haskell's strong type system and performance.

## Project Status

| Component | Progress | Status |
| :--- | :--- | :--- |
| **Core Rules** | `[██████████]` 100% | ✅ Implemented (Bitboards, MoveGen, Validation) |
| **Formats** | `[█████░░░░░]` 50% | 🚧 Partial (FEN ✅, PGN 🚧, UCI 🚧) |
| **Engine** | `[█░░░░░░░░░]` 10% | 🚧 Basic State (No Search/Eval) |
| **Variants** | `[░░░░░░░░░░]` 0% | ❌ Standard Chess Only |
| **Extras** | `[░░░░░░░░░░]` 0% | ❌ No Books/Tablebases |

## Feature Comparison

We aim to reach feature parity with PyChess. Here is the current standing:

| Feature | haskell-chess | PyChess | Notes |
| :--- | :---: | :---: | :--- |
| **Board Representation** | ✅ Bitboards | ✅ Object + C++ | Haskell uses efficient 64-bit bitboards. |
| **Move Generation** | ✅ Implemented | ✅ Implemented | Haskell is Standard-only for now. |
| **Move Validation** | ✅ Full Legality | ✅ Full Legality | Includes checks, castling, en passant. |
| **FEN Support** | ✅ Read/Write | ✅ Read/Write | Full support for all 6 FEN fields. |
| **PGN Support** | 🚧 Basic Parsing | ✅ Full Support | Haskell parses structure but lacks game tree building. |
| **UCI Support** | 🚧 Basic Parsing | ✅ Full Engine Mgr | Haskell supports basic command parsing. |
| **Opening Books** | ❌ Missing | ✅ Polyglot/BIN | Planned for future. |
| **Endgame Tablebases** | ❌ Missing | ✅ Gaviota/Syzygy | Planned for future. |
| **Time Controls** | ❌ Missing | ✅ Full Support | Only move counters implemented so far. |
| **Variants** | ❌ Standard Only | ✅ 30+ Variants | Atomic, Crazyhouse, Shogi, etc. |
| **Evaluation** | ❌ Missing | ✅ Basic Eval | No static evaluation function yet. |
| **Search** | ❌ Missing | ✅ Alphas/Beta | No search algorithm yet. |

## Roadmap & Future Tasks

To achieve parity with PyChess, the following tasks are identified:

### 1. File Formats & Protocols
- [ ] **Advanced PGN**: Build a proper game tree from PGN, handle NAGs, comments, and recursive variations.
- [ ] **Full UCI**: Implement a full UCI engine manager to run and interact with external engines.

### 2. Knowledge & Data
- [ ] **Opening Books**: Implement `Polyglot` (.bin) book reading support.
- [ ] **Endgame Tablebases**: Add support for probing Syzygy (or Gaviota) tablebases.

### 3. Engine Features
- [ ] **Evaluation**: Implement a static evaluation function (material, position).
- [ ] **Search**: Implement Alpha-Beta search with iterative deepening.
- [ ] **Time Management**: Implement time control logic (time left, increment, move time).

### 4. Variants (Long Term)
- [ ] Refactor `Board` and `MoveGen` to support non-standard rules.
- [ ] Implement popular variants: Atomic, Crazyhouse, King of the Hill, 3-Check.

## Module Overview

- **Chess.Types**: Fundamental, non-board specific types such as colors, piece kinds and square utilities.
- **Chess.Bitboard**: Raw 64‑bit bitboard manipulations and precomputed attack tables.
- **Chess.SquareSet**: A wrapper over bitboards providing set style operations on squares.
- **Chess.Board.Base**: Static piece layout and direct queries such as attacks from a given square.
- **Chess.Board.GameState**: Turn, castling rights, en passant square and other state needed to play a game.
- **Chess.Board.Fen**: Serialisation and deserialisation from Forsyth‑Edwards notation.
- **Chess.Board.MoveGen**: Move generation logic.
- **Chess.Board.San / Chess.Board.Uci**: SAN and UCI notation handling.
- **Chess.Board.Validation**: Legality checks and game status functions.
- **Chess.Board**: The primary board type combining all of the above and exposing the public API.
- **Chess**: Convenience module re-exporting the most commonly used types and functions.

## Running Tests

The project uses `cabal` and `hspec` for testing. From the repository root:

```bash
cabal test
```

A GitHub Actions workflow runs this command automatically for each push and pull request.
