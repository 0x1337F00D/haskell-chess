# haskell-chess

A Haskell port of the [PyChess](https://github.com/pychess/pychess) library. This project aims to provide a robust, efficient chess library in Haskell, mirroring the features of PyChess while leveraging Haskell's strong type system and performance.

## Project Status

| Component | Progress | Status |
| :--- | :--- | :--- |
| **Core Rules** | `[██████████]` 100% | ✅ Implemented (Bitboards, MoveGen, Validation) |
| **Formats** | `[██████████]` 100% | ✅ Implemented (FEN ✅, PGN ✅, UCI ✅) |
| **Engine** | `[██████████]` 100% | ✅ Implemented (Search/Eval) |
| **Variants** | `[████████░░]` 80% | 🚧 Atomic, KotH, RacingKings, ThreeCheck |
| **Extras** | `[██████████]` 100% | ✅ Implemented (Books, TB, Time) |

## Feature Comparison

We aim to reach feature parity with PyChess. Here is the current standing:

| Feature | haskell-chess | PyChess | Notes |
| :--- | :---: | :---: | :--- |
| **Board Representation** | ✅ Bitboards | ✅ Object + C++ | Haskell uses efficient 64-bit bitboards. |
| **Move Generation** | ✅ Implemented | ✅ Implemented | Supports Standard and Atomic (via Type Class). |
| **Move Validation** | ✅ Full Legality | ✅ Full Legality | Includes checks, castling, en passant. |
| **FEN Support** | ✅ Read/Write | ✅ Read/Write | Full support for all 6 FEN fields. |
| **PGN Support** | ✅ Full Parsing | ✅ Full Support | Support for game trees, comments, NAGs, and variations. |
| **UCI Support** | ✅ Implemented | ✅ Full Engine Mgr | Haskell engine handles standard UCI loop. |
| **Opening Books** | ✅ Polyglot | ✅ Polyglot/BIN | Read support for .bin books. |
| **Endgame Tablebases** | ✅ Online & Local | ✅ Gaviota/Syzygy | Queries Lichess API. Local probing supported. |
| **Time Controls** | ✅ Implemented | ✅ Full Support | Infinite, Standard, Delay, MoveTime. |
| **Variants** | 🚧 Atomic, KotH, RacingKings, ThreeCheck | ✅ 30+ Variants | Atomic, Crazyhouse, Shogi, etc. |
| **Evaluation** | ✅ Implemented | ✅ Basic Eval | Material + PSTO evaluation. |
| **Search** | ✅ Implemented | ✅ Alpha-Beta + ID | Alpha-Beta + Quiescence + ID. |

## Roadmap & Future Tasks

To achieve parity with PyChess, the following tasks are identified:

### 1. File Formats & Protocols
- [x] **Advanced PGN**: Build a proper game tree from PGN, handle NAGs, comments, and recursive variations.
- [x] **Full UCI**: Implement a full UCI engine manager to run and interact with external engines.

### 2. Knowledge & Data
- [x] **Opening Books**: Implement `Polyglot` (.bin) book reading support.
- [x] **Endgame Tablebases**: Add support for probing Syzygy (or Gaviota) tablebases. (Implemented via Online API & Local Fathom)

### 3. Engine Features
- [x] **Evaluation**: Implement a static evaluation function (material, position).
- [x] **Search**: Implement Alpha-Beta search (Iterative Deepening implemented).
- [x] **Time Management**: Implement time control logic (time left, increment, move time).

### 4. Variants (Long Term)
- [x] Refactor `Board` and `MoveGen` to support non-standard rules (Implemented `ChessVariant` typeclass).
- [x] Implement popular variants:
    - [x] Atomic
    - [x] King of the Hill
    - [x] Racing Kings
    - [x] Three-Check
    - [x] Crazyhouse

## Supported Variants

Comparison of variants supported by PyChess vs haskell-chess:

| Variant | PyChess | haskell-chess | Notes |
| :--- | :---: | :---: | :--- |
| **Standard** | ✅ | ✅ | Full support. |
| **Atomic** | ✅ | ✅ | Explosions implemented. |
| **King of the Hill** | ✅ | ✅ | Center mate implemented. |
| **Racing Kings** | ✅ | ✅ | No check rule implemented. |
| **Three-Check** | ✅ | ✅ | Win by 3 checks implemented. |
| **Crazyhouse** | ✅ | ✅ | Drop Moves implemented. |
| **Chess960** | ✅ | ✅ | Implemented (Fischer Random). |
| **Antichess** | ✅ | ❌ | (Giveaway/Suicide/Losers) Pending. |
| **Horde** | ✅ | ❌ | Pending. |

## Architecture

This project is evolving towards a Type-Safe Architecture to ensure correctness by construction, leveraging Haskell's advanced type system features like GADTs and DataKinds.
See [ARCHITECTURE.md](ARCHITECTURE.md) for a deep dive into the design.

## Module Overview

- **Chess.Core.***: The new type-safe core modules (Board, Game, Move, Rules).
- **Chess.Types**: Fundamental, non-board specific types such as colors, piece kinds and square utilities.
- **Chess.Bitboard**: Raw 64‑bit bitboard manipulations and precomputed attack tables.
- **Chess.SquareSet**: A wrapper over bitboards providing set style operations on squares.
- **Chess.Board.Base**: Static piece layout and direct queries such as attacks from a given square.
- **Chess.Board.GameState**: Turn, castling rights, en passant square and other state needed to play a game.
- **Chess.Board.Fen**: Serialisation and deserialisation from Forsyth‑Edwards notation.
- **Chess.Board.MoveGen**: Move generation logic.
- **Chess.Board.San / Chess.Board.Uci**: SAN and UCI notation handling.
- **Chess.Board.Validation**: Legality checks and game status functions.
- **Chess.Tablebase**: Online Syzygy tablebase probing via Lichess API.
- **Chess.Pgn**: PGN parsing logic.
- **Chess.Book.Polyglot**: Polyglot opening book reader.
- **Chess.Board**: The primary board type combining all of the above and exposing the public API.
- **Chess**: Convenience module re-exporting the most commonly used types and functions.

## Running Tests

The project uses `cabal` and `hspec` for testing. From the repository root:

```bash
cabal test
```

A GitHub Actions workflow runs this command automatically for each push and pull request.

> **Note**: The full perft suite (`PerftSpec`) is currently disabled by default to avoid high memory usage in CI environments. To run it, uncomment the relevant line in `test/Main.hs`.
