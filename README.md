# haskell-chess

A Haskell port of a Python chess library. The code is organised into several
modules that mirror the logical parts of the engine.

## Module Overview

- **Chess.Types**: Fundamental, non-board specific types such as colors,
  piece kinds and square utilities.
- **Chess.Bitboard**: Raw 64‑bit bitboard manipulations and precomputed
  attack tables.
- **Chess.SquareSet**: A wrapper over bitboards providing set style
  operations on squares.
- **Chess.Board.Base**: Static piece layout and direct queries such as
  attacks from a given square.
- **Chess.Board.GameState**: Turn, castling rights, en passant square and
  other state needed to play a game.
- **Chess.Board.Fen**: Serialisation and deserialisation from Forsyth‑Edwards
  notation.
- **Chess.Board.MoveGen**: Move generation logic.
- **Chess.Board.San / Chess.Board.Uci**: SAN and UCI notation handling.
- **Chess.Board.Validation**: Legality checks and game status functions.
- **Chess.Board**: The primary board type combining all of the above and
  exposing the public API.
- **Chess**: Convenience module re-exporting the most commonly used types and
  functions.

## Development Phases

The project is being translated feature by feature. Planned phases include:

1. Core data types and constants (`Chess.Types`)
2. Bitboard primitives (`Chess.Bitboard`)
3. Square utilities and attack tables
4. `SquareSet` wrapper
5. Base board representation
6. Full board state management
7. Move generation
8. Game over detection
9. Notation handling (SAN/UCI)
10. Final polish and API consolidation

Each phase will expand the library and be accompanied by tests in the
`test` directory.

## Running Tests

The project uses `cabal` and `hspec` for testing. From the repository root:

```bash
cabal test
```

A GitHub Actions workflow runs this command automatically for each push and
pull request.
