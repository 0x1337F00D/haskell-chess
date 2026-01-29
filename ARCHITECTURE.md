# Type-Safe Chess Engine Architecture

This document outlines a conceptual architecture for a Haskell chess engine designed for **correctness by construction**.

The design relies heavily on **Generalized Algebraic Data Types (GADTs)**, **DataKinds**, **Type Families**, and the **Opaque Data Pattern** to ensure that the compiler rejects logically invalid chess states.

## 0. Current Architecture in this Repo (Reality Check)

The repository employs a **Hybrid Architecture** composed of two complementary layers:

1. **Performance-Oriented Engine Layer (`Chess.Board.*`)**
   * **Bitboard-based representation**: Uses fast move generation, validation, and Zobrist hashing.
   * **Optimization**: Includes deferred check detection, `vector` based lookups, and Parallel Search.
   * **Scope**: Powers the search, evaluation, and format parsers (FEN/PGN/UCI).
   * **Types**: Uses `Chess.Types.Square` (0–63) and prioritizes execution speed.

2. **Type-Safe Core Layer (`Chess.Core.*`)**
   * **Type Safety**: Uses `DataKinds`, `GADTs`, and strongly-typed `File`, `Rank`, and `Square`.
   * **Hybrid State**: The `ActiveGame` type wraps the optimized `Chess.Board.Base.Board` (Bitboard) for its internal state, enabling O(1) updates while enforcing high-level invariants via the type system (turn order, check status).
   * **Invariants**: Encodes rules like **exactly one king per side**, pawn rank restrictions, and turn safety.
   * **Variants**: Logic is modeled via type classes (`ChessVariant`), and moves are represented as typed proofs.

The **bridge** logic in `Chess.Core.Rules.Common` converts between typed core structures and the bitboard representation where necessary, but the primary state (`ActiveGame`) now holds the bitboard directly to minimize conversion overhead during play.

**Recent improvements (implemented now):**
* **Internal Bitboard State**: `ActiveGame` uses `Chess.Board.Base.Board` directly, removing the overhead of the previous Map-based "Shadow Board".
* **Deferred Check Detection**: `ActiveGame` carries a `SCheckStatus` type index (`Safe | Checked | Unchecked`), allowing move generation to optimize based on known safety (e.g., skipping castling checks if known to be safe, or generating only evasions if checked).
* **Typed Newtypes**: `GameState` uses `HalfmoveClock` and `FullmoveNumber` newtypes; Search uses typed `Depth` to prevent accidental mixing of counters.

## 1. Foundation: The Finite Space
*Goal: Eliminate array bounds errors and invalid coordinate arithmetic.*

### Strongly Typed Coordinates
Instead of representing squares as integers (0-63), we define coordinates as the product of two finite, enumerated types.

*   **Ranks and Files as Finite Sets**: We define `File` (A..H) and `Rank` (1..8) as distinct sum types.
*   **Pawn-Specific Coordinates**: To satisfy the rule that pawns cannot exist on Rank 1 or Rank 8, we define a subset type `PawnRank` (2..7).
*   **The Board Topology**: The `Square` type is a tuple of `(File, Rank)`. Since `File` and `Rank` are closed sets, it is topologically impossible to construct a `Square` that is "off the board."

**Prevention Mechanism**: Array index out-of-bounds errors are eliminated. A function accepting a `Square` is guaranteed to receive a valid board position.

## 2. The Physical Board: Structural Invariants
*Goal: Enforce piece placement rules (e.g., exactly one King per side).*

### The Piece GADT
We use a GADT to link pieces to their intrinsic properties at the type level.

*   **Color Indexing**: `data Piece (c :: Color)` ensures a White Knight cannot be treated as Black.
*   **Piece Classification**: Distinct constructors for `King`, `Pawn`, and `Slider` allow functions to enforce logic specific to movement patterns (e.g., sliding vs. stepping) via pattern matching, which the compiler checks for exhaustiveness.

### The Composite Board Structure
A raw 64-square array allows illegal states like "zero kings" or "three kings." We replace this with a composite structure (used for setup and validation):

1.  **King Registry**: `whiteKing :: Square, blackKing :: Square`.
    *   This record enforces the invariant: *There is always exactly one King per side.*
2.  **Pawn Map**: `Map (File, PawnRank) (Color)`.
    *   By using `PawnRank` as the key, it is statically impossible to place a pawn on a promotion rank or back rank.
3.  **Piece Maps**: `whitePieces :: Map Square (MajorMinorPiece 'White)` and `blackPieces :: Map Square (MajorMinorPiece 'Black)`.

**Note**: This Map-based structure (`Chess.Core.Board.Internal.Board`) is primarily used for **Setup** and **Validation** (loading FEN, verifying integrity). Once the game transitions to the `Active` phase, the internal representation switches to the optimized `Chess.Board.Base.Board` (Bitboard) wrapped in `ActiveGame` for performance, while the type system maintains the invariants established during setup.

**Prevention Mechanism**: It is impossible to represent a board state where a King is captured (missing) or where pawns exist on invalid ranks during the Setup phase, and the Active phase preserves these properties via legal move generation.

## 3. Game Phases as Type States
*Goal: Restrict operations based on the lifecycle of the game.*

We employ **DataKinds** to index the main game wrapper, creating a state machine enforced by the type system.

*   **Phases**: `type Phase = Setup | Active | Finished`.
*   **The Game Container**: `data Game (p :: Phase) ...`.

### Phase Transitions
*   **Setup Phase**: Allows arbitrary placement of pieces (validated for consistency). Transitioning to `Active` requires a proof of validity (e.g., kings present, side-to-move not in checkmate).
*   **Active Phase**: The only phase where `makeMove` is callable.
*   **Finished Phase**: Contains the result (Mate, Draw). No move functions exist for this type.

**Prevention Mechanism**: You cannot accidentally ask the engine to make a move on a game that has already ended or is currently being set up.

## 4. Turn Safety and Dynamic State
*Goal: Enforce turn order and prevent moving the opponent's pieces.*

### The Active Game State
The `Active` game state is indexed by the current turn: `data ActiveGame (v :: Variant) (turn :: Color) (status :: CheckStatus)`.

*   **Turn Enforcement**: Move generation functions have the signature:
    `ActiveGame v c s -> [Move c]`
    The `Move` type is also indexed by `c`. It is a type error to apply a `Move White` to a `ActiveGame v 'Black s`.
*   **The Transition**: Applying a move flips the type index:
    `apply :: Move c -> ActiveGame c -> NextState (Opposite c)`

**Prevention Mechanism**: The "wrong turn" class of bugs is eliminated. The compiler ensures that White only moves during White's turn.

## 5. Moves as Constructed Proofs
*Goal: Prevent illegal moves from being represented or applied.*

A `Move` is not just a data structure (start, end); it is a **Witness** produced by a trusted kernel.

### Opaque Move Types
The `Move` type is exported abstractly. Users cannot construct a `Move` manually (e.g., `Move "e2" "e5"`). They must use a generator:
`generateLegalMoves :: ActiveGame c -> [Move c]`

### Variant-Specific Constructors (Internal)
Internally, the `Move` type utilizes a GADT to model rule-specific logic:

*   **`StandardMove`**: Carries proof that the path is clear (for sliders) or the step is valid.
*   **`CastlingMove`**: Only constructible if `CastlingRights` are present in the Game State and the path is clear.
*   **`EnPassantMove`**: Requires the Game State to have a specific `EnPassantTarget` set from the previous turn.
*   **`PromotionMove`**: Requires the `from` square to be `PawnRank 7` (for White) and `to` square to be `Rank 8`.

**Prevention Mechanism**:
*   **Castling**: Cannot castle out of check or through check, because the constructor for `CastlingMove` validates these preconditions during generation.
*   **Promotion**: Impossible to create a "Knight Promotion" move for a pawn on Rank 3.
*   **Application**: The `applyMove` function is total. Since a `Move` acts as a proof of its own legality, applying it never fails.

## 6. Check, Mate, and the Existential Step
*Goal: Handle the uncertainty of the next state while preserving safety.*

When a move is made, the resulting state depends on complex logic (did this checkmate the opponent?). We cannot know the resulting type index statically, so we use an **Existential Wrapper**.

### The Next State
The result of `applyMove` is a wrapper type:

```haskell
data MoveResult (v :: Variant) (c :: Color) where
  Checkmate :: Outcome -> MoveResult v c
  Stalemate :: MoveResult v c
  Continue  :: ActiveGame v c status -> MoveResult v c
```

*   **Forcing Handling**: The user *must* pattern match on `MoveResult`.
    *   If `Checkmate`, they get a proof of victory.
    *   If `Stalemate`, the game ends.
    *   If `Continue`, they receive a new `ActiveGame` with the turn flipped, ready for the next recursion.

**Prevention Mechanism**: The engine cannot enter an undefined state after a move. The user is forced by the compiler to handle game termination explicitly.

## 7. Tactical States: Safe vs. Checked
*Goal: Encode King safety in the type system to optimize move generation.*

We refine the `ActiveGame` type to include the tactical status of the king using the `SCheckStatus` GADT:
`data ActiveGame (v :: Variant) (turn :: Color) (status :: CheckStatus)`

where `status` can be `Safe`, `Checked`, or `Unchecked`.

*   **Deferred Detection**: The engine can transition to `Unchecked` after a move, deferring the expensive check verification until necessary (or computing it lazily).
*   **Preconditions**: Castling is only permitted if `status ~ Safe` (or if verified to be safe).
*   **Filtering**: The move generator for `ActiveGame v c 'Checked` is optimized to only generate moves that resolve the check (evasions), significantly pruning the search tree.

## 8. Module Boundaries as Safety Barriers
*Goal: Isolate unsafe logic.*

The architecture relies on a strict Trusted Computing Base (TCB):

1.  **`Core.Board`**: Exports `Square`, `Piece`, and `Board`. Constructors for `Board` are hidden; only `fromFEN` (which returns `Maybe Board`) or `initialBoard` can create them.
2.  **`Core.Move`**: Exports `Move` as an opaque type.
3.  **`Core.Rules`**: Contains the complex logic. This is the *only* module allowed to call the internal constructors of `Move`.

**Prevention Mechanism**: "External" code (UI, Search Engine, UCI Adapter) cannot fabricate a fake board or a fake move. They are consumers of proofs generated by the Core.

## 9. Extensibility Without Weakening Guarantees
*Goal: Allow variants and extensions without breaking the core type safety.*

### Type Classes for Variants
We can parameterize the game by a `Variant` type:
`data ActiveGame (v :: Variant) (turn :: Color) (status :: CheckStatus)`

*   **Ad-hoc Polymorphism**: Move generation and validation logic are defined via type classes:
    `class ChessVariant v where generateMoves :: ...; executeMove :: ...`
*   **Extension**: Implementing "Atomic Chess" or "Chess960" involves creating a new empty data type `Atomic` and implementing the `ChessVariant` instance. The core machinery (Phase, Coordinates, Color) remains generic and safe.

### Unsafe Hooks for Search
Search engines require performance and may need to tentatively make "pseudo-legal" moves.
*   **Strict Boundary**: The Search module can import "Unsafe" primitives (like integer-based boards) but must marshal them back into Safe types before returning a result to the user.
*   **Phantom Types for Evaluation**: Evaluation functions can use phantom types to ensure they are only comparing positions from the same game variant.

## 10. Summary of Trade-offs

| Feature | Correctness Benefit | Complexity Cost |
| :--- | :--- | :--- |
| **Finite Coordinates** | No bounds checks ever. | "Square arithmetic" (e.g., `sq + 1`) requires explicit conversion to/from Enum. |
| **Split Board (Kings/Pawns)** | Structural invariants (1 King). | Board update logic is more complex than updating a single array. |
| **Opaque Moves** | Impossible to apply illegal moves. | Serialization (UCI/SAN) requires parsing functions that return `Maybe Move`. |
| **Type-Indexed Turn** | Impossible to move out of turn. | Game loop must handle existential types or continuation passing style. |

This architecture ensures that the vast majority of chess engine bugs—illegal moves, invalid states, and rule violations—are transformed into compilation errors. The runtime is reserved solely for chess logic (is this position mate?), not validity checks (is this index -1?).

## 11. Concrete Next Steps to Leverage More Haskell Power

The following improvements are practical extensions of the current codebase:

1. **Strengthen Opaque Boundaries in the Engine Layer**
   * Hide direct `Square` construction outside trusted modules.
   * Provide safe constructors (`square`) and explicit `unsafe` helpers for hot paths.
2. **Promote Typed Coordinates into the Engine**
   * Gradually replace raw `Int` file/rank arithmetic with finite `File`/`Rank` types.
   * Use `Bounded`/`Enum` instances for safe iteration over board coordinates.
3. **Move Proofs as First-Class Values**
   * Expand the typed `Move` GADT to cover more variants (Crazyhouse drops, Fischer Random castling).
   * Expose an opaque `Move` API in the engine layer so only legal moves can be applied externally.
4. **Type-Level Game State Guarantees**
   * Ensure FEN parsing produces a typed `Game 'Setup` or `Game 'Active` depending on validation.
   * Encode “in check / safe” as a type index in more public APIs (not just internal).
5. **Use Newtypes for Domain Counters and Indices**
   * Continue adding lightweight newtypes (like `HalfmoveClock` and `FullmoveNumber`) to prevent category mistakes without runtime cost.

These changes are incremental and compatible with the existing split architecture: the bitboard engine stays fast, while correctness and maintainability improve as more invariants become type-checked.
