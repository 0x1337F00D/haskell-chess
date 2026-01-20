module Main (main) where

import Test.Hspec

import qualified ChessSpec
import qualified TypesSpec
import qualified BitboardSpec
import qualified SquareSetSpec
import qualified Board.BaseSpec
import qualified Board.GameStateSpec
import qualified Board.FenSpec
import qualified Board.MoveGenSpec
import qualified Board.ValidationSpec
import qualified Board.UciSpec
import qualified Board.SanSpec
import qualified PerftSpec
import qualified PgnSpec
import qualified GameTerminationSpec
import qualified Book.PolyglotSpec
import qualified TimeSpec
import qualified CoreSpec
import qualified TablebaseSpec

main :: IO ()
main = hspec $ do
  TablebaseSpec.spec
  CoreSpec.spec
  ChessSpec.spec
  TypesSpec.spec
  BitboardSpec.spec
  SquareSetSpec.spec
  Board.BaseSpec.spec
  Board.GameStateSpec.spec
  Board.FenSpec.spec
  Board.MoveGenSpec.spec
  Board.ValidationSpec.spec
  Board.UciSpec.spec
  Board.SanSpec.spec
  -- PerftSpec.spec -- Disabled to prevent CI OOM (exit code 143)
  GameTerminationSpec.spec
  PgnSpec.spec
  Book.PolyglotSpec.spec
  TimeSpec.spec
