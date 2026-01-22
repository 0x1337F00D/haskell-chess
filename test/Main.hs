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
import qualified CorePerftSpec
import qualified CoreFenSpec
import qualified EngineSpec
import qualified TablebaseSpec
import qualified PyChessCongruencySpec
import qualified VariantSpec
import qualified Chess960Spec

main :: IO ()
main = hspec $ do
  VariantSpec.spec
  Chess960Spec.spec
  PyChessCongruencySpec.spec
  EngineSpec.spec
  TablebaseSpec.spec
  CoreSpec.spec
  CorePerftSpec.spec
  CoreFenSpec.spec
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
  PerftSpec.spec -- Enabled
  GameTerminationSpec.spec
  PgnSpec.spec
  Book.PolyglotSpec.spec
  TimeSpec.spec
