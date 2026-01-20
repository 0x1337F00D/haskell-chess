{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}

module Chess.Core.Board
  ( -- * Foundation
    Color(..)
  , File(..)
  , Rank(..)
  , PawnRank(..)
  , toRank
  , Square(..)
  , opposite
    -- * Pieces
  , PieceType(..)
  , Piece(..)
  , MajorMinorPiece(..)
  , SomePiece(..)
  , pieceColor
  , pieceType
  , toMajorMinor
    -- * Board
  , Board -- Opaque
  , whiteKing
  , blackKing
  , pawns
  , whitePieces
  , blackPieces
  , initialBoard
  , fromFEN
  , getPieceAt
  ) where

import Chess.Core.Board.Internal
