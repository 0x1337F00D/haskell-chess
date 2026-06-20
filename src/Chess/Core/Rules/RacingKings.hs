{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Chess.Core.Rules.RacingKings where

import Chess.Core.Rules.Class
import Chess.Core.Rules.Common
import Chess.Core.Board.Internal
import Chess.Core.Game.Internal
import Chess.Core.Move.Internal

import qualified Chess.Types as T
import qualified Chess.Board.GameState as GS
import qualified Chess.Board.MoveGen as MG
import qualified Chess.Board.Validation as Val

instance VariantMoveGen 'RacingKings where
  generateMoves (ag :: ActiveGame 'RacingKings c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag
        baseMoves = MG.legalGenMovesList baseBoard gs
        coreMoves = map toCoreMove baseMoves
        c = colorVal @c
        oppC = opposite c

        noGiveCheck m =
            let baseNext = applyMoveBase m baseBoard
            in not (Val.isCheck baseNext (dummyGameState oppC))
          where
            dummyGameState col = GS.initialGameState { GS.turn = toColor col }

    in filter noGiveCheck coreMoves

instance VariantMoveApply 'RacingKings where
  applyMove = genericApplyMove

instance VariantMoveExecute 'RacingKings where
  executeMove (m :: Move c) (ag :: ActiveGame 'RacingKings c s) =
    case applyMove m ag of
      Transition nextAg ->
        let
           c = colorVal @c
           internalB' = internalBoard nextAg

           -- Racing Kings doesn't really have check?
           -- "noGiveCheck" in generateMoves ensures no check is GIVEN.
           -- So nextAg is always Safe?
           -- genericExecuteMove calculates check status.
           -- If I use genericExecuteMove, it will run isCheck.
           -- In Racing Kings, isCheck is probably always False if moves are filtered correctly.
           -- BUT the win condition logic is complex.
           -- I should replicate the logic here.

           -- We need to know if nextAg has moves (Stalemate check).
           -- But we also need to check win conditions.

           nextAgSafe = setStatus SSafe nextAg -- Assuming no checks.
           nextMoves = generateMoves nextAgSafe
           realHasMoves = not (null nextMoves)

           wKingSq = if MG.hasKing internalB' T.White then Just (MG.kingSquareFast internalB' T.White) else Nothing
           bKingSq = if MG.hasKing internalB' T.Black then Just (MG.kingSquareFast internalB' T.Black) else Nothing
           wInGoal = case wKingSq of Just sq -> T.squareRank sq == 7; _ -> False
           bInGoal = case bKingSq of Just sq -> T.squareRank sq == 7; _ -> False

           -- Use the standard classification for fallback
           replies = if realHasMoves then HasReplies else NoReplies

           result =
                if c == White
                then if wInGoal
                     then if realHasMoves
                          then Continue nextAgSafe
                          else Checkmate (Winner White)
                     else statusToMoveResult @'RacingKings @c nextAg (SafePos replies)
                else
                     if bInGoal && wInGoal then Checkmate Draw else
                     if wInGoal then Checkmate (Winner White) else
                     if bInGoal then Checkmate (Winner Black) else
                     statusToMoveResult @'RacingKings @c nextAg (SafePos replies)
        in result

  perftExecuteMove (m :: Move c) (ag :: ActiveGame 'RacingKings c s) =
    case applyMove m ag of
      Transition nextAg ->
        let
           c = colorVal @c
           internalB' = internalBoard nextAg

           wKingSq = if MG.hasKing internalB' T.White then Just (MG.kingSquareFast internalB' T.White) else Nothing
           bKingSq = if MG.hasKing internalB' T.Black then Just (MG.kingSquareFast internalB' T.Black) else Nothing
           wInGoal = case wKingSq of Just sq -> T.squareRank sq == 7; _ -> False
           bInGoal = case bKingSq of Just sq -> T.squareRank sq == 7; _ -> False

        in if c == White
           then if wInGoal
                then Continue (nextAg { checkStatus = SUnchecked })
                else Continue (nextAg { checkStatus = SUnchecked })
           else
                if bInGoal && wInGoal then Checkmate Draw else
                if wInGoal then Checkmate (Winner White) else
                if bInGoal then Checkmate (Winner Black) else
                Continue (nextAg { checkStatus = SUnchecked })

instance VariantPerft 'RacingKings

instance ChessVariant 'RacingKings
