{-# LANGUAGE BangPatterns #-}
module Chess.Engine.TT where

import Data.Word
import Data.Bits
import Data.Coerce (coerce)
import qualified Data.Vector.Storable.Mutable as UM
import Control.Monad (when)

<<<<<<< HEAD
import Chess.Types (Move(..), Depth(..))
=======
import Chess.Types (Move(..), Square(..), PieceType(..), Depth(..))
>>>>>>> origin/main

-- | Transposition Table Entry Flags
-- TTEval is for storing static evaluation (depth 0).
data TTFlag = TTExact | TTLower | TTUpper | TTEval
    deriving (Eq, Show, Enum)

-- | Transposition Table
-- We store Key (64-bit) and Data (64-bit) interleaved.
-- Size is number of entries.
data TT = TT
    { ttData :: !(UM.IOVector Word64)
    , ttMask :: !Int
    }

-- | Create a new Transposition Table with 2^n entries.
newTT :: Int -> IO TT
newTT sizeBits = do
    let size = 1 `shiftL` sizeBits
    v <- UM.replicate (size * 2) 0
    return $ TT v (size - 1)

-- | Clear the TT.
clearTT :: TT -> IO ()
clearTT (TT v _) = UM.set v 0

-- | Pack Data into 64 bits.
-- Move (16) | Score (16) | Depth (8) | Flag (2) | Age (8) | Unused (14)
packData :: Move -> Int -> Depth -> TTFlag -> Int -> Word64
packData m score depth flag age =
    let mW = fromIntegral (coerce m :: Word16) :: Word64
        sW = fromIntegral (score + 32768) :: Word64 -- Bias to make positive
        dW = fromIntegral (unDepth depth) :: Word64
        fW = fromIntegral (fromEnum flag) :: Word64
        aW = fromIntegral age :: Word64
    in mW .|.
       (sW `shiftL` 16) .|.
       (dW `shiftL` 32) .|.
       (fW `shiftL` 40) .|.
       (aW `shiftL` 42)

unpackData :: Word64 -> (Move, Int, Depth, TTFlag, Int)
unpackData w =
    let m = coerce (fromIntegral (w .&. 0xFFFF) :: Word16) :: Move
        s = fromIntegral ((w `shiftR` 16) .&. 0xFFFF) - 32768
        d = Depth (fromIntegral ((w `shiftR` 32) .&. 0xFF))
        f = toEnum (fromIntegral ((w `shiftR` 40) .&. 0x3))
        a = fromIntegral ((w `shiftR` 42) .&. 0xFF)
    in (m, s, d, f, a)

-- | Probe the TT.
probeTT :: TT -> Word64 -> IO (Maybe (Move, Int, Depth, TTFlag))
probeTT (TT v mask) key = do
    let idx = (fromIntegral key .&. mask) * 2
    entryKey <- UM.unsafeRead v idx
    if entryKey == key
    then do
        entryData <- UM.unsafeRead v (idx + 1)
        let (m, s, d, f, _) = unpackData entryData
        return $ Just (m, s, d, f)
    else return Nothing

-- | Store in TT.
-- Replacement strategy: Depth-preferred or Always replace?
-- Simple: Always replace (or Depth-preferred).
-- We'll use depth-preferred + age (not implemented yet, assuming same age).
storeTT :: TT -> Word64 -> Depth -> Int -> TTFlag -> Move -> IO ()
storeTT (TT v mask) key depth score flag move = do
    let idx = (fromIntegral key .&. mask) * 2
    -- Read old entry to decide replacement
    oldKey <- UM.unsafeRead v idx
    oldData <- UM.unsafeRead v (idx + 1)
    let (_, _, oldDepth, _, _) = unpackData oldData

    -- Replace if:
    -- 1. Empty (oldKey == 0, assumption)
    -- 2. Different key (collision) -> Always replace? Or depth?
    --    Modern engines usually have buckets or replace if depth >= oldDepth.
    -- 3. Same key -> Replace if depth >= oldDepth.

    let replace = oldKey /= key || depth >= oldDepth

    when replace $ do
        UM.unsafeWrite v idx key
        UM.unsafeWrite v (idx + 1) (packData move score depth flag 0)
