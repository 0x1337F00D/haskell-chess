{-# LANGUAGE BangPatterns #-}
module Chess.Engine.TT where

import Data.Word
import Data.Bits
import qualified Data.Vector.Unboxed.Mutable as UM
import Control.Monad (when)

import Chess.Types (Move(..), Square(..), PieceType(..))

-- | Transposition Table Entry Flags
data TTFlag = TTExact | TTLower | TTUpper
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

-- | Pack Move into 16 bits.
-- Standard: 0 (1 bit) | Prom (3 bits) | To (6 bits) | From (6 bits) -> 16 bits
-- Drop:     1 (1 bit) | Piece (3 bits) | To (6 bits) | Unused (6 bits) -> 16 bits
-- Note: Prom needs to encode Nothing (0) + 4 types (Knight, Bishop, Rook, Queen).
-- We can map: 0=None, 1=Knight, 2=Bishop, 3=Rook, 4=Queen.
-- 3 bits is enough (0-7).
packMove :: Move -> Word16
packMove (Move (Square f) (Square t) p) =
    let pVal :: Int
        pVal = case p of
                 Nothing -> 0
                 Just Knight -> 1
                 Just Bishop -> 2
                 Just Rook   -> 3
                 Just Queen  -> 4
                 Just _      -> 0 -- King/Pawn promotion impossible
        val = (fromIntegral f) .|.
              ((fromIntegral t) `shiftL` 6) .|.
              ((fromIntegral pVal) `shiftL` 12)
    in val
packMove (DropMove pt (Square t)) =
    let pVal = fromEnum pt -- 0-5
        val = (fromIntegral t) `shiftL` 6 .|.
              ((fromIntegral pVal) `shiftL` 12) .|.
              (1 `shiftL` 15) -- Set Drop bit
    in val
packMove NullMove = 0 -- Should not store NullMove usually

unpackMove :: Word16 -> Move
unpackMove w =
    if testBit w 15
    then -- Drop Move
        let t = (w `shiftR` 6) .&. 0x3F
            p = (w `shiftR` 12) .&. 0x7
            pt = toEnum (fromIntegral p)
        in DropMove pt (Square (fromIntegral t))
    else -- Standard Move
        let f = w .&. 0x3F
            t = (w `shiftR` 6) .&. 0x3F
            p = (w `shiftR` 12) .&. 0x7
            prom = case p of
                     1 -> Just Knight
                     2 -> Just Bishop
                     3 -> Just Rook
                     4 -> Just Queen
                     _ -> Nothing
        in Move (Square (fromIntegral f)) (Square (fromIntegral t)) prom

-- | Pack Data into 64 bits.
-- Move (16) | Score (16) | Depth (8) | Flag (2) | Age (8) | Unused (14)
packData :: Move -> Int -> Int -> TTFlag -> Int -> Word64
packData m score depth flag age =
    let mW = fromIntegral (packMove m) :: Word64
        sW = fromIntegral (score + 32768) :: Word64 -- Bias to make positive
        dW = fromIntegral depth :: Word64
        fW = fromIntegral (fromEnum flag) :: Word64
        aW = fromIntegral age :: Word64
    in mW .|.
       (sW `shiftL` 16) .|.
       (dW `shiftL` 32) .|.
       (fW `shiftL` 40) .|.
       (aW `shiftL` 42)

unpackData :: Word64 -> (Move, Int, Int, TTFlag, Int)
unpackData w =
    let m = unpackMove (fromIntegral (w .&. 0xFFFF))
        s = fromIntegral ((w `shiftR` 16) .&. 0xFFFF) - 32768
        d = fromIntegral ((w `shiftR` 32) .&. 0xFF)
        f = toEnum (fromIntegral ((w `shiftR` 40) .&. 0x3))
        a = fromIntegral ((w `shiftR` 42) .&. 0xFF)
    in (m, s, d, f, a)

-- | Probe the TT.
probeTT :: TT -> Word64 -> IO (Maybe (Move, Int, Int, TTFlag))
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
storeTT :: TT -> Word64 -> Int -> Int -> TTFlag -> Move -> IO ()
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
