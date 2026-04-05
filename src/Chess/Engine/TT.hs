{-# LANGUAGE BangPatterns #-}
module Chess.Engine.TT where

import Data.Word
import Data.Bits
import Data.Coerce (coerce)
import qualified Data.Vector.Storable.Mutable as UM
import Control.Monad (when)

import Chess.Types (Move(..), Depth(..))

-- | Transposition Table Entry Flags
-- TTEval is for storing static evaluation (depth 0).
data TTFlag = TTExact | TTLower | TTUpper | TTEval
    deriving (Eq, Show, Enum)

-- | Transposition Table
-- We store Key (64-bit) and Data (64-bit) interleaved.
-- Size is number of entries.
data TT = TT
    { ttData :: {-# UNPACK #-} !(UM.IOVector Word64)
    , ttMask :: !Int
    }

-- | Create a new Transposition Table with 2^n entries.
newTT :: Int -> IO TT
newTT sizeBits = do
    let size = 1 `shiftL` sizeBits
    v <- UM.replicate (size `shiftL` 1) 0
    return $ TT v (size - 1)

-- | Clear the TT.
clearTT :: TT -> IO ()
clearTT (TT v _) = UM.set v 0

-- | Pack Data into 64 bits.
-- Move (16) | Score (16) | Depth (8) | Flag (2) | Age (8) | Unused (14)
{-# INLINE packData #-}
packData :: Move -> Int -> Int -> TTFlag -> Int -> Word64
packData m score depthVal flag age =
    let !mW = fromIntegral (coerce m :: Word16) :: Word64
        !sW = fromIntegral (score + 32768) :: Word64 -- Bias to make positive
        !dW = fromIntegral depthVal :: Word64
        !fW = fromIntegral (fromEnum flag) :: Word64
        !aW = fromIntegral age :: Word64
    in mW .|.
       (sW `shiftL` 16) .|.
       (dW `shiftL` 32) .|.
       (fW `shiftL` 40) .|.
       (aW `shiftL` 42)

-- | Probe the TT.
-- Performance: Fold the upper 32 bits into the lower 32 bits before masking
-- to reduce hash collisions when the TT mask discards high-entropy upper bits.
probeTT :: TT -> Word64 -> IO (Maybe (Move, Int, Depth, TTFlag))
probeTT (TT v mask) !key = do
    let !k1 = fromIntegral key :: Int
        !k2 = fromIntegral (key `shiftR` 32) :: Int
        !idx = ((k1 `xor` k2) .&. mask) `shiftL` 1
    !entryKey <- UM.unsafeRead v idx
    if entryKey == key
    then do
        !entryData <- UM.unsafeRead v (idx + 1)
        let !m = coerce (fromIntegral (entryData .&. 0xFFFF) :: Word16) :: Move
            !s = fromIntegral ((entryData `shiftR` 16) .&. 0xFFFF) - 32768
            !d = Depth (fromIntegral ((entryData `shiftR` 32) .&. 0xFF))
            !f = toEnum (fromIntegral ((entryData `shiftR` 40) .&. 0x3))
        return $ Just (m, s, d, f)
    else return Nothing

-- | Store in TT.
-- Replacement strategy: Always replace if age differs.
-- Otherwise, depth-preferred or always replace for exact matches.
-- Performance: Fold the upper 32 bits into the lower 32 bits before masking.
storeTT :: TT -> Int -> Word64 -> Depth -> Int -> TTFlag -> Move -> IO ()
storeTT (TT v mask) !age !key !depth !score !flag !move = do
    let !k1 = fromIntegral key :: Int
        !k2 = fromIntegral (key `shiftR` 32) :: Int
        !idx = ((k1 `xor` k2) .&. mask) `shiftL` 1
    -- Read old entry to decide replacement
    !oldKey <- UM.unsafeRead v idx

    let !currentAgeMasked = age .&. 0xFF
    let !depthVal = unDepth depth

    if oldKey /= key then do
        UM.unsafeWrite v idx key
        UM.unsafeWrite v (idx + 1) (packData move score depthVal flag currentAgeMasked)
    else do
        !oldData <- UM.unsafeRead v (idx + 1)
        let !oldDepth = fromIntegral ((oldData `shiftR` 32) .&. 0xFF) :: Int
            !oldAge = fromIntegral ((oldData `shiftR` 42) .&. 0xFF) :: Int

        when (oldAge /= currentAgeMasked || depthVal >= oldDepth || flag == TTExact) $ do
            UM.unsafeWrite v (idx + 1) (packData move score depthVal flag currentAgeMasked)
