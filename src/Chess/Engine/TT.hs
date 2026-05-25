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
{-# INLINE packData #-}
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

{-# INLINE unpackData #-}
unpackData :: Word64 -> (Move, Int, Depth, TTFlag, Int)
unpackData w =
    let m = coerce (fromIntegral (w .&. 0xFFFF) :: Word16) :: Move
        s = fromIntegral ((w `shiftR` 16) .&. 0xFFFF) - 32768
        d = Depth (fromIntegral ((w `shiftR` 32) .&. 0xFF))
        f = toEnum (fromIntegral ((w `shiftR` 40) .&. 0x3))
        a = fromIntegral ((w `shiftR` 42) .&. 0xFF)
    in (m, s, d, f, a)

-- | Probe the TT.
-- Performance: Fold the upper 32 bits into the lower 32 bits before masking
-- to reduce hash collisions when the TT mask discards high-entropy upper bits.
{-# INLINE probeTTFast #-}
probeTTFast :: TT -> Word64 -> IO Word64
probeTTFast (TT v mask) key = do
    let hashFold = fromIntegral (key `xor` (key `shiftR` 32)) :: Int
        idx = (hashFold .&. mask) * 2
    entryKey <- UM.unsafeRead v idx
    if entryKey == key
    then UM.unsafeRead v (idx + 1)
    else return maxBound

{-# INLINE probeTT #-}
probeTT :: TT -> Word64 -> IO (Maybe (Move, Int, Depth, TTFlag))
probeTT tt key = do
    res <- probeTTFast tt key
    if res == maxBound
    then return Nothing
    else return $ Just (let (m, s, d, f, _) = unpackData res in (m, s, d, f))

{-# INLINE storeTT #-}
storeTT :: TT -> Int -> Word64 -> Depth -> Int -> TTFlag -> Move -> IO ()
storeTT (TT v mask) age key depth score flag move = do
    let hashFold = fromIntegral (key `xor` (key `shiftR` 32)) :: Int
        idx = (hashFold .&. mask) * 2
    oldKey <- UM.unsafeRead v idx

    replace <- if oldKey /= key
               then return True
               else do
                   oldData <- UM.unsafeRead v (idx + 1)
                   let oldDepth = Depth (fromIntegral ((oldData `shiftR` 32) .&. 0xFF))
                       oldAge = fromIntegral ((oldData `shiftR` 42) .&. 0xFF)
                       currentAgeMasked = age .&. 0xFF
                   return $ oldAge /= currentAgeMasked || depth >= oldDepth || flag == TTExact

    when replace $ do
        let currentAgeMasked = age .&. 0xFF
        UM.unsafeWrite v idx key
        UM.unsafeWrite v (idx + 1) (packData move score depth flag currentAgeMasked)
