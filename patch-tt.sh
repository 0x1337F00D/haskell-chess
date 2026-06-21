#!/bin/bash
cat << 'PATCH' > test-search.patch
--- src/Chess/Engine/TT.hs
+++ src/Chess/Engine/TT.hs
@@ -62,6 +62,17 @@
         return $ Just (m, s, d, f)
     else return Nothing

+{-# INLINE probeTTFast #-}
+probeTTFast :: TT -> Word64 -> IO (Word64, Word64)
+probeTTFast (TT v mask) !key = do
+    let !idx = ttIndex mask key
+    !entryKey <- UM.unsafeRead v idx
+    !entryData <- UM.unsafeRead v (idx + 1)
+    return (entryKey, entryData)
+
+{-# INLINE unpackDataFast #-}
+unpackDataFast :: Word64 -> (Move, Int, Depth, TTFlag)
+unpackDataFast !w = let (!m, !s, !d, !f, _) = unpackData w in (m, s, d, f)
+
 -- | Store in TT.
 -- Replacement strategy: Always replace if age differs.
PATCH
patch -p0 < test-search.patch
