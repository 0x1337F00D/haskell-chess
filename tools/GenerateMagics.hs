{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
import Data.Word
import qualified Data.Vector.Unboxed as U
import Chess.Bitboard (initMagics, Magic(..))

main :: IO ()
main = do
    putStrLn "Generating Magic Bitboards..."
    (bishopMagics, rookMagics, magicTable) <- initMagics True

    withFile "src/Chess/Bitboard/MagicTables.hs" WriteMode $ \h -> do
        hPutStrLn h "{-# LANGUAGE OverloadedLists #-}"
        hPutStrLn h "module Chess.Bitboard.MagicTables ("
        hPutStrLn h "    bbBishopMagics,"
        hPutStrLn h "    bbRookMagics,"
        hPutStrLn h "    bbMagicTable"
        hPutStrLn h ") where"
        hPutStrLn h ""
        hPutStrLn h "import Data.Word (Word64)"
        hPutStrLn h "import qualified Data.Vector.Unboxed as U"
        hPutStrLn h "import Chess.Bitboard (Magic(..))"
        hPutStrLn h ""

        let formatMagic (Magic mask m sh offset) =
                "Magic " ++ show mask ++ " " ++ show m ++ " " ++ show sh ++ " " ++ show offset

        hPutStrLn h "bbBishopMagics :: U.Vector Magic"
        hPutStrLn h "bbBishopMagics = U.fromList ["
        hPutStrLn h $ "    " ++ formatMagic (bishopMagics `U.unsafeIndex` 0)
        mapM_ (\i -> hPutStrLn h $ "  , " ++ formatMagic (bishopMagics `U.unsafeIndex` i)) [1..63]
        hPutStrLn h "  ]"
        hPutStrLn h ""

        hPutStrLn h "bbRookMagics :: U.Vector Magic"
        hPutStrLn h "bbRookMagics = U.fromList ["
        hPutStrLn h $ "    " ++ formatMagic (rookMagics `U.unsafeIndex` 0)
        mapM_ (\i -> hPutStrLn h $ "  , " ++ formatMagic (rookMagics `U.unsafeIndex` i)) [1..63]
        hPutStrLn h "  ]"
        hPutStrLn h ""

        hPutStrLn h "bbMagicTable :: U.Vector Word64"
        hPutStrLn h "bbMagicTable = U.fromList ["
        hPutStrLn h $ "    " ++ show (magicTable `U.unsafeIndex` 0)
        mapM_ (\i -> hPutStrLn h $ "  , " ++ show (magicTable `U.unsafeIndex` i)) [1 .. U.length magicTable - 1]
        hPutStrLn h "  ]"

    putStrLn "Successfully wrote src/Chess/Bitboard/MagicTables.hs"
