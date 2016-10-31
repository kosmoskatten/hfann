module Main
    ( main
    ) where

import AI.Fann (Fann, createStandard'3L)

main :: IO ()
main = do
    _ <- createStandard'3L 2 3 1
    return ()
