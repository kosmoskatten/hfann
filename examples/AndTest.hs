module Main
    ( main
    ) where

import Data.Vector.Storable ((!))
import Text.Printf (printf)

import qualified Data.Vector.Storable as Vec

import AI.Fann (Fann, createFromFile, destroy, run)

main :: IO ()
main = do
    fann <- createFromFile "and.net"

    mapM_ (testANN fann) [ Vec.fromList [0, 0]
                         , Vec.fromList [1, 0]
                         , Vec.fromList [0, 1]
                         , Vec.fromList [1, 1]
                         ]

    destroy fann

testANN :: Fann -> Vec.Vector Float -> IO ()
testANN fann input = do
    output <- run fann input
    printf "and (%f, %f) => %f\n" (input ! 0) (input ! 1) (output ! 0)
