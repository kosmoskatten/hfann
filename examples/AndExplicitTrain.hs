module Main
    ( main
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.Vector.Storable ((!))
import Text.Printf (printf)

import qualified Data.Vector.Storable as Vec

import AI.Fann

type Bundle = (Vec.Vector Float, Vec.Vector Float)

bundleStream :: Int -> [Bundle] -> [Bundle]
bundleStream _ [] = []
bundleStream n inp = take n $ cycle inp

trainOnData :: Fann -> Int -> [Bundle] -> IO ()
trainOnData fann n pattern = do
    putStrLn "Start training phase ..."
    forM_ (bundleStream n pattern) $ \(input, output) -> do
        train fann input output
        --printf "Tick. MSE=%f\n" (mse fann)
        --threadDelay 25000

testANN :: Fann -> Vec.Vector Float -> IO ()
testANN fann input = do
    output <- run fann input
    printf "and (%f, %f) => %f\n" (input ! 0) (input ! 1) (output ! 0)

main :: IO ()
main = do
    fann <- createStandard'3L 2 3 1
    setActivationFunctionHidden fann Sigmoid
    setActivationFunctionOutput fann Sigmoid

    let pattern = [ (Vec.fromList [0, 0], Vec.fromList [0])
                  , (Vec.fromList [0, 1], Vec.fromList [0])
                  , (Vec.fromList [1, 0], Vec.fromList [0])
                  , (Vec.fromList [1, 1], Vec.fromList [1])
                  ]

    trainOnData fann 100000 pattern

    printf "After training: MSE=%f\n" (mse fann)

    mapM_ (testANN fann) [ Vec.fromList [0, 0]
                         , Vec.fromList [1, 0]
                         , Vec.fromList [0, 1]
                         , Vec.fromList [1, 1]
                         ]

    destroy fann
