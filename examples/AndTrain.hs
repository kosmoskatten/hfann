module Main
    ( main
    ) where

import AI.Fann ( ActivationFunction (..)
               , createStandard'3L
               , destroy
               , setActivationFunctionHidden
               , setActivationFunctionOutput
               )

main :: IO ()
main = do
    fann <- createStandard'3L 2 3 1
    setActivationFunctionHidden fann SigmoidSymmetric
    setActivationFunctionOutput fann SigmoidSymmetric
    destroy fann
