module Main
    ( main
    ) where

import Control.Monad (void)
import AI.Fann ( ActivationFunction (..)
               , createStandard'3L
               , destroy
               , setActivationFunctionHidden
               , setActivationFunctionOutput
               , trainOnFile
               , save
               )

main :: IO ()
main = do
    fann <- createStandard'3L 2 3 1
    setActivationFunctionHidden fann SigmoidSymmetric
    setActivationFunctionOutput fann SigmoidSymmetric

    trainOnFile fann "and.data" 5000000 1000 0.001
    void $ save fann "and.net"

    destroy fann
