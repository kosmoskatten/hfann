-- |
-- Module:      AI.Fann.Types
-- Copyright:   (C) 2016 Patrik Sandahl
-- Licence:     MIT
-- Maintainer:  Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability:   experimental
-- Portability: Linux

module AI.Fann.Types
    ( ActivationFunction (..)
    , InputData
    , OutputData
    , activationToInt
    ) where

import qualified Data.Vector.Storable as Vec

type InputData  = Vec.Vector Float
type OutputData = Vec.Vector Float

-- | The activation functions used for the neurons during training.
data ActivationFunction
    = Linear
    | Threshold
    | ThresholdSymmetric
    | Sigmoid
    | SigmoidStepwise
    | SigmoidSymmetric
    | SigmoidSymmetricStepwise
    | Gaussian
    | GaussianSymmetric
    | GaussianStepwise
    | Elliot
    | ElliotSymmetric
    | LinearPiece
    | LinearPieceSymmetric
    | SinSymmetric
    | CosSymmetric
    | Sin
    | Cos
    deriving (Eq, Show)

-- | Translate an 'ActivationFunction' to Int.
activationToInt :: ActivationFunction -> Int
activationToInt Linear                   = 0
activationToInt Threshold                = 1
activationToInt ThresholdSymmetric       = 2
activationToInt Sigmoid                  = 3
activationToInt SigmoidStepwise          = 4
activationToInt SigmoidSymmetric         = 5
activationToInt SigmoidSymmetricStepwise = 6
activationToInt Gaussian                 = 7
activationToInt GaussianSymmetric        = 8
activationToInt GaussianStepwise         = 9
activationToInt Elliot                   = 10
activationToInt ElliotSymmetric          = 11
activationToInt LinearPiece              = 12
activationToInt LinearPieceSymmetric     = 13
activationToInt SinSymmetric             = 14
activationToInt CosSymmetric             = 15
activationToInt Sin                      = 16
activationToInt Cos                      = 17
