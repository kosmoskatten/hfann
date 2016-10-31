-- |
-- Module:      AI.Fann
-- Copyright:   (C) 2016 Patrik Sandahl
-- Licence:     MIT
-- Maintainer:  Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability:   experimental
-- Portability: Linux
--
-- Haskell bindings to the Fast Artificial Neural Network Library, FANN.
-- See <http://leenissen.dk/fann/wp/> for more information.
module AI.Fann
    ( Fann
    , ActivationFunction (..)
    , createStandard'3L
    , destroy
    , setActivationFunctionHidden
    , setActivationFunctionOutput
    ) where

import Foreign.C.Types (CInt (..), CUInt (..))
import Foreign.Ptr (Ptr)

import AI.Fann.Types (FannRec)
import qualified AI.Fann.Glue as Glue

-- | Handle to a constructed network. Opaque to the user of the library.
data Fann = Fann
    { outputs :: !Int
    , fannRec :: !(Ptr FannRec)
    }

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

-- | Create a three layer standard fully connected backpropagation neural
-- network. There will be a bias neuron in each layer (except the output layer),
-- and this bias neuron will be connected to all neurons in the next layer.
-- When running the network, the bias nodes always emits 1.
--
-- Resources allocated using this function is released using 'destroy'.
createStandard'3L :: Int
                     -- ^ Number of neurons in the input layer.
                  -> Int
                     -- ^ Number of neurons in the hidden layer.
                  -> Int
                     -- ^ Number of neurons in the output layer.
                  -> IO Fann
createStandard'3L input hidden output = do
    fannRec' <- Glue.createStandard'3L (toCUInt input)
                                       (toCUInt hidden)
                                       (toCUInt output)
    return Fann { outputs = output
                , fannRec = fannRec'
                }

-- | Destroys the entire network, properly freeing all the associated memory.
destroy :: Fann
           -- ^ The instance to be destroyed.
        -> IO ()
destroy = Glue.destroy . fannRec

-- | Set the activation function for all of the hidden layers.
setActivationFunctionHidden :: Fann
                               -- ^ The instance to be updated.
                            -> ActivationFunction
                               -- ^ The given 'ActivationFunction'.
                            -> IO ()
setActivationFunctionHidden fann activation = do
    let actVal = toCInt $ activationToInt activation
    Glue.setActivationFunctionHidden (fannRec fann) actVal

-- | Set the activation function for all of the output layer.
setActivationFunctionOutput :: Fann
                               -- ^ The instance to be updated.
                            -> ActivationFunction
                               -- ^ The given 'ActivationFunction'.
                            -> IO ()
setActivationFunctionOutput fann activation = do
    let actVal = toCInt $ activationToInt activation
    Glue.setActivationFunctionOutput (fannRec fann) actVal

toCInt :: Integral a => a -> CInt
toCInt = CInt . fromIntegral

toCUInt :: Integral a => a -> CUInt
toCUInt = CUInt . fromIntegral

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
