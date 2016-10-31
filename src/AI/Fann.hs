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
    , createStandard'3L
    ) where

import Foreign.C.Types (CUInt (..))

import qualified AI.Fann.Glue as Glue

-- | Handle to a constructed network. Opaque to the user of the library.
data Fann = Fann
    { outputs :: !Int
    }

-- | Create a three layer standard fully connected backpropagation neural
-- network. There will be a bias neuron in each layer (except the output layer),
-- and this bias neuron will be connected to all neurons in the next layer.
-- When running the network, the bias nodes always emits 1.
createStandard'3L :: Int
                     -- ^ Number of neurons in the input layer.
                  -> Int
                     -- ^ Number of neurons in the hidden layer.
                  -> Int
                     -- ^ Number of neurons in the output layer.
                  -> IO Fann
createStandard'3L input hidden output = do
    let input'  = CUInt $ fromIntegral input
        hidden' = CUInt $ fromIntegral hidden
        output' = CUInt $ fromIntegral output
    Glue.createStandard'3L input' hidden' output'
    return Fann { outputs = output }
