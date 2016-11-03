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
--
-- The Haskell bindings is implementing a selected subset of the C API.
module AI.Fann
    ( Fann
    , ActivationFunction (..)
    , InputData
    , OutputData
    , createStandard'3L
    , createFromFile
    , save
    , destroy
    , run
    , numInput
    , numOutput
    , learningRate
    , setLearningRate
    , setActivationFunctionHidden
    , setActivationFunctionOutput
    , mse
    , trainOnFile
    , train
    ) where

import Foreign.C.String (withCString)
import Foreign.C.Types (CFloat (..), CInt (..))
import Foreign.Ptr (Ptr)

import qualified Data.Vector.Storable as Vec

import AI.Fann.FannCtx (FannRec)
import AI.Fann.Types ( ActivationFunction (..), InputData
                     , OutputData, activationToInt
                     )

import qualified AI.Fann.Glue as Glue

-- | Handle to a constructed network. Opaque to the user of the library.
data Fann = Fann
    { fannRec :: !(Ptr FannRec)
    }

-- | Create a three layer standard fully connected backpropagation neural
-- network. There will be a bias neuron in each layer (except the output layer),
-- and this bias neuron will be connected to all neurons in the next layer.
-- When running the network, the bias nodes always emits 1.
--
-- Resources allocated using this function is released using 'destroy'.
createStandard'3L :: Int -> Int -> Int -> IO Fann
createStandard'3L input hidden output =
    Fann <$> Glue.createStandard'3L (fromIntegral input)
                                    (fromIntegral hidden)
                                    (fromIntegral output)

-- | Constructs a backpropagation neural network from a configuration
-- file, which has been saved by 'save'.
createFromFile :: FilePath -> IO Fann
createFromFile file =
    withCString file $ \file' ->
        Fann <$> Glue.createFromFile file'

-- | Save the entire network to a configuration file.
--
-- The configuration file contains all information about the neural
-- network and enables 'createFromFile' to create an exact copy of
-- the neural network and all of the parameters associated
-- with the neural network.
save :: Fann -> FilePath -> IO Bool
save fann file =
    withCString file $ \file' -> do
        CInt r <- Glue.save (fannRec fann) file'
        return (r /= 0)

-- | Destroys the entire network, properly freeing all the associated memory.
destroy :: Fann -> IO ()
destroy = Glue.destroy . fannRec

-- | Will run input through the neural network, returning an array
-- of outputs, the number of which being equal to the number of neurons
-- in the output layer.
run :: Fann -> InputData -> IO OutputData
run fann input =
    fromCFloatVec <$> (Glue.run (fannRec fann) $ toCFloatVec input)

-- | Get the number of input neurons.
numInput :: Fann -> Int
numInput = fromIntegral . Glue.numInput . fannRec

-- | Get the number of output neurons.
numOutput :: Fann -> Int
numOutput = fromIntegral . Glue.numOutput . fannRec

-- | Get the learning rate (default 0.7).
learningRate :: Fann -> Float
learningRate = fromCFloat . Glue.learningRate . fannRec

-- | Set the learning rate.
setLearningRate :: Fann -> Float -> IO ()
setLearningRate fann rate =
    Glue.setLearningRate (fannRec fann) (CFloat rate)

-- | Set the activation function for all of the hidden layers.
setActivationFunctionHidden :: Fann -> ActivationFunction -> IO ()
setActivationFunctionHidden fann activation = do
    let actVal = fromIntegral $ activationToInt activation
    Glue.setActivationFunctionHidden (fannRec fann) actVal

-- | Set the activation function for all of the output layer.
setActivationFunctionOutput :: Fann -> ActivationFunction -> IO ()
setActivationFunctionOutput fann activation = do
    let actVal = fromIntegral $ activationToInt activation
    Glue.setActivationFunctionOutput (fannRec fann) actVal

-- | Read the mean squared error from the network.
--
-- Reads the mean square error from the network. This value is calculated during
-- training or testing, and can therefore sometimes be a bit off if the weights
-- have been changed since the last calculation of the value.
mse :: Fann -> Float
mse = fromCFloat . Glue.mse . fannRec

-- | Train the ANN with a dataset provided in the given file.
trainOnFile :: Fann -> FilePath -> Int -> Int -> Float -> IO ()
trainOnFile fann file epochs epochsPerReport desiredError =
    withCString file $ \file' ->
        Glue.trainOnFile (fannRec fann) file' (fromIntegral epochs)
                         (fromIntegral epochsPerReport) (CFloat desiredError)

-- | Train one iteration with a set of inputs, and a set of desired outputs.
-- The size of the input must be exact 'numInput' and the desired output must
-- exact 'numOutput'.
train :: Fann -> InputData -> OutputData -> IO ()
train fann input output =
    Glue.train (fannRec fann) (toCFloatVec input) (toCFloatVec output)

fromCFloat :: CFloat -> Float
fromCFloat (CFloat f) = f
{-# INLINE fromCFloat #-}

toCFloatVec :: Vec.Vector Float -> Vec.Vector CFloat
toCFloatVec = Vec.map CFloat
{-# INLINE toCFloatVec #-}

fromCFloatVec :: Vec.Vector CFloat -> Vec.Vector Float
fromCFloatVec = Vec.map fromCFloat
{-# INLINE fromCFloatVec #-}
