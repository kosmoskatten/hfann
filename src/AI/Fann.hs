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
    , trainOnFile
    ) where

import Foreign.C.String (withCString)
import Foreign.C.Types (CFloat (..), CInt (..))
import Foreign.Ptr (Ptr)

import qualified Data.Vector.Storable as Vec

import AI.Fann.FannCtx (FannRec)
import AI.Fann.Types (ActivationFunction (..), activationToInt)

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
run :: Fann -> Vec.Vector Float -> IO (Vec.Vector Float)
run fann input =
    Vec.map (\(CFloat f) -> f) <$>
        (Glue.run (fannRec fann) $ Vec.map CFloat input)

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

-- | Train the ANN with a dataset provided in the given file.
trainOnFile :: Fann -> FilePath -> Int -> Int -> Float -> IO ()
trainOnFile fann file epochs epochsPerReport desiredError =
    withCString file $ \file' ->
        Glue.trainOnFile (fannRec fann) file' (fromIntegral epochs)
                         (fromIntegral epochsPerReport) (CFloat desiredError)

fromCFloat :: CFloat -> Float
fromCFloat (CFloat f) = f
