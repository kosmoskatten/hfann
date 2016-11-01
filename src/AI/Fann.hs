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
    , run
    , numInput
    , numOutput
    , destroy
    , setActivationFunctionHidden
    , setActivationFunctionOutput
    , trainOnFile
    , save
    , createFromFile
    ) where

import Foreign.C.String (withCString)
import Foreign.C.Types (CFloat (..), CInt (..), CUInt (..))
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
createStandard'3L :: Int
                     -- ^ Number of neurons in the input layer.
                  -> Int
                     -- ^ Number of neurons in the hidden layer.
                  -> Int
                     -- ^ Number of neurons in the output layer.
                  -> IO Fann
createStandard'3L input hidden output =
    Fann <$> Glue.createStandard'3L (toCUInt input)
                                    (toCUInt hidden)
                                    (toCUInt output)

-- | Will run input through the neural network, returning an array
-- of outputs, the number of which being equal to the number of neurons
-- in the output layer.
run :: Fann
       -- ^ The instance to run.
    -> Vec.Vector Float
       -- ^ Input data to the network.
    -> IO (Vec.Vector Float)
run fann input =
    Vec.map (\(CFloat f) -> f) <$>
        (Glue.run (fannRec fann) $ Vec.map CFloat input)

-- | Query the ANN. Get the number of input neurons.
numInput :: Fann
             -- ^ The instance to be queried.
         -> IO Int
numInput fann = do
    CUInt r <- Glue.numInput (fannRec fann)
    return $ fromIntegral r

-- | Query the ANN. Get the number of output neurons.
numOutput :: Fann
             -- ^ The instance to be queried.
          -> IO Int
numOutput fann = do
    CUInt r <- Glue.numOutput (fannRec fann)
    return $ fromIntegral r

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

-- | Train the ANN with a dataset provided in the given file.
trainOnFile :: Fann
               -- ^ The instance to be trained.
            -> FilePath
               -- ^ File with training dataset.
            -> Int
               -- ^ Max number of epochs.
            -> Int
               -- ^ Number of epochs between reports.
            -> Float
               -- ^ The desired error.
            -> IO ()
trainOnFile fann file epochs epochsPerReport desiredError =
    withCString file $ \file' ->
        Glue.trainOnFile (fannRec fann) file' (toCUInt epochs)
                         (toCUInt epochsPerReport) (CFloat desiredError)

-- | Save the entire network to a configuration file.
--
-- The configuration file contains all information about the neural
-- network and enables 'createFromFile' to create an exact copy of
-- the neural network and all of the parameters associated
-- with the neural network.
save :: Fann
        -- ^ The instance to be saved.
     -> FilePath
        -- ^ Filepath to where to save configuration.
     -> IO Bool
save fann file =
    withCString file $ \file' -> do
        CInt r <- Glue.save (fannRec fann) file'
        return (r /= 0)

-- | Constructs a backpropagation neural network from a configuration
-- file, which has been saved by 'save'.
createFromFile :: FilePath
                  -- ^ Path to the network to load.
               -> IO Fann
createFromFile file =
    withCString file $ \file' ->
        Fann <$> Glue.createFromFile file'

toCInt :: Integral a => a -> CInt
toCInt = CInt . fromIntegral

toCUInt :: Integral a => a -> CUInt
toCUInt = CUInt . fromIntegral
