{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

-- |
-- Module:      AI.Fann.Glue
-- Copyright:   (C) 2016 Patrik Sandahl
-- Licence:     MIT
-- Maintainer:  Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability:   experimental
-- Portability: Linux
--
-- Glue code between Haskell and C for integrating FANN.
module AI.Fann.Glue
    ( createStandard'3L
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

import Foreign.C.String (CString)
import Foreign.C.Types (CFloat, CInt, CUInt)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (Ptr)

import qualified Data.Vector.Storable as Vec
import qualified Language.C.Inline as C

import AI.Fann.FannCtx (FannRec, fannCtx)

C.context fannCtx

C.include "fannwrap.h"

-- | Create a three level ANN.
createStandard'3L :: CUInt -> CUInt -> CUInt -> IO (Ptr FannRec)
createStandard'3L input hidden output =
    [C.block| FannRec *{
        return fann_create_standard(3, $(unsigned int input),
                                       $(unsigned int hidden),
                                       $(unsigned int output));
    } |]

-- | Run the network with input data.
run :: Ptr FannRec -> Vec.Vector CFloat -> IO (Vec.Vector CFloat)
run ptr input =
    Vec.unsafeWith input $ \inputPtr -> do
        outputPtr <- newForeignPtr_ =<<
            [C.block| float *{
                return fann_run($(FannRec *ptr), $(float* inputPtr));
            } |]
        numOutput' <- fromIntegral <$> numOutput ptr
        return $ Vec.unsafeFromForeignPtr0 outputPtr numOutput'

-- | Get the number of input neurons.
numInput :: Ptr FannRec -> IO CUInt
numInput ptr =
    [C.block| unsigned int {
        return fann_get_num_input($(FannRec *ptr));
    } |]

-- | Get the number of output neurons.
numOutput :: Ptr FannRec -> IO CUInt
numOutput ptr =
    [C.block| unsigned int {
        return fann_get_num_output($(FannRec *ptr));
    } |]

-- | Destroy an ANN.
destroy :: Ptr FannRec -> IO ()
destroy ptr =
    [C.block| void {
        fann_destroy($(FannRec *ptr));
    } |]

-- | Set the activation function for all the hidden layers.
setActivationFunctionHidden :: Ptr FannRec -> CInt -> IO ()
setActivationFunctionHidden ptr val =
    [C.block| void {
        fann_set_activation_function_hidden($(FannRec *ptr),
                                            $(int val));
    } |]

-- | Set the activation function for all the output layer.
setActivationFunctionOutput :: Ptr FannRec -> CInt -> IO ()
setActivationFunctionOutput ptr val =
    [C.block| void {
        fann_set_activation_function_output($(FannRec *ptr),
                                            $(int val));
    } |]

-- | Train using a dataset from the given file.
trainOnFile :: Ptr FannRec -> CString -> CUInt -> CUInt -> CFloat -> IO ()
trainOnFile ptr file epochs epochsPerReport desiredError =
    [C.block| void {
        fann_train_on_file($(FannRec *ptr),
                           $(const char *file),
                           $(unsigned int epochs),
                           $(unsigned int epochsPerReport),
                           $(float desiredError));
    } |]

-- | Save a complete network to file.
save :: Ptr FannRec -> CString -> IO CInt
save ptr file =
    [C.block| int {
        return fann_save($(FannRec *ptr), $(const char *file));
    } |]

-- | Create a network from file.
createFromFile :: CString -> IO (Ptr FannRec)
createFromFile file =
    [C.block| FannRec *{
        return fann_create_from_file($(const char* file));
    } |]
