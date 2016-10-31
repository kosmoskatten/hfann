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
    , destroy
    , setActivationFunctionHidden
    , setActivationFunctionOutput
    ) where

import Foreign.C.Types (CInt, CUInt)
import Foreign.Ptr (Ptr)

import qualified Language.C.Inline as C

import AI.Fann.Types (FannRec, fannCtx)

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
