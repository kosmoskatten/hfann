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
    ) where

import Foreign.C.Types
import qualified Language.C.Inline as C

C.include "<stdio.h>"
C.include "fannwrap.h"

createStandard'3L :: CUInt -> CUInt -> CUInt -> IO ()
createStandard'3L input hidden output =
    [C.block| void {
        printf("Got %u %u %u", $(unsigned int input),
                               $(unsigned int hidden),
                               $(unsigned int output));
    } |]
