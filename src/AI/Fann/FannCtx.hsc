{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
-- Module:      AI.Fann.FannCtx
-- Copyright:   (C) 2016 Patrik Sandahl
-- Licence:     MIT
-- Maintainer:  Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability:   experimental
-- Portability: Linux
--
-- Types and inline-c context for the glue between Haskell and C.
module AI.Fann.FannCtx
    ( FannRec
    , fannCtx
    ) where

import Data.Monoid ((<>), mempty)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))
import Language.C.Inline (baseCtx, vecCtx)
import Language.C.Inline.Context (Context (..))

import qualified Data.Map as Map
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH

#include "fannwrap.h"

-- | FFI reference to the FANN struct fann.
data FannRec

-- | Storable instance for 'FannRec'.
instance Storable FannRec where
    sizeOf _    = (#size FannRec)
    alignment _ = alignment (undefined :: Ptr ())
    peek        = error "No peek for FannRec"
    poke _ _    = error "No poke for FannRec"

-- | The 'Context' for FANN.
fannCtx :: Context
fannCtx = baseCtx <> vecCtx <> ctx
  where
    ctx = mempty
        { ctxTypesTable = typesTable
        }

typesTable :: Map.Map C.TypeSpecifier TH.TypeQ
typesTable = Map.fromList
    [ (C.TypeName "FannRec", [t| FannRec |])
    ]
