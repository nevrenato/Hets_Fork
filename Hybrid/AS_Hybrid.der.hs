{- |
Module      :  $Header$
Copyright   :  (c) T.Mossakowski, W.Herding, C.Maeder, Uni Bremen 2004-2006
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  till@informatik.uni-bremen.de
Stability   :  provisional
Portability :  portable

Abstract syntax for hybrid logic extension of CASL
Only the added syntax is specified
-}

module Hybrid.AS_Hybrid where

import Common.Id
import Common.AS_Annotation
import CASL.AS_Basic_CASL

-- DrIFT command
{-! global: GetRange !-}

type H_BASIC_SPEC = BASIC_SPEC H_BASIC_ITEM H_SIG_ITEM H_FORMULA

type AnHybFORM = Annoted (FORMULA H_FORMULA)

data H_BASIC_ITEM = Simple_mod_decl [Annoted SIMPLE_ID] [AnHybFORM] Range
                  | Term_mod_decl [Annoted SORT] [AnHybFORM] Range
                  | Simple_nom_decl [Annoted SIMPLE_ID] [AnHybFORM] Range
                    deriving Show

data RIGOR = Rigid | Flexible deriving Show

data H_SIG_ITEM =
          Rigid_op_items RIGOR [Annoted (OP_ITEM H_FORMULA)] Range
                 -- pos: op, semi colons
        | Rigid_pred_items RIGOR [Annoted (PRED_ITEM H_FORMULA)] Range
                 -- pos: pred, semi colons
             deriving Show

data MODALITY = Simple_mod SIMPLE_ID | Term_mod (TERM H_FORMULA)
             deriving (Eq, Ord, Show)

data NOMINAL = Simple_nom SIMPLE_ID 
        deriving (Eq, Ord, Show)

data H_FORMULA =  At NOMINAL (FORMULA H_FORMULA) Range 
                | BoxOrDiamond Bool MODALITY (FORMULA H_FORMULA) Range 
                | Here NOMINAL Range 
                | Bind NOMINAL (FORMULA H_FORMULA) Range 
             deriving (Eq, Ord, Show)
