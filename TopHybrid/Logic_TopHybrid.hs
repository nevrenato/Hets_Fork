{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, ExistentialQuantification, DeriveDataTypeable, StandaloneDeriving #-}
{- |
Module      :  $Header$
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  : nevrenato@gmail.com
Stability   : experimental
Portability : portable

Description :
Instance of class Logic for hybridized logics 
with an arbitrary logic under.
-}
module TopHybrid.Logic_TopHybrid where

import qualified Data.Map as M
import Logic.Logic
import TopHybrid.AS_TopHybrid
import TopHybrid.TopHybridSign
import TopHybrid.Parse_AS
import TopHybrid.Print_AS 
import TopHybrid.StatAna
import TopHybrid.Utilities
import CASL.Morphism
import CASL.AS_Basic_CASL
import CASL.SymbolParser
import CASL.Sign

-- Import of logics
import CASL.Logic_CASL
import Propositional.Logic_Propositional
import CoCASL.Logic_CoCASL
-- End of import of logics

data Hybridize = Hybridize deriving Show

instance Language Hybridize where
 description _ = "Hybridization of an arbitrary logic"

instance Category Sgn_Wrap Mor where
 ide = undefined
 inverse = undefined
 composeMorphisms = undefined
 dom = undefined
 cod = undefined
 isInclusion = undefined
 legal_mor = undefined
 
instance Syntax Hybridize Spc_Wrap SYMB_ITEMS SYMB_MAP_ITEMS where
        parse_basic_spec Hybridize = Just $ thBasic getLogic
        parse_symb_items Hybridize = Just undefined 
        parse_symb_map_items Hybridize = Just undefined

instance Sentences Hybridize Frm_Wrap Sgn_Wrap Mor Symbol where
        map_sen Hybridize = undefined
        negation Hybridize = undefined
        sym_of Hybridize = undefined
        mostSymsOf Hybridize = undefined
        symmap_of Hybridize = undefined
        sym_name Hybridize = undefined
        symKind Hybridize = undefined
        symsOfSen Hybridize = undefined
        simplify_sen Hybridize = simSen 
        print_named Hybridize = printNamedFormula 
 
instance StaticAnalysis Hybridize Spc_Wrap Frm_Wrap SYMB_ITEMS SYMB_MAP_ITEMS
          Sgn_Wrap Mor Symbol RawSymbol where 
                basic_analysis Hybridize = Just thAna 
                empty_signature Hybridize = emptyHybridSign
                sen_analysis Hybridize = Just anaForm'

instance Logic Hybridize () Spc_Wrap Frm_Wrap SYMB_ITEMS SYMB_MAP_ITEMS
               Sgn_Wrap Mor Symbol RawSymbol () where
                stability Hybridize = Experimental
                parse_basic_sen Hybridize = Just $ formParser'


----- The logics supported section ----
-- Logics supported
underlogicList :: [(String, AnyLogic)]
underlogicList = [ 
                   (show CASL, Logic CASL), 
                   (show Propositional, Logic Propositional),
                   (show CoCASL, Logic CoCASL),
                   (show Hybridize, Logic Hybridize) 
                 ]

-- Construction of the underlogics map using a list
underlogics :: M.Map String AnyLogic
underlogics =  buildMapFromList underlogicList
 
-- Tries to get a logic, if it fails, then
-- gives an error saying that the logic in question doesn't exist 
getLogic :: String -> AnyLogic
getLogic s = maybeE 3 $ M.lookup s underlogics
