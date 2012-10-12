{- |
Module      :  $Header$
Copyright   :  (c) Till Mossakowski, Wiebke Herding, C. Maeder, Uni Bremen 2004
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  luecke@informatik.uni-bremen.de
Stability   :  provisional
Portability :  portable

Parser for modal logic extension of CASL
-}

module Hybrid.Parse_AS where

import CASL.Formula
import CASL.OpItem

import Common.AS_Annotation
import Common.AnnoState
import Common.Id
import Common.Keywords
import Common.Lexer
import Common.Token
import Hybrid.AS_Hybrid
import Hybrid.Keywords
import Text.ParserCombinators.Parsec

import Data.List


hybrid_reserved_words :: [String]
hybrid_reserved_words = [ 
        diamondS,
        termS,
        rigidS,
        flexibleS,
        modalityS,
        modalitiesS,
        nominalS,
        nominalsS]

hybridFormula :: AParser st H_FORMULA
hybridFormula =
        do
        a <- asKey hereP
        n <- nominal  
        return (Here n $ toRange a [] a) 
        <|>
        do 
        a <- asKey asP
        n <- nominal 
        f <- primFormula hybrid_reserved_words
        return (At n f $ toRange a [] a)
        <|>
        do
        a <- asKey exMark
        n <- nominal
        f <- primFormula hybrid_reserved_words
        return (Bind n f $ toRange a [] a)
        <|>
        do
        o <- oBracketT
        m <- modality [] 
        c <- cBracketT
        f <- primFormula hybrid_reserved_words
        return (BoxOrDiamond True m f $ toRange o [] c)
        <|>
        do
        o <- asKey lessS 
        m <- modality [greaterS] -- do not consume matching ">"!
        c <- asKey greaterS
        f <- primFormula hybrid_reserved_words
        return (BoxOrDiamond False m f $ toRange o [] c)
-- The following is blocked because of this is not being suported in 
-- the translation HybridCasl2Casl
--        <|>
--        do
--        d <- asKey diamondS
--        f <- primFormula hybrid_reserved_words
--        let p = tokPos d
--        return (BoxOrDiamond False (Simple_mod $ Token emptyS p) f p)


nominal :: AParser st NOMINAL
nominal =      
        do 
        n <- simpleId
        return (Simple_nom n)

-- The following is blocked because of this is not being suported in 
-- the translation HybridCasl2Casl
modality :: [String] -> AParser st MODALITY
modality ks =
    do t <- term (ks ++ hybrid_reserved_words)
       return $ Term_mod t
--   <|> return (Simple_mod $ mkSimpleId emptyS)


instance TermParser H_FORMULA where
    termParser = aToTermParser hybridFormula

-- Block the Rigid parts
rigor :: AParser st RIGOR
rigor = (asKey flexibleS >> return Flexible)

rigidSigItems :: AParser st H_SIG_ITEM
rigidSigItems =
    do r <- rigor
       itemList hybrid_reserved_words opS opItem (Rigid_op_items r)
         <|> itemList hybrid_reserved_words predS predItem (Rigid_pred_items r)

instance AParsable H_SIG_ITEM where
  aparser = rigidSigItems

hKey :: AParser st Token
hKey =  asKey modalityS <|> asKey modalitiesS  

hKey' :: AParser st Token
hKey' = asKey nominalS <|> asKey nominalsS 

hBasic :: AParser st H_BASIC_ITEM
hBasic =
--    do (as, fs, ps) <- hItem' simpleId
--       return (Simple_nom_decl as fs ps)
--    <|>
--    do (as, fs, ps) <- hItem simpleId
--       return (Simple_mod_decl as fs ps)
--    <|>
    do (as, fs, ps) <- hItem'' simpleId
       return (Simple_mod_decl as fs ps)
    <|>
    do (as, fs, ps) <- hItem''' simpleId
       return (Simple_nom_decl as fs ps)
--    <|>
--    do t <- asKey termS
--       (as, fs, ps) <- hItem (sortId hybrid_reserved_words)
--       return (Term_mod_decl as fs (tokPos t `appRange` ps))

hItem'' :: AParser st a -> AParser st ([Annoted a], [AnHybFORM], Range)
hItem'' pr = do
        c <- hKey
        (as,cs) <- separatedBy (annoParser pr) anSemiOrComma
        let ps = catRange $ c : cs
        return (as,[],ps)

hItem''' :: AParser st a -> AParser st ([Annoted a], [AnHybFORM], Range)
hItem''' pr = do
         c <- hKey'
         (as,cs) <- separatedBy (annoParser pr) anSemiOrComma
         let ps = catRange $ c : cs
         return (as,[],ps)

-- for now this will be forbidden. There cannot be formulas inside
-- declarations of Nominals/Modalities
--hItem :: AParser st a -> AParser st ([Annoted a], [AnHybFORM], Range)
--hItem pr = do
--       c <- hKey
--       (as, cs) <- separatedBy (annoParser pr) anSemiOrComma
--       let ps = catRange $ c : cs
--       do o <- oBraceT
--          (fs, q) <- auxItemList (delete diamondS hybrid_reserved_words) []
--                      (formula hybrid_reserved_words) (,)
--          p <- cBraceT
--          return (as, fs, ps `appRange` q `appRange` toRange o [] p)
--        <|> return (as, [], ps)
--
--hItem' :: AParser st a -> AParser st ([Annoted a], [AnHybFORM], Range)
--hItem' pr = do
--       c <- hKey'
--       (as, cs) <- separatedBy (annoParser pr) anSemiOrComma
--       let ps = catRange $ c : cs
--       do o <- oBraceT
--          (fs, q) <- auxItemList (delete diamondS hybrid_reserved_words) []
--                      (formula hybrid_reserved_words) (,)
--          p <- cBraceT
--          return (as, fs, ps `appRange` q `appRange` toRange o [] p)
--        <|> return (as, [], ps)

instance AParsable H_BASIC_ITEM where
  aparser = hBasic
