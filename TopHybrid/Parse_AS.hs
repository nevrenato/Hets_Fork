{-# LANGUAGE RankNTypes #-}
{- |
Module      :  $Header$
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  nevrenato@gmail.com 
Stability   :  provisional
Portability :  portable


Description  :
Parser for an hybridized arbitrary logic 
-}

module TopHybrid.Parse_AS where

import Common.AnnoState
import Common.AS_Annotation
import Common.Token
import Data.Maybe
import Text.ParserCombinators.Parsec
import Logic.Logic
import TopHybrid.AS_TopHybrid
import TopHybrid.UnderLogicList

-- the top parser; parses an entire specification
thBasic :: AParser st Spc_Wrap
thBasic =
        do 
        asKey "baselogic"
        logicName <- simpleId
        thSpec $ getLogic $ show logicName

-- Parses the specification after knowing 
--the underlying logic
thSpec :: AnyLogic -> AParser st Spc_Wrap
thSpec (Logic l) =
        do
        asKey "Basic_Spec"
        asKey "{"
        s <- callParser $ parse_basic_spec l
        asKey "}"
        i <- many itemParser
        fs <- sepBy (annoFormParser l) anSemiOrComma
        return $ Spc_Wrap l (Bspec i s) fs

-- Calls the underlying logic parser, only if exists. Otherwise
-- will throw out an error
callParser :: Maybe (AParser st a) -> AParser st a
callParser = fromMaybe (fail "Failed! No parser for this logic")

-- Parses the declaration of nominals and modalities
itemParser :: AParser st TH_BASIC_ITEM
itemParser = 
        do 
        asKey "modality"
        ms <- ids
        return $ Simple_mod_decl ms 
        <|>
        do 
        asKey "nominal"
        ns <- ids
        return $ Simple_nom_decl ns 
        where ids = sepBy simpleId anSemiOrComma


-- Formula parser with annotations
annoFormParser :: (Logic l sub bs f s sm si mo sy rw pf) => 
                        l -> AParser st (Annoted Frm_Wrap)
annoFormParser l = allAnnoParser $ formParser l 

-- Just parses the formula, and wraps it in Frm_Wrap
formParser :: (Logic l sub bs f s sm si mo sy rw pf) => 
                        l -> AParser st Frm_Wrap
formParser l = topParser l >>= return . (Frm_Wrap l) 

-- Parser of sentences
-- The precendence order is left associative and when the priority
-- is defined is as follows : () > (not,@,[],<>) > /\ > \/ > (->,<->)
topParser :: (Sentences l f sign morphism symbol) => l -> AParser st (TH_FORMULA f)
topParser l = chainl1 fp1 impAndBiP >>= return
        where   fp1 = (chainl1 fp2 disjP >>= return)
                fp2 = (chainl1 (fParser l) conjP >>= return) 

-- BinaryOps parsers, the reason to separate them, is that so we can get a 
-- precedence order
conjP :: AParser st ((TH_FORMULA f) -> (TH_FORMULA f) -> (TH_FORMULA f))
conjP = asKey "/\\" >> return Conjunction

disjP :: AParser st ((TH_FORMULA f) -> (TH_FORMULA f) -> (TH_FORMULA f))
disjP = asKey "\\/" >> return Disjunction

impAndBiP :: AParser st ((TH_FORMULA f) -> (TH_FORMULA f) -> (TH_FORMULA f))
impAndBiP = (asKey "->" >> return Implication) <|> (asKey "<->" >> return BiImplication)
--------------

-- Parser of sentences without the binary operators
fParser :: (Sentences l f sign morphism symbol) => l -> AParser st (TH_FORMULA f)
fParser l  =
        do 
        asKey "("
        f <- (topParser l)
        asKey ")"
        return $ Par f
        <|>
        do
        asKey "not"
        f <- (fParser l <|> topParser l)
        return $ Neg f
        <|> 
        do
        asKey "@"
        n <- simpleId
        f <- (fParser l <|> topParser l)
        return $ At n f 
        <|>
        do 
        asKey "["
        m <- simpleId
        asKey "]"
        f <- (fParser l <|> topParser l)
        return $ Box m f
        <|>
        do 
        asKey "<"
        m <- simpleId
        asKey ">"
        f <- ( fParser l <|> topParser l)
        return $ Dia m f
        <|>
        do 
        n <- simpleId
        return $ Here n
        <|>
        do
        asKey "{"
        f <- callParser $ parse_basic_sen l
        asKey "}"
        return $ UnderLogic f 
