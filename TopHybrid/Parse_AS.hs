{-# LANGUAGE RankNTypes #-}
{- |
Module      :  $Header$
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  nevrenato@gmail.com 
Stability   :  provisional
Portability :  portable


Description  :
Parser for hybrid logic with an arbitrary logic below.
-}

module TopHybrid.Parse_AS where

import Common.AnnoState
import Common.Token
import Common.Id
import Data.Maybe
import Text.ParserCombinators.Parsec
import Logic.Logic

import TopHybrid.AS_TopHybrid
import TopHybrid.UnderLogicList

thBasic :: AParser st Spec_Wrapper
thBasic =
        do
        asKey "underlogic"
        logicName <- simpleId
        p <- specParser $ getLogic $ show logicName
        return p 

specParser :: AnyLogic -> AParser st Spec_Wrapper
specParser l'@(Logic l) = 
        do
        asKey "Basic_Spec"
        asKey "{"
        s <- callParser $ parse_basic_spec l
        asKey "}"
        i <- many itemParser 
        f <- sepBy (formParser l') anSemiOrComma
        return $ Spec_Wrapper (Bspec i s) f

itemParser :: AParser st TH_BASIC_ITEM
itemParser = 
        do 
        asKey "modality"
        ms <- ids
        return $ Simple_mod_decl ms nullRange
        <|>
        do 
        asKey "nominal"
        ns <- ids
        return $ Simple_nom_decl ns nullRange

ids :: AParser st [SIMPLE_ID]
ids = sepBy simpleId anSemiOrComma 
 
formParser :: AnyLogic -> AParser st Form_Wrapper 
formParser l'@(Logic l) = 
        do
        asKey "@"
        n <- simpleId
        (Form_Wrapper f) <- formParser l' 
        return $ Form_Wrapper $ At n f nullRange
        <|>
        do 
        asKey "Here"
        n <- simpleId
        return $ Form_Wrapper $ Here n () nullRange 
        <|>
        do 
        x <- callParser $ parse_basic_sen l
        return $ Form_Wrapper $ UnderLogic x 


callParser :: Maybe (AParser st a) -> AParser st a
callParser = fromMaybe (fail "Failed! No parser for this logic")

instance TermParser (TH_FORMULA f) where
