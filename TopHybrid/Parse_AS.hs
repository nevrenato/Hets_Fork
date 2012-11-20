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
        let l = getLogic $ show logicName
        p <- thSpec l
        return p

thSpec :: AnyLogic -> AParser st Spec_Wrapper
thSpec l@(Logic l')=
        do
        asKey "Basic_Spec"
        asKey "{"
        s <- callParser $ parse_basic_spec l'
        asKey "}"
        i <- many itemParser 
        f <- sepBy (formParser l) anSemiOrComma
        return $ Spec_Wrapper l (Bspec i s) f

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

ids :: AParser st [SIMPLE_ID]
ids = sepBy simpleId anSemiOrComma 
 
formParser :: AnyLogic -> AParser st Form_Wrapper 
formParser (Logic l) = (fParser l) >>= return . Form_Wrapper 
        
fParser :: (Sentences l f sign morphism symbol) => l -> AParser st (TH_FORMULA f)
fParser l  =
        do
        asKey "@"
        n <- simpleId
        f <- fParser l 
        return $ At n f 
        <|>
        do 
        asKey "Here"
        n <- simpleId
        return $ Here n
        <|>
        do
        asKey "{" 
        f <- callParser $ parse_basic_sen l
        asKey "}"
        return $ UnderLogic f 


callParser :: Maybe (AParser st a) -> AParser st a
callParser = fromMaybe (fail "Failed! No parser for this logic")

instance TermParser (TH_FORMULA f) where
