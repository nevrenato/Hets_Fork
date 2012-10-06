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
specParser (Logic l) = 
        do
        i <- many itemParser
        s <- callParser $ parse_basic_spec l
        return $ Spec_Wrapper $ Bspec i  s 

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

formParser :: (TermParser f) => AParser st (TH_FORMULA f) 
formParser = 
        do
        asKey "@"
        n <- simpleId
        f <- termParser False 
        return $ At n f nullRange 
        <|>
        do 
        f <- termParser False
        return $ UnderLogic f 


callParser :: Maybe (AParser st a) -> AParser st a
callParser = fromMaybe (fail "no parser for this logic")

instance TermParser f => TermParser (TH_FORMULA f) where
