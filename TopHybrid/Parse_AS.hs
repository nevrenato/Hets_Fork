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
formParser (Logic l) = (fParser' l) >>= return . Form_Wrapper 

op :: AParser st ((TH_FORMULA f) -> (TH_FORMULA f) -> (TH_FORMULA f))
op = 
        (asKey "/\\" >> return Conjunction )
        <|>
        (asKey "\\/" >> return Disjunction)
        <|>
        (asKey "->" >> return Implication)
        <|>
        (asKey "<->" >> return BiImplication)
     
fParser' :: (Sentences l f sign morphism symbol) => l -> AParser st (TH_FORMULA f)
fParser' l = chainl1 (fParser l) op >>= return 

fParser :: (Sentences l f sign morphism symbol) => l -> AParser st (TH_FORMULA f)
fParser l  =
        do 
        asKey "("
        f <- fParser' l
        asKey ")"
        return f
        <|>
        do
        asKey "not"
        f <- fParser' l
        return $ Neg f
        <|> 
        do
        asKey "@"
        n <- simpleId
        f <- (fParser l <|> fParser' l)
        return $ At n f 
        <|>
        do 
        asKey "["
        m <- simpleId
        asKey "]"
        f <- fParser' l
        return $ Box m f
        <|>
        do 
        asKey "<"
        m <- simpleId
        asKey ">"
        f <- fParser' l
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
               
callParser :: Maybe (AParser st a) -> AParser st a
callParser = fromMaybe (fail "Failed! No parser for this logic")
