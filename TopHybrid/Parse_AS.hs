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
formParser (Logic l) = (topParser l) >>= return . Form_Wrapper 

-- BinaryOps parsers, the reason to separate them, is so we can get a 
-- precedence order
conjP :: AParser st ((TH_FORMULA f) -> (TH_FORMULA f) -> (TH_FORMULA f))
conjP = asKey "/\\" >> return Conjunction

disjP :: AParser st ((TH_FORMULA f) -> (TH_FORMULA f) -> (TH_FORMULA f))
disjP = asKey "\\/" >> return Disjunction

impAndBiP :: AParser st ((TH_FORMULA f) -> (TH_FORMULA f) -> (TH_FORMULA f))
impAndBiP = (asKey "->" >> return Implication) <|> (asKey "<->" >> return BiImplication)
--
-- The parsing goes by order of precedence
topParser :: (Sentences l f sign morphism symbol) => l -> AParser st (TH_FORMULA f)
topParser l = chainl1 fp1 impAndBiP >>= return
        where   fp1 = (chainl1 fp2 disjP >>= return)
                fp2 = (chainl1 (fParser l) conjP >>= return) 

fParser :: (Sentences l f sign morphism symbol) => l -> AParser st (TH_FORMULA f)
fParser l  =
        do 
        asKey "("
        f <- (topParser l)
        asKey ")"
        return f
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
               
callParser :: Maybe (AParser st a) -> AParser st a
callParser = fromMaybe (fail "Failed! No parser for this logic")
