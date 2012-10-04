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
import Data.Maybe
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
        s <- callParser $ parse_basic_spec l
        return $ Spec_Wrapper $ ExtSpec s 

callParser :: Maybe (AParser st a) -> AParser st a
callParser = fromMaybe (fail "no parser for this logic")

