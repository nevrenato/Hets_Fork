{- |
Module      :  $Id: fromKif.hs 14816 2011-03-28 13:28:38Z maeder $
Description :  converting Kif to CASL
Copyright   :  (c) C.Maeder, Uni Bremen 2006
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  experimental
Portability :  portable

convert .kif to .casl
-}
module Main where

import CASL.Kif
import CASL.Kif2CASL
import CASL.ToDoc ()

import Common.Utils
import Common.DocUtils

import Text.ParserCombinators.Parsec
import System.Environment

main :: IO ()
main = getArgs >>= mapM_ process

process :: String -> IO ()
process s = do
  e <- parseFromFile kifProg s
  case e of
    Left err -> print err
    Right l -> do
        let f = fst (stripSuffix [".kif"] s) ++ ".casl"
        writeFile f $ showDoc (kif2CASL l) "\n"
