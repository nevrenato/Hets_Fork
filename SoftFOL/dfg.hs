{- |
Module      :  $Id: dfg.hs 13959 2010-08-31 22:15:26Z cprodescu $
Description :  a standalone dfg parser
Copyright   :  (c) C. Maeder and Uni Bremen 2007
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  portable

parse dfg files
-}

module Main where

import System.Environment
import Text.ParserCombinators.Parsec
import SoftFOL.DFGParser
import SoftFOL.Print ()
import Common.DocUtils

main :: IO ()
main = getArgs >>= mapM_ process

process :: String -> IO ()
process f = do
  s <- readFile f
  case parse parseSPASS f s of
    Right b -> writeFile (f ++ ".dfg") $ shows (pretty b) "\n"
    Left err -> print err
