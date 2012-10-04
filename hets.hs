{-# LANGUAGE CPP #-}
{- |
Module      :  $Id: hets.hs 16727 2012-03-15 11:44:27Z maeder $
Copyright   :  (c) Uni Bremen 2003-2005
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable (imports Logic.Logic)

The Main module of the Heterogeneous Tool Set.
   It provides the main function to call (and not much more).

-}

-- for interactice purposes use Test.hs

module Main where

import System.Environment (getArgs)

import Driver.Options
import Driver.AnaLib
import Driver.WriteFn

import Static.DevGraph
import Static.FromXml

#ifdef UNI_PACKAGE
import GUI.ShowGraph
#else
import Control.Monad ( when )
#endif

#ifdef PROGRAMATICA
import Haskell.Haskell2DG
#endif

import Common.LibName
import Interfaces.DataTypes
import CMDL.ProcessScript
import CMDL.DataTypes
import PGIP.XMLparsing
import PGIP.XMLstate (isRemote)
#ifdef SERVER
import PGIP.Server
#endif

import Maude.Maude2DG (anaMaudeFile)
import LF.Twelf2DG (anaTwelfFile)
import OMDoc.Import (anaOMDocFile)
#ifdef HEXPAT
import HolLight.HolLight2DG (anaHolLightFile)
#endif

#ifdef HAXML
import Isabelle.Isa2DG (anaIsaFile)
#endif

main :: IO ()
main =
    getArgs >>= hetcatsOpts >>= \ opts ->
#ifdef SERVER
     if serve opts then hetsServer opts else
#endif
     if isRemote opts || interactive opts
       then cmdlRun opts >>= displayGraph "" opts . getMaybeLib . intState
       else do
              putIfVerbose opts 3 $ "Options: " ++ show opts
              mapM_ (processFile opts) (infiles opts)

processFile :: HetcatsOpts -> FilePath -> IO ()
processFile opts file = do
    putIfVerbose opts 3 ("Processing input: " ++ file)
    res <- case guess file (intype opts) of
#ifdef PROGRAMATICA
      HaskellIn -> anaHaskellFile opts file
#endif
#ifdef HEXPAT
      HolLightIn -> anaHolLightFile opts file
#endif
#ifdef HAXML
      IsaIn -> anaIsaFile opts file
#endif
      PrfIn -> anaLibReadPrfs opts file
      ProofCommand -> do
        st <- cmdlProcessFile opts file
        return . getMaybeLib $ intState st
      MaudeIn -> anaMaudeFile opts file
      TwelfIn -> anaTwelfFile opts file
      OmdocIn -> anaOMDocFile opts file
      DgXml | not (defLogicIsDMU opts) -> readDGXml opts file
      _ -> anaLib opts file
    case res of
      Just (ln, nEnv) ->
        writeSpecFiles opts file nEnv ln $ lookupDGraph ln nEnv
      _ -> return ()
    displayGraph file opts res

displayGraph :: FilePath -> HetcatsOpts -> Maybe (LibName, LibEnv) -> IO ()
#ifdef UNI_PACKAGE
displayGraph file opts res = case guiType opts of
    NoGui -> return ()
    UseGui -> showGraph file opts res
#else
displayGraph _ opts _ = when (guiType opts == UseGui)
  $ fail $ "No graph display interface; \n"
            ++ "UNI_PACKAGE option has been "
            ++ "disabled during compilation of Hets"
#endif
