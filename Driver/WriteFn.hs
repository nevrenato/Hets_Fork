{-# LANGUAGE CPP #-}
{- |
Module      :  $Header$
Description :  Writing various formats, according to Hets options
Copyright   :  (c) Klaus Luettich, C.Maeder, Uni Bremen 2002-2006
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable(DevGraph)

Writing various formats, according to Hets options
-}

module Driver.WriteFn (writeSpecFiles, writeVerbFile) where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.XML.Light
import Data.List (partition, (\\))

import Common.AS_Annotation
import Common.Id
import Common.IRI (IRI, simpleIdToIRI, iriToStringShortUnsecure)
import Common.DocUtils
import Common.ExtSign
import Common.LibName
import Common.Result
import Common.Parsec (forget)
import Common.GlobalAnnotations (GlobalAnnos)
import qualified Data.Map as Map
import Common.SExpr
import Common.IO

import Logic.Coerce
import Logic.Comorphism (targetLogic)
import Logic.Logic
import Logic.Grothendieck
import Comorphisms.LogicGraph
import qualified Static.ToXml as ToXml

import CASL.Logic_CASL

import CASL.CompositionTable.Pretty
import CASL.CompositionTable.ToXml
import CASL.CompositionTable.ComputeTable
import CASL.CompositionTable.ModelChecker
import CASL.CompositionTable.ParseSparQ

#ifdef PROGRAMATICA
import Haskell.CreateModules
#endif
import Isabelle.CreateTheories
import Isabelle.IsaParse
import Isabelle.IsaPrint (printIsaTheory)
import SoftFOL.CreateDFGDoc
import SoftFOL.DFGParser
import SoftFOL.ParseTPTP

import FreeCAD.XMLPrinter (exportXMLFC)
import FreeCAD.Logic_FreeCAD


import VSE.Logic_VSE
import VSE.ToSExpr

#ifndef NOOWLLOGIC
import OWL2.Logic_OWL2
import qualified OWL2.ManchesterPrint as OWL2 (printOWLBasicTheory)
import qualified OWL2.ManchesterParser as OWL2 (basicSpec)
#endif

#ifdef RDFLOGIC
import RDF.Logic_RDF
import qualified RDF.Print as RDF (printRDFBasicTheory)
#endif

import CommonLogic.Logic_CommonLogic
import qualified CommonLogic.AS_CommonLogic as CL_AS (exportCLIF)
import qualified CommonLogic.Parse_CLIF as CL_Parse (cltext)

import Logic.Prover
import Static.GTheory
import Static.DevGraph
import Static.CheckGlobalContext
import Static.DotGraph
import qualified Static.PrintDevGraph as DG
import Proofs.StatusUtils
import Static.ComputeTheory

import Driver.Options
import Driver.ReadFn (libNameToFile)
import Driver.WriteLibDefn

import OMDoc.XmlInterface (xmlOut)
import OMDoc.Export (exportLibEnv)

writeVerbFile :: HetcatsOpts -> FilePath -> String -> IO ()
writeVerbFile opts f str = do
    putIfVerbose opts 2 $ "Writing file: " ++ f
    writeEncFile (ioEncoding opts) f str

-- | compute for each LibName in the List a path relative to the given FilePath
writeVerbFiles :: HetcatsOpts -- ^ Hets options
               -> String -- ^ A suffix to be combined with the libname
               -> [(LibName, String)] -- ^ An output list
               -> IO ()
writeVerbFiles opts suffix = mapM_ f
        where f (ln, s) = writeVerbFile opts (libNameToFile ln ++ suffix) s

writeLibEnv :: HetcatsOpts -> FilePath -> LibEnv -> LibName -> OutType
            -> IO ()
writeLibEnv opts filePrefix lenv ln ot =
    let f = filePrefix ++ "." ++ show ot
        dg = lookupDGraph ln lenv in case ot of
      Prf -> toShATermString (ln, lookupHistory ln lenv)
             >>= writeVerbFile opts f
      XmlOut -> writeVerbFile opts f $ ppTopElement
          $ ToXml.dGraph lenv ln dg
      OmdocOut -> do
          let Result ds mOmd = exportLibEnv (recurse opts) (outdir opts) ln lenv
          showDiags opts ds
          case mOmd of
               Just omd -> writeVerbFiles opts ".omdoc"
                           $ map (\ (libn, od) -> (libn, xmlOut od)) omd
               Nothing -> putIfVerbose opts 0 "could not translate to OMDoc"
      GraphOut (Dot showInternalNodeLabels) -> writeVerbFile opts f
        $ dotGraph f showInternalNodeLabels "" dg
      _ -> return ()

writeSoftFOL :: HetcatsOpts -> FilePath -> G_theory -> IRI
             -> SPFType -> Int -> String -> IO ()
writeSoftFOL opts f gTh i c n msg = do
      let cc = case c of
                 ConsistencyCheck -> True
                 ProveTheory -> False
      mDoc <- printTheoryAsSoftFOL i n cc
              $ (if cc then theoremsToAxioms else id) gTh
      maybe (putIfVerbose opts 0 $
             "could not translate to " ++ msg ++ " file: " ++ f)
          ( \ d -> do
              let str = shows d "\n"
              case parse (if n == 0 then forget parseSPASS else forget tptp)
                   f str of
                Left err -> putIfVerbose opts 0 $ show err
                _ -> putIfVerbose opts 3 $ "reparsed: " ++ f
              writeVerbFile opts f str) mDoc

writeFreeCADFile :: HetcatsOpts -> FilePath -> G_theory -> IO ()
writeFreeCADFile opts filePrefix (G_theory lid (ExtSign sign _) _ _ _) = do
  fcSign <- coercePlainSign lid FreeCAD
            "Expecting a FreeCAD signature for writing FreeCAD xml" sign
  writeVerbFile opts (filePrefix ++ ".xml") $ exportXMLFC fcSign

writeIsaFile :: HetcatsOpts -> FilePath -> G_theory -> LibName -> IRI
             -> IO ()
writeIsaFile opts filePrefix raw_gTh ln i = do
  let Result ds mTh = createIsaTheory raw_gTh
      addThn = (++ '_' : iriToStringShortUnsecure i)
      fp = addThn filePrefix
  showDiags opts ds
  case mTh of
    Nothing ->
      putIfVerbose opts 0 $ "could not translate to Isabelle theory: " ++ fp
    Just (sign, sens) -> do
      let tn = addThn . reverse . takeWhile (/= '/') . reverse $ case
               show $ getLibId ln of
                   [] -> filePrefix
                   lstr -> lstr
          sf = shows (printIsaTheory tn sign sens) "\n"
          f = fp ++ ".thy"
      case parse parseTheory f sf of
        Left err -> putIfVerbose opts 0 $ show err
        _ -> putIfVerbose opts 3 $ "reparsed: " ++ f
      writeVerbFile opts f sf
      when (hasPrfOut opts && verbose opts >= 3) $ let
        (axs, rest) = partition ( \ s -> isAxiom s || isDef s) sens
         in mapM_ ( \ s -> let
           tnf = tn ++ "_" ++ senAttr s
           tf = fp ++ "_" ++ senAttr s ++ ".thy"
           in writeVerbFile opts tf $ shows
                   (printIsaTheory tnf sign $ s : axs) "\n") rest

writeTheory :: [String] -> String -> HetcatsOpts -> FilePath -> GlobalAnnos
  -> G_theory -> LibName -> IRI -> OutType -> IO ()
writeTheory ins nam opts filePrefix ga
  raw_gTh@(G_theory lid (ExtSign sign0 _) _ sens0 _) ln i ot =
    let fp = filePrefix ++ "_" ++ iriToStringShortUnsecure i
        f = fp ++ "." ++ show ot
        th = (sign0, toNamedList sens0)
        lang = language_name lid
    in case ot of
    FreeCADOut -> writeFreeCADFile opts filePrefix raw_gTh
    ThyFile -> writeIsaFile opts filePrefix raw_gTh ln i
    DfgFile c -> writeSoftFOL opts f raw_gTh i c 0 "DFG"
    TPTPFile c -> writeSoftFOL opts f raw_gTh i c 1 "TPTP"
    TheoryFile d -> do
      if null $ show d then
        writeVerbFile opts f $ shows (DG.printTh ga i raw_gTh) "\n"
        else putIfVerbose opts 0 "printing theory delta is not implemented"
      when (lang == language_name VSE) $ do
        (sign, sens) <- coerceBasicTheory lid VSE "" th
        let (sign', sens') = addUniformRestr sign sens
            lse = map (namedSenToSExpr sign') sens'
        unless (null lse) $ writeVerbFile opts (fp ++ ".sexpr")
            $ shows (prettySExpr $ SList lse) "\n"
    SigFile d -> do
      if null $ show d then
        writeVerbFile opts f $ shows (pretty $ signOf raw_gTh) "\n"
        else putIfVerbose opts 0 "printing signature delta is not implemented"
      when (lang == language_name VSE) $ do
        (sign, sens) <- coerceBasicTheory lid VSE "" th
        let (sign', _sens') = addUniformRestr sign sens
        writeVerbFile opts (f ++ ".sexpr")
          $ shows (prettySExpr $ vseSignToSExpr sign') "\n"
    SymXml -> writeVerbFile opts f $ ppTopElement
           $ ToXml.showSymbolsTh ins nam ga raw_gTh
#ifdef PROGRAMATICA
    HaskellOut -> case printModule raw_gTh of
        Nothing ->
            putIfVerbose opts 0 $ "could not translate to Haskell file: " ++ f
        Just d -> writeVerbFile opts f $ shows d "\n"
#endif
    ComptableXml -> if lang == language_name CASL then do
          th2 <- coerceBasicTheory lid CASL "" th
          let Result ds res = computeCompTable i th2
          showDiags opts ds
          case res of
            Just td -> writeVerbFile opts f $ tableXmlStr td
            Nothing -> return ()
        else putIfVerbose opts 0 $ "expected CASL theory for: " ++ f
#ifndef NOOWLLOGIC
    OWLOut
      | lang == language_name OWL2 -> do
            th2 <- coerceBasicTheory lid OWL2 "" th
            let owltext = shows (OWL2.printOWLBasicTheory th2) "\n"
            case parse (OWL2.basicSpec >> eof) f owltext of
              Left err -> putIfVerbose opts 0 $ show err
              _ -> putIfVerbose opts 3 $ "reparsed: " ++ f
            writeVerbFile opts f owltext
      | otherwise -> putIfVerbose opts 0 $ "expected OWL theory for: " ++ f
#endif
#ifdef RDFLOGIC
    RDFOut
        | lang == language_name RDF -> do
            th2 <- coerceBasicTheory lid RDF "" th
            let rdftext = shows (RDF.printRDFBasicTheory th2) "\n"
            writeVerbFile opts f rdftext
        | otherwise -> putIfVerbose opts 0 $ "expected RDF theory for: " ++ f
#endif
    CLIFOut
      | lang == language_name CommonLogic -> do
            (_, th2) <- coerceBasicTheory lid CommonLogic "" th
            let cltext = shows (CL_AS.exportCLIF th2) "\n"
            case parse (many CL_Parse.cltext >> eof) f cltext of
              Left err -> putIfVerbose opts 0 $ show err
              _ -> putIfVerbose opts 3 $ "reparsed: " ++ f
            writeVerbFile opts f cltext
      | otherwise -> putIfVerbose opts 0 $ "expected Common Logic theory for: "
                                                                            ++ f
    _ -> return () -- ignore other file types

modelSparQCheck :: HetcatsOpts -> G_theory -> IRI -> IO ()
modelSparQCheck opts gTh@(G_theory lid (ExtSign sign0 _) _ sens0 _) i =
    case coerceBasicTheory lid CASL "" (sign0, toNamedList sens0) of
    Just th2 -> do
      table <- parseSparQTableFromFile $ modelSparQ opts
      case table of
        Left err -> putIfVerbose opts 0
          $ "could not parse SparQTable from file: " ++ modelSparQ opts
          ++ "\n" ++ show err
        Right y -> do
            putIfVerbose opts 4 $ unlines
               ["lisp file content:", show $ table2Doc y, "lisp file end."]
            let Result d _ = modelCheck i th2 y
            if null d then
                putIfVerbose opts 0 "Modelcheck succeeded, no errors found"
             else showDiags
               (if verbose opts >= 2 then opts else opts {verbose = 2})
               $ reverse d
    _ ->
      putIfVerbose opts 0 $ "could not translate Theory to CASL:\n "
         ++ showDoc gTh ""

writeTheoryFiles :: HetcatsOpts -> [OutType] -> FilePath -> LibEnv
                 -> GlobalAnnos -> LibName -> IRI -> Int -> IO ()
writeTheoryFiles opts specOutTypes filePrefix lenv ga ln i n =
  let dg = lookupDGraph ln lenv
      nam = getDGNodeName $ labDG dg n
      ins = getImportNames dg n
  in case globalNodeTheory dg n of
      Nothing -> putIfVerbose opts 0 $ "could not compute theory of spec "
                 ++ show i
      Just raw_gTh0 -> do
            let tr = transNames opts
                Result es mTh = if null tr then return (raw_gTh0, "") else do
                   comor <- lookupCompComorphism (map tokStr tr) logicGraph
                   tTh <- mapG_theory comor raw_gTh0
                   return (tTh, show comor)
            showDiags opts es
            case mTh of
             Nothing ->
               putIfVerbose opts 0 "could not translate theory"
             Just (raw_gTh, tStr) -> do
               unless (null tStr) $
                   putIfVerbose opts 2 $ "Translated using comorphism " ++ tStr
               putIfVerbose opts 4 $ "Sublogic of " ++ show i ++ ": " ++
                   show (sublogicOfTh raw_gTh)
               unless (modelSparQ opts == "") $
                   modelSparQCheck opts (theoremsToAxioms raw_gTh) i
               mapM_ (writeTheory ins nam opts filePrefix ga raw_gTh ln i)
                 specOutTypes

writeSpecFiles :: HetcatsOpts -> FilePath -> LibEnv -> LibName -> DGraph
               -> IO ()
writeSpecFiles opts file lenv ln dg = do
    let gctx = globalEnv dg
        gns = Map.keys gctx
        mns = map $ \ t -> Map.findWithDefault (simpleIdToIRI t) (tokStr t)
          $ Map.fromList $ map (\ i -> (iriToStringShortUnsecure i, i)) gns
        ga = globalAnnos dg
        ns = mns $ specNames opts
        vs = mns $ viewNames opts
        filePrefix = snd $ getFilePrefix opts file
        outTypes = outtypes opts
        specOutTypes = filter ( \ ot -> case ot of
            ThyFile -> True
            DfgFile _ -> True
            TPTPFile _ -> True
            XmlOut -> True
            OmdocOut -> True
            TheoryFile _ -> True
            SigFile _ -> True
            OWLOut -> True
            CLIFOut -> True
            FreeCADOut -> True
            HaskellOut -> True
            ComptableXml -> True
            SymXml -> True
            _ -> False) outTypes
        allSpecs = null ns
        noViews = null vs
        ignore = null specOutTypes && modelSparQ opts == ""
    mapM_ (writeLibEnv opts filePrefix lenv ln) $
          if null $ dumpOpts opts then outTypes else EnvOut : outTypes
    mapM_ ( \ i -> case Map.lookup i gctx of
        Just (SpecEntry (ExtGenSig _ (NodeSig n _))) ->
            writeTheoryFiles opts specOutTypes filePrefix lenv ga ln i n
        _ -> unless allSpecs
               $ putIfVerbose opts 0 $ "Unknown spec name: " ++ show i
      ) $ if ignore then [] else
        if allSpecs then gns else ns
    unless noViews $
      mapM_ ( \ i -> case Map.lookup i gctx of
        Just (ViewOrStructEntry _ (ExtViewSig _ (GMorphism cid _ _ m _) _)) ->
            writeVerbFile opts (filePrefix ++ "_" ++ show i ++ ".view")
              $ shows (pretty $ Map.toList $ symmap_of (targetLogic cid) m) "\n"
        _ -> putIfVerbose opts 0 $ "Unknown view name: " ++ show i
      ) vs
    mapM_ ( \ n ->
      writeTheoryFiles opts specOutTypes filePrefix lenv ga ln
         (simpleIdToIRI $ genToken $ 'n' : show n) n)
      $ if ignore || not allSpecs then [] else
      nodesDG dg
      \\ Map.fold ( \ e l -> case e of
            SpecEntry (ExtGenSig _ (NodeSig n _)) -> n : l
            _ -> l) [] gctx
    doDump opts "GlobalAnnos" $ putStrLn $ showGlobalDoc ga ga ""
    doDump opts "PrintStat" $ putStrLn $ printStatistics dg
    doDump opts "DGraph" $ putStrLn $ showDoc dg ""
    doDump opts "DuplicateDefEdges" $ let es = duplicateDefEdges dg in
      unless (null es) $ print es
    doDump opts "LogicGraph" $ putStrLn $ showDoc logicGraph ""
    doDump opts "LibEnv" $
               writeVerbFile opts (filePrefix ++ ".lenv") $
                    shows (DG.prettyLibEnv lenv) "\n"
