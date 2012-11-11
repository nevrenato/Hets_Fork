{-# LANGUAGE CPP #-}
{- |
Module      :  $Header$
Copyright   :  Eugen Kuksa 2011
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  eugenk@informatik.uni-bremen.de
Stability   :  provisional
Portability :  non-portable (imports Logic.Logic)

Analyses CommonLogic files.
-}

module CommonLogic.ParseCLAsLibDefn (parseCL_CLIF) where

import Common.Id
import Common.IRI (simpleIdToIRI)
import Common.LibName
import Common.AS_Annotation as Anno
import Common.AnnoState
import Common.DocUtils

import Driver.Options

import Text.ParserCombinators.Parsec

import Logic.Grothendieck

import CommonLogic.AS_CommonLogic as CL
import CommonLogic.Logic_CommonLogic
import CommonLogic.Parse_CLIF (basicSpec)

import Syntax.AS_Library
import Syntax.AS_Structured as Struc

import Control.Monad (foldM)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sortBy)

import System.FilePath (combine, splitFileName, addExtension)
import System.Directory (doesFileExist, getCurrentDirectory)

import Network.URI
#ifndef NOHTTP
import Network.HTTP
import Network.Stream (Result)
#endif

type SpecMap = Map String SpecInfo
type SpecInfo = (BASIC_SPEC, Set String, Set String)
                -- (spec, topTexts, importedBy)

-- | call for CommonLogic CLIF-parser with recursive inclusion of importations
parseCL_CLIF :: FilePath -> HetcatsOpts -> IO LIB_DEFN
parseCL_CLIF filename opts = do
  let dirFile@(dir,_) = splitFileName filename
  specMap <- downloadSpec opts Map.empty Set.empty Set.empty False dirFile
  specs <- anaImports opts dir specMap
  return $ convertToLibDefN (convertFileToLibStr filename) specs


-- call for CommonLogic CLIF-parser for a single file
parseCL_CLIF_contents :: FilePath -> String -> Either ParseError [BASIC_SPEC]
parseCL_CLIF_contents = runParser (many basicSpec) (emptyAnnos ())

{- maps imports in basic spec to global definition links (extensions) in
development graph -}
convertToLibDefN :: String -> [(BASIC_SPEC, NAME)] -> LIB_DEFN
convertToLibDefN fn specs =
  Lib_defn
    (emptyLibName fn)
    (emptyAnno (Logic_decl (Logic_name
                              (simpleIdToIRI $ mkSimpleId $ show CommonLogic)
                              Nothing
                              Nothing
                          ) Nothing nullRange)
      : map convertToLibItems specs
    )
    nullRange
    []

convertToLibItems :: (BASIC_SPEC, NAME) -> Anno.Annoted LIB_ITEM
convertToLibItems (b, n) =
  emptyAnno $ Spec_defn (simpleIdToIRI n) emptyGenericity (createSpec b) nullRange

createSpec :: BASIC_SPEC -> Anno.Annoted SPEC
createSpec b =
  let imports = Set.elems $ directImports b
      bs = emptyAnno $ Struc.Basic_spec (G_basic_spec CommonLogic b) nullRange
  in case imports of
          [] -> bs
          _ -> emptyAnno $ Extension [
              case imports of
                [n] -> specFromName n
                _ -> emptyAnno $ Union (map specFromName imports) nullRange
              , bs
            ] nullRange

specFromName :: NAME -> Annoted SPEC
specFromName n = emptyAnno $ Spec_inst (simpleIdToIRI $ cnvImportName n) [] nullRange

specNameL :: [BASIC_SPEC] -> String -> [String]
specNameL [_] def = [def]
specNameL bs def = map (specName def) [0 .. (length bs)]

-- returns a unique name for a node
specName :: String -> Int -> String
specName def i = def ++ "_" ++ show i

cnvImportName :: NAME -> NAME
cnvImportName = mkSimpleId . convertFileToLibStr . tokStr

collectDownloads :: HetcatsOpts -> String -> SpecMap -> (String, SpecInfo)
                    -> IO SpecMap
collectDownloads opts dir specMap (n, (b, topTexts, importedBy)) =
  let directImps = Set.elems $ Set.map tokStr $ directImports b
      newTopTexts = Set.insert n topTexts
      newImportedBy = Set.insert n importedBy
  in  foldM (\ sm d -> do
          newDls <- downloadSpec opts sm newTopTexts newImportedBy True (dir,d)
          return (Map.unionWith unify newDls sm)
        ) specMap directImps -- imports get @n@ as new "importedBy"

downloadSpec :: HetcatsOpts -> SpecMap -> Set String -> Set String
                -> Bool -> (String,String) -> IO SpecMap
downloadSpec opts specMap topTexts importedBy isImport dirFile@(dir,_) =
  let filename = uncurry combine dirFile in
  let fn = convertFileToLibStr filename in
  case Map.lookup fn specMap of
      Just (b, t, i)
        | isImport && Set.member (convertFileToLibStr filename) importedBy
          -> error (concat [
                    "Illegal cyclic import: ", show (pretty importedBy), "\n"
                  , "Hets currently cannot handle cyclic imports "
                  , "of Common Logic files. "
                  , "If you really need them, send us a message at "
                  , "hets@informatik.uni-bremen.de, and we will fix it."
                ])
        | t == topTexts
          -> return specMap
        | otherwise -> do
          let newTopTexts = t `Set.union` topTexts
          let newImportedBy = i `Set.union` importedBy
          let newSpecMap = Map.insert fn (b, newTopTexts, newImportedBy) specMap
          collectDownloads opts dir newSpecMap (fn, (b, newTopTexts, newImportedBy))
      Nothing -> do
          contents <- getCLIFContents opts dirFile
          case parseCL_CLIF_contents filename contents of
              Left err -> error $ show err
              Right bs ->
                let nbs = zip (specNameL bs fn) bs
                    nbtis = map (\ (n, b) -> (n, (b, topTexts, importedBy))) nbs
                    newSpecMap = foldr (\ (n, bti) sm ->
                        Map.insertWith unify n bti sm
                      ) specMap nbtis
                in foldM (\ sm nbt -> do
                          newDls <- collectDownloads opts dir sm nbt
                          return (Map.unionWith unify newDls sm)
                      ) newSpecMap nbtis

unify :: SpecInfo -> SpecInfo -> SpecInfo
unify (_, s, p) (a, t, q) = (a, s `Set.union` t, p `Set.union` q)

{- one could add support for uri fragments/query
(perhaps select a text from the file) -}
getCLIFContents :: HetcatsOpts -> (String,String) -> IO String
getCLIFContents opts dirFile@(_,file) =
  let filename = uncurry combine dirFile in
  case parseURIReference filename of
    Nothing -> do
      putStrLn ("Not an URI: " ++ filename)
      localFileContents opts file
    Just uri ->
      case uriScheme uri of
        "" ->
          localFileContents opts (uriToString id uri "")
        "file:" ->
          localFileContents opts (uriPath uri)
#ifndef NOHTTP
        "http:" -> getCLIFContentsHTTP filename ""
        "https:" ->
          simpleHTTP (defaultGETRequest uri) >>= getResponseBody
#endif
        x -> error ("Unsupported URI scheme: " ++ x)

#ifndef NOHTTP
getCLIFContentsHTTP :: String -> String -> IO String
getCLIFContentsHTTP uriS extension =
  let (Just uri) = parseURIReference (uriS ++ extension) in do
    res <- simpleHTTP $ defaultGETRequest uri
    rb <- getResponseBody res
    case httpResponseCode res of
        (2,0,0) -> return rb
        (x,y,z) -> case extension of
          ""     -> getCLIFContentsHTTP uriS ".clf"
          ".clf" -> getCLIFContentsHTTP uriS ".clif"
          _      -> error $ "File not found via HTTP: " ++ uriS ++ "[.clf | .clif]\nHTTP-code " ++ show x ++ show y ++ show z

httpResponseCode :: Result (Response a) -> (Int, Int, Int)
httpResponseCode res = case res of
    Left _ -> (0,0,0)
    Right r -> rspCode r
#endif

localFileContents :: HetcatsOpts -> String -> IO String
localFileContents opts filename = do
  curDir <- getCurrentDirectory
  file <- findLibFile (curDir : libdirs opts) filename
  readFile file

findLibFile :: [FilePath] -> String -> IO FilePath
findLibFile ds f = do
  e <- doesFileExist f
  if e then return f else findLibFileAux ds f

findLibFileAux :: [FilePath] -> String -> IO FilePath
findLibFileAux [] f = error $ "Could not find Common Logic Library " ++ f
findLibFileAux (d : ds) f = do
  let fs = [ combine d $ addExtension f $ show $ CommonLogicIn b
           | b <- [False, True]]
  es <- mapM doesFileExist fs
  case filter fst $ zip es fs of
        [] -> findLibFileAux ds f
        (_, f0) : _ -> return f0

-- retrieves all importations from the text
directImports :: BASIC_SPEC -> Set NAME
directImports (CL.Basic_spec items) = Set.unions
  $ map (getImports_textMetas . textsFromBasicItems . Anno.item) items

textsFromBasicItems :: BASIC_ITEMS -> [TEXT_META]
textsFromBasicItems (Axiom_items axs) = map Anno.item axs

getImports_textMetas :: [TEXT_META] -> Set NAME
getImports_textMetas tms = Set.unions $ map (getImports_text . getText) tms

getImports_text :: TEXT -> Set NAME
getImports_text (Named_text _ t _) = getImports_text t
getImports_text (Text p _) = Set.fromList $ map impName $ filter isImportation p

isImportation :: PHRASE -> Bool
isImportation (Importation _) = True
isImportation _ = False

impName :: PHRASE -> NAME
impName (Importation (Imp_name n)) = n
impName _ = undefined -- not necessary because filtered out

anaImports :: HetcatsOpts -> String -> SpecMap -> IO [(BASIC_SPEC, NAME)]
anaImports opts dir specMap = do
  downloads <- foldM
    (\ sm nbt -> do
      newSpecs <- collectDownloads opts dir sm nbt
      return (Map.unionWith unify newSpecs sm)
    ) specMap $ Map.assocs specMap
  let specAssocs = Map.assocs downloads
  let sortedSpecs = sortBy usingImportedByCount specAssocs
      -- sort by putting the latest imported specs to the beginning
  return $ bsNamePairs sortedSpecs

{- not fast (O(n+m)), but reliable -}
usingImportedByCount :: (String, SpecInfo) -> (String, SpecInfo) -> Ordering
usingImportedByCount (_, (_, _, importedBy1)) (_, (_, _, importedBy2)) =
  compare (Set.size importedBy2) (Set.size importedBy1)

bsNamePairs :: [(String, SpecInfo)] -> [(BASIC_SPEC, NAME)]
bsNamePairs = foldr (\ (n, (b, _, _)) r -> (b, mkSimpleId n) : r) []
