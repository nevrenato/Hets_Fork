module Isabelle.IsaExport where

import Text.XML.HaXml.XmlContent hiding (Const)
import Text.XML.HaXml.OneOfN
import Text.XML.HaXml.Types


{-Type decls-}

data IsaExport = IsaExport IsaExport_Attrs Imports Consts Axioms
                           Theorems Types
               deriving (Eq,Show)
data IsaExport_Attrs = IsaExport_Attrs
    { isaExportFile :: String
    } deriving (Eq,Show)
newtype Imports = Imports [Import]   deriving (Eq,Show)
data Import = Import
    { importName :: String
    } deriving (Eq,Show)
newtype Consts = Consts [ConstDecl]   deriving (Eq,Show)
data ConstDecl = ConstDecl ConstDecl_Attrs (OneOf3 TVar TFree Type)
                           (OneOf2 Term NoTerm)
               deriving (Eq,Show)
data ConstDecl_Attrs = ConstDecl_Attrs
    { constDeclName :: String
    } deriving (Eq,Show)
newtype Axioms = Axioms [Term]   deriving (Eq,Show)
newtype Theorems = Theorems [Term]   deriving (Eq,Show)
newtype Types = Types [TypeDecl]   deriving (Eq,Show)
data NoTerm = NoTerm  deriving (Eq,Show)
data Term = TermBound Term_Attrs Bound
          | TermFree Term_Attrs Free
          | TermVar Term_Attrs Var
          | TermConst Term_Attrs Const
          | TermApp Term_Attrs App
          | TermAbs Term_Attrs Abs
          deriving (Eq,Show)
data Term_Attrs = Term_Attrs
    { termName :: String
    } deriving (Eq,Show)
data TypeDecl = TypeDecl TypeDecl_Attrs [RecType]
              deriving (Eq,Show)
data TypeDecl_Attrs = TypeDecl_Attrs
    { typeDeclName :: String
    } deriving (Eq,Show)
data RecType = RecType RecType_Attrs Vars Constructors
             deriving (Eq,Show)
data RecType_Attrs = RecType_Attrs
    { recTypeI :: String
    , recTypeName :: String
    , recTypeAltname :: (Maybe String)
    } deriving (Eq,Show)
newtype Vars = Vars [Vars_]   deriving (Eq,Show)
data Vars_ = Vars_DtTFree DtTFree
           | Vars_DtType DtType
           | Vars_DtRec DtRec
           deriving (Eq,Show)
data DtTFree = DtTFree
    { dtTFreeS :: String
    } deriving (Eq,Show)
data DtType = DtType DtType_Attrs [DtType_]
            deriving (Eq,Show)
data DtType_Attrs = DtType_Attrs
    { dtTypeS :: String
    } deriving (Eq,Show)
data DtType_ = DtType_DtTFree DtTFree
             | DtType_DtType DtType
             | DtType_DtRec DtRec
             deriving (Eq,Show)
data DtRec = DtRec
    { dtRecI :: String
    } deriving (Eq,Show)
newtype Constructors = Constructors [Constructor]   deriving (Eq,Show)
data Constructor = Constructor Constructor_Attrs [Constructor_]
                 deriving (Eq,Show)
data Constructor_Attrs = Constructor_Attrs
    { constructorVal :: String
    } deriving (Eq,Show)
data Constructor_ = Constructor_DtTFree DtTFree
                  | Constructor_DtType DtType
                  | Constructor_DtRec DtRec
                  deriving (Eq,Show)
data Bound = Bound
    { boundIndex :: String
    } deriving (Eq,Show)
data Free = FreeTVar Free_Attrs TVar
          | FreeTFree Free_Attrs TFree
          | FreeType Free_Attrs Type
          deriving (Eq,Show)
data Free_Attrs = Free_Attrs
    { freeName :: String
    } deriving (Eq,Show)
data Var = VarTVar Var_Attrs TVar
         | VarTFree Var_Attrs TFree
         | VarType Var_Attrs Type
         deriving (Eq,Show)
data Var_Attrs = Var_Attrs
    { varName :: String
    , varIndex :: (Maybe String)
    } deriving (Eq,Show)
data Const = ConstTVar Const_Attrs TVar
           | ConstTFree Const_Attrs TFree
           | ConstType Const_Attrs Type
           deriving (Eq,Show)
data Const_Attrs = Const_Attrs
    { constName :: String
    , constInfix :: (Maybe String)
    , constInfixl :: (Maybe String)
    , constInfixr :: (Maybe String)
    , constMixfix_i :: (Maybe String)
    } deriving (Eq,Show)
data App = App (OneOf6 Bound Free Var Const App Abs)
               (OneOf6 Bound Free Var Const App Abs)
         deriving (Eq,Show)
data Abs = Abs Abs_Attrs (OneOf3 TVar TFree Type)
               (OneOf6 Bound Free Var Const App Abs)
         deriving (Eq,Show)
data Abs_Attrs = Abs_Attrs
    { absVname :: String
    } deriving (Eq,Show)
data TVar = TVar TVar_Attrs [Class]
          deriving (Eq,Show)
data TVar_Attrs = TVar_Attrs
    { tVarName :: String
    , tVarIndex :: (Maybe String)
    } deriving (Eq,Show)
data TFree = TFree TFree_Attrs [Class]
           deriving (Eq,Show)
data TFree_Attrs = TFree_Attrs
    { tFreeName :: String
    } deriving (Eq,Show)
data Type = Type Type_Attrs [Type_]
          deriving (Eq,Show)
data Type_Attrs = Type_Attrs
    { typeName :: String
    } deriving (Eq,Show)
data Type_ = Type_TVar TVar
           | Type_TFree TFree
           | Type_Type Type
           deriving (Eq,Show)
data Class = Class
    { className :: String
    } deriving (Eq,Show)


{-Instance decls-}

instance HTypeable IsaExport where
    toHType _ = Defined "IsaExport" [] []
instance XmlContent IsaExport where
    toContents (IsaExport as a b c d e) =
        [CElem (Elem (N "IsaExport") (toAttrs as) (toContents a ++
                                                   toContents b ++ toContents c ++ toContents d ++
                                                   toContents e)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["IsaExport"]
        ; interior e $ return (IsaExport (fromAttrs as))
                       `apply` parseContents `apply` parseContents `apply` parseContents
                       `apply` parseContents `apply` parseContents
        } `adjustErr` ("in <IsaExport>, "++)
instance XmlAttributes IsaExport_Attrs where
    fromAttrs as =
        IsaExport_Attrs
          { isaExportFile = definiteA fromAttrToStr "IsaExport" "file" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "file" (isaExportFile v)
        ]

instance HTypeable Imports where
    toHType _ = Defined "Imports" [] []
instance XmlContent Imports where
    toContents (Imports a) =
        [CElem (Elem (N "Imports") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Imports"]
        ; interior e $ return (Imports) `apply` many parseContents
        } `adjustErr` ("in <Imports>, "++)

instance HTypeable Import where
    toHType _ = Defined "Import" [] []
instance XmlContent Import where
    toContents as =
        [CElem (Elem (N "Import") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["Import"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <Import>, "++)
instance XmlAttributes Import where
    fromAttrs as =
        Import
          { importName = definiteA fromAttrToStr "Import" "name" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "name" (importName v)
        ]

instance HTypeable Consts where
    toHType _ = Defined "Consts" [] []
instance XmlContent Consts where
    toContents (Consts a) =
        [CElem (Elem (N "Consts") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Consts"]
        ; interior e $ return (Consts) `apply` many parseContents
        } `adjustErr` ("in <Consts>, "++)

instance HTypeable ConstDecl where
    toHType _ = Defined "ConstDecl" [] []
instance XmlContent ConstDecl where
    toContents (ConstDecl as a b) =
        [CElem (Elem (N "ConstDecl") (toAttrs as) (toContents a ++
                                                   toContents b)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["ConstDecl"]
        ; interior e $ return (ConstDecl (fromAttrs as))
                       `apply` parseContents `apply` parseContents
        } `adjustErr` ("in <ConstDecl>, "++)
instance XmlAttributes ConstDecl_Attrs where
    fromAttrs as =
        ConstDecl_Attrs
          { constDeclName = definiteA fromAttrToStr "ConstDecl" "name" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "name" (constDeclName v)
        ]

instance HTypeable Axioms where
    toHType _ = Defined "Axioms" [] []
instance XmlContent Axioms where
    toContents (Axioms a) =
        [CElem (Elem (N "Axioms") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Axioms"]
        ; interior e $ return (Axioms) `apply` many parseContents
        } `adjustErr` ("in <Axioms>, "++)

instance HTypeable Theorems where
    toHType _ = Defined "Theorems" [] []
instance XmlContent Theorems where
    toContents (Theorems a) =
        [CElem (Elem (N "Theorems") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Theorems"]
        ; interior e $ return (Theorems) `apply` many parseContents
        } `adjustErr` ("in <Theorems>, "++)

instance HTypeable Types where
    toHType _ = Defined "Types" [] []
instance XmlContent Types where
    toContents (Types a) =
        [CElem (Elem (N "Types") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Types"]
        ; interior e $ return (Types) `apply` many parseContents
        } `adjustErr` ("in <Types>, "++)

instance HTypeable NoTerm where
    toHType _ = Defined "NoTerm" [] []
instance XmlContent NoTerm where
    toContents NoTerm =
        [CElem (Elem (N "NoTerm") [] []) ()]
    parseContents = do
        { (Elem _ _ []) <- element ["NoTerm"]
        ; return NoTerm
        } `adjustErr` ("in <NoTerm>, "++)

instance HTypeable Term where
    toHType _ = Defined "Term" [] []
instance XmlContent Term where
    toContents (TermBound as a) =
        [CElem (Elem (N "Term") (toAttrs as) (toContents a) ) ()]
    toContents (TermFree as a) =
        [CElem (Elem (N "Term") (toAttrs as) (toContents a) ) ()]
    toContents (TermVar as a) =
        [CElem (Elem (N "Term") (toAttrs as) (toContents a) ) ()]
    toContents (TermConst as a) =
        [CElem (Elem (N "Term") (toAttrs as) (toContents a) ) ()]
    toContents (TermApp as a) =
        [CElem (Elem (N "Term") (toAttrs as) (toContents a) ) ()]
    toContents (TermAbs as a) =
        [CElem (Elem (N "Term") (toAttrs as) (toContents a) ) ()]
    parseContents = do 
        { e@(Elem _ as _) <- element ["Term"]
        ; interior e $ oneOf
            [ return (TermBound (fromAttrs as)) `apply` parseContents
            , return (TermFree (fromAttrs as)) `apply` parseContents
            , return (TermVar (fromAttrs as)) `apply` parseContents
            , return (TermConst (fromAttrs as)) `apply` parseContents
            , return (TermApp (fromAttrs as)) `apply` parseContents
            , return (TermAbs (fromAttrs as)) `apply` parseContents
            ] `adjustErr` ("in <Term>, "++)
        }
instance XmlAttributes Term_Attrs where
    fromAttrs as =
        Term_Attrs
          { termName = definiteA fromAttrToStr "Term" "name" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "name" (termName v)
        ]

instance HTypeable TypeDecl where
    toHType _ = Defined "TypeDecl" [] []
instance XmlContent TypeDecl where
    toContents (TypeDecl as a) =
        [CElem (Elem (N "TypeDecl") (toAttrs as) (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["TypeDecl"]
        ; interior e $ return (TypeDecl (fromAttrs as))
                       `apply` many parseContents
        } `adjustErr` ("in <TypeDecl>, "++)
instance XmlAttributes TypeDecl_Attrs where
    fromAttrs as =
        TypeDecl_Attrs
          { typeDeclName = definiteA fromAttrToStr "TypeDecl" "name" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "name" (typeDeclName v)
        ]

instance HTypeable RecType where
    toHType _ = Defined "RecType" [] []
instance XmlContent RecType where
    toContents (RecType as a b) =
        [CElem (Elem (N "RecType") (toAttrs as) (toContents a ++
                                                 toContents b)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["RecType"]
        ; interior e $ return (RecType (fromAttrs as))
                       `apply` parseContents `apply` parseContents
        } `adjustErr` ("in <RecType>, "++)
instance XmlAttributes RecType_Attrs where
    fromAttrs as =
        RecType_Attrs
          { recTypeI = definiteA fromAttrToStr "RecType" "i" as
          , recTypeName = definiteA fromAttrToStr "RecType" "name" as
          , recTypeAltname = possibleA fromAttrToStr "altname" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "i" (recTypeI v)
        , toAttrFrStr "name" (recTypeName v)
        , maybeToAttr toAttrFrStr "altname" (recTypeAltname v)
        ]

instance HTypeable Vars where
    toHType _ = Defined "Vars" [] []
instance XmlContent Vars where
    toContents (Vars a) =
        [CElem (Elem (N "Vars") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Vars"]
        ; interior e $ return (Vars) `apply` many parseContents
        } `adjustErr` ("in <Vars>, "++)

instance HTypeable Vars_ where
    toHType _ = Defined "Vars" [] []
instance XmlContent Vars_ where
    toContents (Vars_DtTFree a) = toContents a
    toContents (Vars_DtType a) = toContents a
    toContents (Vars_DtRec a) = toContents a
    parseContents = oneOf
        [ return (Vars_DtTFree) `apply` parseContents
        , return (Vars_DtType) `apply` parseContents
        , return (Vars_DtRec) `apply` parseContents
        ] `adjustErr` ("in <Vars>, "++)

instance HTypeable DtTFree where
    toHType _ = Defined "DtTFree" [] []
instance XmlContent DtTFree where
    toContents as =
        [CElem (Elem (N "DtTFree") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["DtTFree"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <DtTFree>, "++)
instance XmlAttributes DtTFree where
    fromAttrs as =
        DtTFree
          { dtTFreeS = definiteA fromAttrToStr "DtTFree" "s" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "s" (dtTFreeS v)
        ]

instance HTypeable DtType where
    toHType _ = Defined "DtType" [] []
instance XmlContent DtType where
    toContents (DtType as a) =
        [CElem (Elem (N "DtType") (toAttrs as) (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["DtType"]
        ; interior e $ return (DtType (fromAttrs as))
                       `apply` many parseContents
        } `adjustErr` ("in <DtType>, "++)
instance XmlAttributes DtType_Attrs where
    fromAttrs as =
        DtType_Attrs
          { dtTypeS = definiteA fromAttrToStr "DtType" "s" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "s" (dtTypeS v)
        ]

instance HTypeable DtType_ where
    toHType _ = Defined "DtType" [] []
instance XmlContent DtType_ where
    toContents (DtType_DtTFree a) = toContents a
    toContents (DtType_DtType a) = toContents a
    toContents (DtType_DtRec a) = toContents a
    parseContents = oneOf
        [ return (DtType_DtTFree) `apply` parseContents
        , return (DtType_DtType) `apply` parseContents
        , return (DtType_DtRec) `apply` parseContents
        ] `adjustErr` ("in <DtType>, "++)

instance HTypeable DtRec where
    toHType _ = Defined "DtRec" [] []
instance XmlContent DtRec where
    toContents as =
        [CElem (Elem (N "DtRec") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["DtRec"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <DtRec>, "++)
instance XmlAttributes DtRec where
    fromAttrs as =
        DtRec
          { dtRecI = definiteA fromAttrToStr "DtRec" "i" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "i" (dtRecI v)
        ]

instance HTypeable Constructors where
    toHType _ = Defined "Constructors" [] []
instance XmlContent Constructors where
    toContents (Constructors a) =
        [CElem (Elem (N "Constructors") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Constructors"]
        ; interior e $ return (Constructors) `apply` many parseContents
        } `adjustErr` ("in <Constructors>, "++)

instance HTypeable Constructor where
    toHType _ = Defined "Constructor" [] []
instance XmlContent Constructor where
    toContents (Constructor as a) =
        [CElem (Elem (N "Constructor") (toAttrs as) (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["Constructor"]
        ; interior e $ return (Constructor (fromAttrs as))
                       `apply` many parseContents
        } `adjustErr` ("in <Constructor>, "++)
instance XmlAttributes Constructor_Attrs where
    fromAttrs as =
        Constructor_Attrs
          { constructorVal = definiteA fromAttrToStr "Constructor" "val" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "val" (constructorVal v)
        ]

instance HTypeable Constructor_ where
    toHType _ = Defined "Constructor" [] []
instance XmlContent Constructor_ where
    toContents (Constructor_DtTFree a) = toContents a
    toContents (Constructor_DtType a) = toContents a
    toContents (Constructor_DtRec a) = toContents a
    parseContents = oneOf
        [ return (Constructor_DtTFree) `apply` parseContents
        , return (Constructor_DtType) `apply` parseContents
        , return (Constructor_DtRec) `apply` parseContents
        ] `adjustErr` ("in <Constructor>, "++)

instance HTypeable Bound where
    toHType _ = Defined "Bound" [] []
instance XmlContent Bound where
    toContents as =
        [CElem (Elem (N "Bound") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["Bound"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <Bound>, "++)
instance XmlAttributes Bound where
    fromAttrs as =
        Bound
          { boundIndex = definiteA fromAttrToStr "Bound" "index" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "index" (boundIndex v)
        ]

instance HTypeable Free where
    toHType _ = Defined "Free" [] []
instance XmlContent Free where
    toContents (FreeTVar as a) =
        [CElem (Elem (N "Free") (toAttrs as) (toContents a) ) ()]
    toContents (FreeTFree as a) =
        [CElem (Elem (N "Free") (toAttrs as) (toContents a) ) ()]
    toContents (FreeType as a) =
        [CElem (Elem (N "Free") (toAttrs as) (toContents a) ) ()]
    parseContents = do 
        { e@(Elem _ as _) <- element ["Free"]
        ; interior e $ oneOf
            [ return (FreeTVar (fromAttrs as)) `apply` parseContents
            , return (FreeTFree (fromAttrs as)) `apply` parseContents
            , return (FreeType (fromAttrs as)) `apply` parseContents
            ] `adjustErr` ("in <Free>, "++)
        }
instance XmlAttributes Free_Attrs where
    fromAttrs as =
        Free_Attrs
          { freeName = definiteA fromAttrToStr "Free" "name" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "name" (freeName v)
        ]

instance HTypeable Var where
    toHType _ = Defined "Var" [] []
instance XmlContent Var where
    toContents (VarTVar as a) =
        [CElem (Elem (N "Var") (toAttrs as) (toContents a) ) ()]
    toContents (VarTFree as a) =
        [CElem (Elem (N "Var") (toAttrs as) (toContents a) ) ()]
    toContents (VarType as a) =
        [CElem (Elem (N "Var") (toAttrs as) (toContents a) ) ()]
    parseContents = do 
        { e@(Elem _ as _) <- element ["Var"]
        ; interior e $ oneOf
            [ return (VarTVar (fromAttrs as)) `apply` parseContents
            , return (VarTFree (fromAttrs as)) `apply` parseContents
            , return (VarType (fromAttrs as)) `apply` parseContents
            ] `adjustErr` ("in <Var>, "++)
        }
instance XmlAttributes Var_Attrs where
    fromAttrs as =
        Var_Attrs
          { varName = definiteA fromAttrToStr "Var" "name" as
          , varIndex = possibleA fromAttrToStr "index" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "name" (varName v)
        , maybeToAttr toAttrFrStr "index" (varIndex v)
        ]

instance HTypeable Const where
    toHType _ = Defined "Const" [] []
instance XmlContent Const where
    toContents (ConstTVar as a) =
        [CElem (Elem (N "Const") (toAttrs as) (toContents a) ) ()]
    toContents (ConstTFree as a) =
        [CElem (Elem (N "Const") (toAttrs as) (toContents a) ) ()]
    toContents (ConstType as a) =
        [CElem (Elem (N "Const") (toAttrs as) (toContents a) ) ()]
    parseContents = do 
        { e@(Elem _ as _) <- element ["Const"]
        ; interior e $ oneOf
            [ return (ConstTVar (fromAttrs as)) `apply` parseContents
            , return (ConstTFree (fromAttrs as)) `apply` parseContents
            , return (ConstType (fromAttrs as)) `apply` parseContents
            ] `adjustErr` ("in <Const>, "++)
        }
instance XmlAttributes Const_Attrs where
    fromAttrs as =
        Const_Attrs
          { constName = definiteA fromAttrToStr "Const" "name" as
          , constInfix = possibleA fromAttrToStr "infix" as
          , constInfixl = possibleA fromAttrToStr "infixl" as
          , constInfixr = possibleA fromAttrToStr "infixr" as
          , constMixfix_i = possibleA fromAttrToStr "mixfix_i" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "name" (constName v)
        , maybeToAttr toAttrFrStr "infix" (constInfix v)
        , maybeToAttr toAttrFrStr "infixl" (constInfixl v)
        , maybeToAttr toAttrFrStr "infixr" (constInfixr v)
        , maybeToAttr toAttrFrStr "mixfix_i" (constMixfix_i v)
        ]

instance HTypeable App where
    toHType _ = Defined "App" [] []
instance XmlContent App where
    toContents (App a b) =
        [CElem (Elem (N "App") [] (toContents a ++ toContents b)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["App"]
        ; interior e $ return (App) `apply` parseContents
                       `apply` parseContents
        } `adjustErr` ("in <App>, "++)

instance HTypeable Abs where
    toHType _ = Defined "Abs" [] []
instance XmlContent Abs where
    toContents (Abs as a b) =
        [CElem (Elem (N "Abs") (toAttrs as) (toContents a ++
                                             toContents b)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["Abs"]
        ; interior e $ return (Abs (fromAttrs as)) `apply` parseContents
                       `apply` parseContents
        } `adjustErr` ("in <Abs>, "++)
instance XmlAttributes Abs_Attrs where
    fromAttrs as =
        Abs_Attrs
          { absVname = definiteA fromAttrToStr "Abs" "vname" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "vname" (absVname v)
        ]

instance HTypeable TVar where
    toHType _ = Defined "TVar" [] []
instance XmlContent TVar where
    toContents (TVar as a) =
        [CElem (Elem (N "TVar") (toAttrs as) (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["TVar"]
        ; interior e $ return (TVar (fromAttrs as))
                       `apply` many parseContents
        } `adjustErr` ("in <TVar>, "++)
instance XmlAttributes TVar_Attrs where
    fromAttrs as =
        TVar_Attrs
          { tVarName = definiteA fromAttrToStr "TVar" "name" as
          , tVarIndex = possibleA fromAttrToStr "index" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "name" (tVarName v)
        , maybeToAttr toAttrFrStr "index" (tVarIndex v)
        ]

instance HTypeable TFree where
    toHType _ = Defined "TFree" [] []
instance XmlContent TFree where
    toContents (TFree as a) =
        [CElem (Elem (N "TFree") (toAttrs as) (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["TFree"]
        ; interior e $ return (TFree (fromAttrs as))
                       `apply` many parseContents
        } `adjustErr` ("in <TFree>, "++)
instance XmlAttributes TFree_Attrs where
    fromAttrs as =
        TFree_Attrs
          { tFreeName = definiteA fromAttrToStr "TFree" "name" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "name" (tFreeName v)
        ]

instance HTypeable Type where
    toHType _ = Defined "Type" [] []
instance XmlContent Type where
    toContents (Type as a) =
        [CElem (Elem (N "Type") (toAttrs as) (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["Type"]
        ; interior e $ return (Type (fromAttrs as))
                       `apply` many parseContents
        } `adjustErr` ("in <Type>, "++)
instance XmlAttributes Type_Attrs where
    fromAttrs as =
        Type_Attrs
          { typeName = definiteA fromAttrToStr "Type" "name" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "name" (typeName v)
        ]

instance HTypeable Type_ where
    toHType _ = Defined "Type" [] []
instance XmlContent Type_ where
    toContents (Type_TVar a) = toContents a
    toContents (Type_TFree a) = toContents a
    toContents (Type_Type a) = toContents a
    parseContents = oneOf
        [ return (Type_TVar) `apply` parseContents
        , return (Type_TFree) `apply` parseContents
        , return (Type_Type) `apply` parseContents
        ] `adjustErr` ("in <Type>, "++)

instance HTypeable Class where
    toHType _ = Defined "class" [] []
instance XmlContent Class where
    toContents as =
        [CElem (Elem (N "class") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["class"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <class>, "++)
instance XmlAttributes Class where
    fromAttrs as =
        Class
          { className = definiteA fromAttrToStr "class" "name" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "name" (className v)
        ]



{-Done-}
