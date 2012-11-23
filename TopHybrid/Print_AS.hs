{- |
Module      :  $Header$
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  nevrenato@gmail.com 
Stability   :  provisional
Portability :  portable

Description : 
Instance of class Pretty for hybrid logic
with an arbitrary logic below.
-}

module TopHybrid.Print_AS where

import Common.Doc
import Common.DocUtils
import TopHybrid.AS_TopHybrid
import TopHybrid.TopHybridSign

instance (Pretty f) => Pretty (TH_FORMULA f) where
        pretty (At n f) = keyword "@" <+> (pretty n) <+> (pretty f) 
        pretty (UnderLogic f) = keyword "{" <+> (pretty f) <+> (keyword "}") 
        pretty (Box m f) = keyword "[" <> (pretty m) <> keyword "]" <+> (pretty f) 
        pretty (Dia m f) = keyword "<" <> (pretty m) <> keyword ">" <+> (pretty f) 
        pretty (Conjunction f f') = pretty f <+> (keyword "/\\") <+> (pretty f')
        pretty (Disjunction f f') = pretty f <+> (keyword "\\/") <+> (pretty f')
        pretty (Implication f f') = pretty f <+> (keyword "->") <+> (pretty f')
        pretty (BiImplication f f') = pretty f <+> (keyword "<->") <+> (pretty f')
        pretty (Here n) = pretty n
        pretty (Neg f) = keyword "not" <+> (pretty f)
        pretty (Par f) = keyword "(" <+> (pretty f) <+> (keyword ")")

instance (Pretty s) => Pretty (THybridSign s) where
        pretty x@(THybridSign _ _ s) =    
                keyword "Modalities" <+> (pretty $ modies x) $+$
                keyword "Nominals" <+> (pretty $ nomies x) $+$
                keyword "Under Sig {" $+$ (pretty s) $+$ (keyword "}") 

instance (Pretty b) => Pretty (TH_BSPEC b) where
        pretty (Bspec x b) =    pretty x 
                                $+$ keyword "Under Spec {" $+$ 
                                (pretty b) 
                                $+$ keyword "}"

instance Pretty (TH_BASIC_ITEM) where 
        pretty (Simple_mod_decl x) = keyword "Modalities" <+> (pretty x)
        pretty (Simple_nom_decl x) = keyword "Nominals" <+> (pretty x)

instance Pretty Form_Wrapper where
        pretty (Form_Wrapper f) = pretty f
instance Pretty Sign_Wrapper where
        pretty (Sign_Wrapper s) = pretty s
instance Pretty Spec_Wrapper where
        pretty (Spec_Wrapper _ b f) = pretty b $+$ (pretty f)
instance Pretty Mor where 
        
