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

parCover :: (Pretty f) => f -> Doc
parCover f = (keyword "(") <+> (pretty f) <+> (keyword ")")
 
instance (Pretty f) => Pretty (TH_FORMULA f) where
        pretty (At n f) = keyword "@" <+> (parCover f) 
        pretty (UnderLogic f) = pretty f
        pretty (Box m f) = keyword "[" <> (pretty m) <> keyword "]" <+> (parCover f) 
        pretty (Dia m f) = keyword "<" <> (pretty m) <> keyword ">" <+> (parCover f) 
        pretty (Conjunction f f') = (parCover f) <+> (keyword "/\\") <+> (parCover f')
        pretty (Disjunction f f') = (parCover f) <+> (keyword "\\/") <+> (parCover f')
        pretty (Implication f f') = (parCover f) <+> (keyword "->") <+> (parCover f')
        pretty (BiImplication f f') = (parCover f) <+> (keyword "<->") <+> (parCover f')
        pretty (Here n) = pretty n
        pretty (Neg f) = keyword "not" <+> (parCover f)

instance (Pretty s) => Pretty (THybridSign s) where
        pretty (THybridSign _ _ s) =    keyword "modalities" $+$
                                        keyword "nominals" $+$
                                        keyword "Under Sig {" $+$
                                        (pretty s) $+$
                                        keyword "}" 

instance (Pretty b) => Pretty (TH_BSPEC b) where
        pretty (Bspec _ b) =    keyword "decls here" 
                                $+$ keyword "Under Spec {" $+$ 
                                (pretty b) 
                                $+$ keyword "}"

instance Pretty Form_Wrapper where
        pretty (Form_Wrapper f) = pretty f
instance Pretty Sign_Wrapper where
        pretty (Sign_Wrapper s) = pretty s
instance Pretty Spec_Wrapper where
        pretty (Spec_Wrapper _ b _) = pretty b
instance Pretty Mor where 
        
