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

instance (Show f) => Pretty (TH_FORMULA f) where
        pretty _ = pretty ()

instance (Show s) => Pretty (THybridSign s) where
        pretty _ = pretty () 

instance (Show b) => Pretty (TH_BSPEC b) where
        pretty _ = pretty () 

instance Pretty Form_Wrapper where
        pretty _ = pretty ()
instance Pretty Sign_Wrapper where
        pretty _ = pretty ()
instance Pretty Spec_Wrapper where
        pretty _ = pretty ()
instance Pretty Mor where 
        
