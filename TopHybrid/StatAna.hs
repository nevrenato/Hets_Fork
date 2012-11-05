{- |
Module      :  $Header$
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  nevrenato@gmail.com 
Stability   :  provisional
Portability :  portable

Description : 
Static analysis of hybrid logic with an 
arbitrary logic below.
-}

module TopHybrid.StatAna (thAna) where

import TopHybrid.AS_TopHybrid
import TopHybrid.Print_AS
import TopHybrid.TopHybridSign
import Common.GlobalAnnotations
import Common.Result
import Common.ExtSign
import Common.AS_Annotation
import CASL.AS_Basic_CASL
import CASL.Sign
thAna :: (Spec_Wrapper, Sign Form_Wrapper Sign_Wrapper, GlobalAnnos) -> 
        Result (Spec_Wrapper, ExtSign (Sign Form_Wrapper Sign_Wrapper) Symbol, [Named Form_Wrapper])
thAna (b, s, _)= error $ show s ++ ("\n")  ++(show b) 
