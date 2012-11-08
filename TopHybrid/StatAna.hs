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
import Common.Id
import ATerm.Lib

thAna :: (Spec_Wrapper, Sign_Wrapper, GlobalAnnos) -> 
        Result (Spec_Wrapper, ExtSign Sign_Wrapper Symbol, [Named Form_Wrapper])
-- Debug purposes
thAna (b, s, _) = error $ ("Spitted info : \n\n") 
        ++ 
        (show $ anaNomsMods b s)  
        ++ 
        ("\n\n")  
        ++ 
        (show b) 

anaNomsMods :: Spec_Wrapper -> Sign_Wrapper -> Sign_Wrapper
anaNomsMods (Spec_Wrapper (Bspec ds _) _) (Sign_Wrapper s) = Sign_Wrapper $ foldl f s ds
        where   f x (Simple_mod_decl ms _) = x { modies = modies x ++ ms }
                f x (Simple_nom_decl ns _) = x { nomies = nomies x ++ ns }   
