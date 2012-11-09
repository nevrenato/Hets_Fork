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
import Control.Arrow
import Data.List
import Data.Maybe

-- Note
-- Later, it is needed to propagate the errors from anaNomsMods
-- makeNamed :: String -> Sentence -> ...

thAna :: (Spec_Wrapper, Sign_Wrapper, GlobalAnnos) -> 
        Result (Spec_Wrapper, ExtSign Sign_Wrapper Symbol, [Named Form_Wrapper])

thAna (b, s, _) = hint (b, mkExtSign s', fs) (deb s' b) nullRange
        where   s' = fromMaybe s $ maybeResult $ anaNomsMods b s
                fs = anaForms b s'

-- | Need to check if the analyser does his work well
deb s b = ("Debug : \n\n") ++ 
          ("Signature :" ++ (show s)) ++ 
          ("Spec :" ++ (show b))

-- | Adds the new modalities and nominals to the signature, also 
-- checks for repetead declarations
anaNomsMods :: Spec_Wrapper -> Sign_Wrapper -> Result Sign_Wrapper
anaNomsMods b (Sign_Wrapper s) = 
        if (m,n) == x then return (Sign_Wrapper s') 
                else hint (Sign_Wrapper s') msg nullRange 
        where
        x = nomsMods b
        (m,n) = (nub *** nub) x 
        s' = s { modies = m, nomies = n }   
        msg = "Repeated nominals and/or modalities"

-- | Gets the new nominals and modalities
nomsMods :: Spec_Wrapper -> ([MODALITY],[NOMINAL])
nomsMods (Spec_Wrapper (Bspec ds _) _) = foldl f ([],[]) ds
        where   f (x,y) (Simple_mod_decl ms _) = (x ++ ms,y)
                f (x,y) (Simple_nom_decl ns _) = (x, y ++ ns)  

anaForms :: Spec_Wrapper -> Sign_Wrapper -> [Named Form_Wrapper]
anaForms (Spec_Wrapper _ fs) (Sign_Wrapper s) = map (makeNamed "") fs 
--map ((makeNamed "") . (anaForm (modies s,nomies s))) fs  

-- | Analyses a formula 
anaForm :: ([MODALITY],[NOMINAL]) -> Form_Wrapper -> Form_Wrapper
anaForm (ms,ns) (Form_Wrapper f) = 
        case f of 
           -- (At n _ _) -> if n `elem`ns then Form_Wrapper f else error "Nominal not found"
           _ -> Form_Wrapper f             
