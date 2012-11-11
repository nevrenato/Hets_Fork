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
import Control.Categorical.Bifunctor
import Data.List 
import Data.Maybe
import Data.Map as Map  hiding (foldl,foldr,map)

mkHint :: a -> String -> Result a
mkHint a s = hint a s nullRange
 
-- | Need to check if the analyser does his work well
deb s b = ("Debug : \n\n") ++ 
          ("Signature :" ++ (show s)) ++ 
          ("Spec :" ++ (show b))

msgs :: Map Int String
msgs = Map.insert 1 msg1 $ Map.insert 0 msg0 empty
        where msg0 = "Repeated nominals and/or modalities"
              msg1 = "Nominal not found"

genError = "Unspecific error found"

----------------

-- | Collects the newly declared nomies and modies 
colnomsMods :: [TH_BASIC_ITEM] -> ([MODALITY],[NOMINAL])
colnomsMods = foldr f ([],[]) 
        where   f (Simple_mod_decl ms _) = bimap (++ ms) id
                f (Simple_nom_decl ns _) = bimap id (++ ns)  

-- | Adds the newly declared nomies/modies to the signature
-- checking for redundancy

anaNomsMods :: [TH_BASIC_ITEM] -> Sign_Wrapper -> Result Sign_Wrapper
anaNomsMods ds (Sign_Wrapper s) = 
                if x' == x then return $ Sign_Wrapper s' 
                               else mkHint (Sign_Wrapper s') msg 
                where
                x = colnomsMods ds
                x' = bimap nub nub x 
                s' = s { modies = fst x', nomies = snd x' }  
                msg = fromMaybe genError $ Map.lookup 0 msgs 


anaForm :: Sign_Wrapper -> Form_Wrapper -> Result Form_Wrapper
anaForm (Sign_Wrapper s) (Form_Wrapper f) = 
       case f of 
                (At n _ _) -> if n `elem` (nomies s)
                        then return $ Form_Wrapper f else
                               mkError "" $ Form_Wrapper f
                _ -> return $ Form_Wrapper f  
        
-- | Kind of a lift of the formula analyser
anaForms :: Spec_Wrapper -> Sign_Wrapper -> Result [Named Form_Wrapper]
anaForms (Spec_Wrapper _ fs) s = 
        map ((makeNamed "") . (anaForm s)) fs  

--foldr :: (a -> b -> b) -> b -> [a] -> b
anaForms (Spec_Wrapper _ fs) s = 
        foldr ( make 

-- | Examining the list of formulas and collecting results 
thAna :: (Spec_Wrapper, Sign_Wrapper, GlobalAnnos) -> 
        Result (Spec_Wrapper, ExtSign Sign_Wrapper Symbol, [Named Form_Wrapper])

thAna (b@(Spec_Wrapper (Bspec ds _) _), s, _) = mkHint (b, mkExtSign s', fs) $ deb s' b  
        where   s' = fromMaybe s $ maybeResult $ anaNomsMods ds s
                fs = anaForms b s'




