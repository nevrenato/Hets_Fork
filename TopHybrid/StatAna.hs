{-# LANGUAGE ExistentialQuantification #-}
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

import Logic.Logic
import TopHybrid.AS_TopHybrid
import TopHybrid.TopHybridSign
import TopHybrid.Print_AS
import TopHybrid.Utilities
import TopHybrid.ATC_TopHybrid
import CASL.Sign -- Symbols
import Common.GlobalAnnotations
import Common.Result
import Common.ExtSign
import Common.AS_Annotation
import Control.Categorical.Bifunctor
import Control.Monad
import Data.List 
import Unsafe.Coerce
import ATerm.Lib

-- | Need to check if the analyser does his work well
deb :: (Show a, Show b, Show c) => a -> b -> c -> String
deb s b c = ("Debug : \n\n\n") ++ 
            ("Old Signature :" ++ (show s)) ++ 
            ("\n\n\nOld Spec :" ++ (show b)) ++
            ("\n\n\nOld Forms :" ++ (show c)) ++ 
            ("\n\n\n")

deb' :: (Show a, Show b, Show c) => a -> b -> c -> String
deb' s b c = ("Debug : \n\n\n") ++ 
            ("New Signature :" ++ (show s)) ++ 
            ("\n\n\nNew Spec :" ++ (show b)) ++
            ("\n\n\nNew Forms :" ++ (show c)) ++ 
            ("\n\n\n")

-- | End of auxiliar functions 

-- | Collects the newly declared nomies and modies 
colnomsMods :: [TH_BASIC_ITEM] -> ([MODALITY],[NOMINAL])
colnomsMods = foldr f ([],[]) 
        where   f (Simple_mod_decl ms _) = bimap (++ ms) id
                f (Simple_nom_decl ns _) = bimap id (++ ns)  

-- | Adds the newly declared nomies/modies to the signature
-- checking for redundancy
-- Note : The nub function removes repeated elements from the list
anaNomsMods :: [TH_BASIC_ITEM] -> Sign_Wrapper -> Result Sign_Wrapper
anaNomsMods ds (Sign_Wrapper s) = if x' == x then return $ Sign_Wrapper s' 
                                        else mkHint (Sign_Wrapper s') msg 
                where
                x = colnomsMods ds
                x' = bimap nub nub x 
                s' = s { modies = fst x', nomies = snd x' }  
                msg = maybeE 0 Nothing

-- | Formula analyser
anaForm :: Sign_Wrapper -> Form_Wrapper -> Result Form_Wrapper
anaForm (Sign_Wrapper s) (Form_Wrapper f) = 
        case f of 
                (At n _ _) -> fun n
                (Here n _ _) -> fun n
                _ -> return $ Form_Wrapper f
        where                   
        fun n = if (n `elem` nomies s) then return $ Form_Wrapper f
                        else mkError msg $ Form_Wrapper f    
        msg = maybeE 1 Nothing 

-- | Lift of the formula analyser
-- Analyses each formula and collects the results 
anaForms :: [Form_Wrapper] -> Sign_Wrapper -> Result [Named Form_Wrapper]
anaForms f s = mapM ((liftName "") . (anaForm s)) f 


-- | Examining the list of formulas and collecting results 
thAna :: (Spec_Wrapper, Sign_Wrapper, GlobalAnnos) -> 
        Result (Spec_Wrapper, ExtSign Sign_Wrapper Symbol, [Named Form_Wrapper])

thAna  (b@(Spec_Wrapper (Logic l) sp fs), s, _) = 
                        (mkHint id (deb' b'' a'' c'')) `ap`
                        (mkHint id (deb s' b fs')) `ap` 
                        mergedRes 
         where                   
                s' = anaNomsMods (bitems sp) s
                fs' = s' >>= (anaForms fs)
                topAna = liftM2 (\x1 x2 -> (b,mkExtSign x1,x2)) s' fs' 
                undA = undAna l $ und sp
                partMerge = liftM (trimap f1 f2 f3) undA
                mergedRes = liftM2 (<***>) partMerge topAna
                b'' = case mergedRes of Result _ (Just (_,ExtSign b' _,_)) -> b'
                a'' = case mergedRes of Result _ (Just (a,_,_)) -> a
                c'' = case mergedRes of Result _ (Just (_,_,c)) -> c

-- These functions merge the content from the top and under analysis 
f1 e (Spec_Wrapper l (Bspec ds _) fs) = Spec_Wrapper l (Bspec ds e) fs
f2 (ExtSign s3 _) (ExtSign s1 _) = mkExtSign (addExtension s3 s1)
f3 xs fs = map (mapNamed (Form_Wrapper . UnderLogic)) xs ++ fs

-- An unsafe function, that bypasses the typechecker, so that we can do
-- analysis to the underlogic, as the compiler can't know if we are doing
-- analysis to the right correspondent spec ( l -> bs ), since we don't
-- restrict that correspondence in the top spec, Spec_Wrapper.
unsafeToSpec :: (StaticAnalysis l bs sen si smi sign mor symb raw) => l -> a -> bs
unsafeToSpec _ = unsafeCoerce 

-- Analysis of the underlogic
undAna :: (StaticAnalysis l bs sen si smi sign mor symb raw) => 
                l -> a -> Result(bs,ExtSign sign symb,[Named sen])
undAna l a = (maybeE 2 $ basic_analysis l) x 
        where x = (unsafeToSpec l a, empty_signature l, emptyGlobalAnnos)

-- These instances should be automatically generated by DriFT, but it cannot
-- since they are not declared in a usual format 

instance  ShATermConvertible Sign_Wrapper where
         toShATermAux att (Sign_Wrapper s) = toShATermAux att s
--         fromShATermAux a b = mapSnd Sign_Wrapper $ fromShATermAux a b
--                 where mapSnd f (a,b) = (a, f b)
         fromShATermAux _ _= error "I entered here"

instance ShATermConvertible Spec_Wrapper where
         toShATermAux att (Spec_Wrapper _ s _) = toShATermAux att s 
--         fromShATermAux a b = mapSnd Spec_Wrapper $ fromShATermAux a b
--                 where mapSnd f (x,y) = (x, f y)
         fromShATermAux _ _ = error "I entered here"

instance ShATermConvertible Form_Wrapper where
        toShATermAux att (Form_Wrapper f) = toShATermAux att f
        fromShATermAux _ _ = error "I entered here"              
