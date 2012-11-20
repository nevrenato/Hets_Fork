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
import Common.Id
import Control.Categorical.Bifunctor
import Control.Monad
import Data.List 
import Unsafe.Coerce
import ATerm.Lib

dump :: (Show a, Show b, Show c, Show x) => (a,b,c) -> x -> String
dump (a,b,c) x = ("Dump : \n") ++
                       ("Original spec : \n") ++ show a ++ ("\n ---- End of original spec dump ----\n") ++
                       ("Original sign : \n") ++ show b ++ ("\n ---- End of original sign dump ----\n") ++
                       ("Original forms : \n") ++ show c ++ ("\n ---- End of original forms dump ----\n") ++
                       ("\n\n ---- End of original dump ----\n\n") ++
                       ("Resulting dump : \n") ++ show x ++
                       ("\n\n ---- End of  dump ----\n\n")

-- | End of auxiliar functions 

-- | Collects the newly declared nomies and modies 
colnomsMods :: [TH_BASIC_ITEM] -> ([MODALITY],[NOMINAL])
colnomsMods = foldr f ([],[]) 
        where   f (Simple_mod_decl ms) = bimap (++ ms) id
                f (Simple_nom_decl ns) = bimap id (++ ns)  

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
anaForm :: AnyLogic -> Sign_Wrapper -> Form_Wrapper -> Result Form_Wrapper
anaForm l'@(Logic l) s'@(Sign_Wrapper s) (Form_Wrapper f) = 
        case f of 
                (At n f') -> (anaForm l' s' $ Form_Wrapper f') >>= (nomCheck s' n)
                (Here n) -> nomCheck' s' n f >>= (return . Form_Wrapper) 
                (UnderLogic f') -> (undFormAna l (extended s) f') >>= (return . Form_Wrapper . UnderLogic)
                _ -> ( return . Form_Wrapper )  f
  
-- Checks nominals existence
nomCheck :: Sign_Wrapper -> NOMINAL -> Form_Wrapper -> Result Form_Wrapper
nomCheck (Sign_Wrapper s) n (Form_Wrapper f) = if n `elem` nomies s then return ff else mkError msg ff
        where
        ff = Form_Wrapper $ At n f 
        msg = maybeE 1 Nothing 

-- Checks nominals existence for Here formulas, this later will be optimized in
-- order to only have one nominal check function
nomCheck' :: (GetRange f, Show f) => 
                        Sign_Wrapper -> NOMINAL -> (TH_FORMULA f) -> Result (TH_FORMULA f)
nomCheck' (Sign_Wrapper s) n  = if n `elem` nomies s then return else mkError msg 
        where
        msg = maybeE 1 Nothing

-- | Lift of the formula analyser
-- Analyses each formula and collects the results 
anaForms :: AnyLogic -> [Form_Wrapper] -> Sign_Wrapper -> Result [Named Form_Wrapper]
anaForms l f s = mapM ((liftName "") . (anaForm l s)) f 


-- | Examining the list of formulas and collecting results 
thAna :: (Spec_Wrapper, Sign_Wrapper, GlobalAnnos) -> 
        Result (Spec_Wrapper, ExtSign Sign_Wrapper Symbol, [Named Form_Wrapper])
thAna  (b@(Spec_Wrapper l'@(Logic l) sp fs), s, _) = -- (mkHint id (dump (s,b,fs) res')) `ap` 
        finalRes 
        where                   
        undA = undAna l $ und sp 
        partMerge = liftM (trimap f1 f2 f3) undA 
        s' = anaNomsMods (bitems sp) s 
        topAna = liftM2 (\x1 x2 -> (b,mkExtSign x1,x2)) s' (return []) 
        mergedRes = liftM2 (<***>) partMerge topAna 
        finalRes = mergedRes >>= \(x1,x2,x3) -> anaForms l' fs (plainSign x2) >>= return . (\x4 -> (x1,x2,x3++x4))
--        res' = finalRes >>= (\(a,ExtSign s _, f) -> return (a,s,f))     


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
unsafeToForm :: (StaticAnalysis l bs sen si smi sign mor symb raw) => l -> a -> sen
unsafeToForm _ = unsafeCoerce
unsafeToSig :: (StaticAnalysis l bs sen si smi sign mor symb raw) => l -> a -> sign
unsafeToSig _ = unsafeCoerce

-- Analysis of the underlogic
undAna :: (StaticAnalysis l bs sen si smi sign mor symb raw) => 
                l -> a -> Result(bs,ExtSign sign symb,[Named sen])
undAna l a = (maybeE 2 $ basic_analysis l) x 
        where x = (unsafeToSpec l a, empty_signature l, emptyGlobalAnnos)

undFormAna :: (StaticAnalysis l bs sen si smi sign mor symb raw ) =>
                l -> a -> b -> Result sen
undFormAna l a b = (maybeE 4 $ sen_analysis l) (a',b')
        where   a' = (unsafeToSig l a)
                b' = (unsafeToForm l b)


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
