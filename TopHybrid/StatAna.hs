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
import Common.DocUtils
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

-- | Top Formula analyser
anaForm :: (StaticAnalysis l bs sen si smi sign mor symb raw) =>  
        l -> bs -> Sign_Wrapper -> Form_Wrapper -> Result Form_Wrapper
anaForm l bs s (Form_Wrapper f) = (anaForm' l bs s f) >>= return . Form_Wrapper

anaForm' :: (Show f, GetRange f, ShATermConvertible f, 
             StaticAnalysis l bs sen sy sm si mo sy' rs) => 
                l -> bs -> Sign_Wrapper -> (TH_FORMULA f) -> Result (TH_FORMULA sen)
anaForm' l bs s'@(Sign_Wrapper s) f = 
        case f of  
                  (At n f') -> (anaForm' l bs s' f') >>= return . (At n) >>= nomOrModCheck (nomies s) n
                  (Box m f') -> (anaForm' l bs s' f') >>= return . (Box m) >>= nomOrModCheck (modies s) m
                  (Dia m f') -> (anaForm' l bs s' f') >>= return . (Dia m) >>= nomOrModCheck (modies s) m
                  (Conjunction f' f'') -> (liftM2 Conjunction) (anaForm' l bs s' f') (anaForm' l bs s' f'')
                  (Disjunction f' f'') -> (liftM2 Disjunction) (anaForm' l bs s' f') (anaForm' l bs s' f'')
                  (Implication f' f'') -> (liftM2 Implication) (anaForm' l bs s' f') (anaForm' l bs s' f'')
                  (BiImplication f' f'') -> (liftM2 BiImplication) (anaForm' l bs s' f') (anaForm' l bs s' f'')
                  (Here n) -> nomOrModCheck (nomies s) n $ Here n 
                  (Neg f') -> (liftM Neg) (anaForm' l bs s' f')
                  (UnderLogic f') -> (undFormAna l (extended s) f' bs) >>= (return . UnderLogic)
                  (Par f') -> (liftM Par) (anaForm' l bs s' f')

-- Checks for nominals and modalities
nomOrModCheck :: (Pretty f, GetRange f, ShATermConvertible f) => 
                [SIMPLE_ID] -> SIMPLE_ID -> (TH_FORMULA f) -> Result (TH_FORMULA f)
nomOrModCheck xs x = if x `elem` xs  then return else mkError msg
     where msg = maybeE 1 Nothing 
  
-- | Lift of the formula analyser
-- Analyses each formula and collects the results 
anaForms :: (StaticAnalysis l bs sen si smi sign mor symb raw) =>  
        l -> bs -> [Form_Wrapper] -> Sign_Wrapper -> Result [Named Form_Wrapper]
anaForms l bs f s = mapM ((liftName "") . (anaForm l bs s)) f 


-- | Examining the list of formulas and collecting results 
thAna :: (Spec_Wrapper, Sign_Wrapper, GlobalAnnos) -> 
        Result (Spec_Wrapper, ExtSign Sign_Wrapper Symbol, [Named Form_Wrapper])
thAna  (b@(Spec_Wrapper (Logic l) sp fs), s, _) = finalRes 
        where                   
        undA = undAna l $ und sp 
        (Result _ (Just undAf)) = undA >>= return . (\(x1,x2,x3) -> x1) -- This needs to be changed
        partMerge = liftM (trimap f1 f2 f3) undA 
        s' = anaNomsMods (bitems sp) s 
        topAna = liftM2 (\x1 x2 -> (b,mkExtSign x1,x2)) s' (return []) 
        mergedRes = liftM2 (<***>) partMerge topAna 
        finalRes = mergedRes >>= \(x1,x2,x3) -> anaForms l (undAf) fs (plainSign x2) >>= return . (\x4 -> (x1,x2,x3++x4)) 


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
                l -> a -> b -> bs -> Result sen
undFormAna l a b c = (maybeE 4 $ sen_analysis l) (c, a',b')
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
