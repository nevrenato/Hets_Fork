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

import TopHybrid.AS_TopHybrid
import TopHybrid.TopHybridSign
import TopHybrid.Print_AS
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
import Data.Maybe
import Data.Map as Map  hiding (foldl,foldr,map)
import Logic.Logic
import Unsafe.Coerce
import TopHybrid.UnderLogicList
import CASL.Logic_CASL
import ATerm.Lib

mkHint :: a -> String -> Result a
mkHint a s = hint a s nullRange

-- | Need to check if the analyser does his work well
deb :: (Show a, Show b, Show c) => a -> b -> c -> String
deb s b c = ("Debug : \n\n\n") ++ 
            ("Signature :" ++ (show s)) ++ 
            ("\n\n\nSpec :" ++ (show b)) ++
            ("\n\n\nForms :" ++ (show c)) ++ 
            ("\n\n\n")


msgs :: Map Int String
msgs = Map.insert 1 msg1 $ Map.insert 0 msg0 empty
        where msg0 = "Repeated nominals and/or modalities"
              msg1 = "Nominal not found"

genError :: String
genError = "Unspecific error found"

-- | Lifter of the mkNamed function 
liftName :: (Monad m) => m a -> m (Named a)
liftName = liftM $ makeNamed ""

-- | Tries to get a static analyser
maybeCall :: Maybe a -> a
maybeCall = fromMaybe (error "Shit")
-- change the error function

-- | End of auxiliar functions 

-- | Collects the newly declared nomies and modies 
colnomsMods :: [TH_BASIC_ITEM] -> ([MODALITY],[NOMINAL])
colnomsMods = foldr f ([],[]) 
        where   f (Simple_mod_decl ms _) = bimap (++ ms) id
                f (Simple_nom_decl ns _) = bimap id (++ ns)  

-- | Adds the newly declared nomies/modies to the signature
-- checking for redundancy
-- The nub function clears repeated elements from the list
anaNomsMods :: [TH_BASIC_ITEM] -> Sign_Wrapper -> Result Sign_Wrapper
anaNomsMods ds (Sign_Wrapper s) = 
                if x' == x then return $ Sign_Wrapper s' 
                               else mkHint (Sign_Wrapper s') msg 
                where
                x = colnomsMods ds
                x' = bimap nub nub x 
                s' = s { modies = fst x', nomies = snd x' }  
                msg = fromMaybe genError $ Map.lookup 0 msgs 

-- | Formula analyser
anaForm :: Sign_Wrapper -> Form_Wrapper -> Result Form_Wrapper
anaForm (Sign_Wrapper s) (Form_Wrapper f) = 
        case f of 
                (At n _ _) -> result msg $ elem n $ nomies s
                _ -> return $ Form_Wrapper f
        where   msg = fromMaybe genError $ Map.lookup 1 msgs  
                result _ True  = return $ Form_Wrapper f
                result m False = mkError m $ Form_Wrapper f
     
-- | Mapper and collector of the formula list
anaForms :: Sign_Wrapper -> [Form_Wrapper] -> Result [Named Form_Wrapper]
anaForms s = mapM (liftName . (anaForm s)) 

-- | Examining the list of formulas and collecting results 
-- fs' needs to be done in a butcher way, cuz we don't want 
-- the results to be collected this time
thAna :: (Spec_Wrapper, Sign_Wrapper, GlobalAnnos) -> 
        Result (Spec_Wrapper, ExtSign Sign_Wrapper Symbol, [Named Form_Wrapper])

thAna  (b@(Spec_Wrapper (Logic l) (Bspec ds e) fs), s@(Sign_Wrapper e'), g) = 
                                      (mkHint id (deb s' b fs')) `ap` f'        
         where                    
                s' = anaNomsMods ds s
                fs' = case s' of (Result _ x) -> anaForms (fromMaybe s x) fs 
                f = liftM2 (\x1 x2 -> (b,mkExtSign x1,x2)) s' fs'
                f' = liftM2 merger (xx l (tt l e)) f  

merger (x1,x2,x3) (y1,y2,y3) = (f x1 y1, g x2 y2, h x3 y3)
        where   f e (Spec_Wrapper l (Bspec ds _) fs) = Spec_Wrapper l (Bspec ds e) fs
                g (ExtSign s1 s2) (ExtSign s3 _) = mkExtSign (addExtension s3 s1)
                h xs fs = map (mapNamed (Form_Wrapper . UnderLogic)) xs ++ fs

tt :: (StaticAnalysis l bs sen si smi sign mor symb raw) => l -> a -> bs
tt _ = unsafeCoerce 

xx :: (StaticAnalysis l bs sen si smi sign mor symb raw) => l -> bs -> Result(bs,ExtSign sign symb,[Named sen])
xx l a = (maybeCall $ basic_analysis l) (a,empty_signature l,emptyGlobalAnnos)

------------------ 
-- Boring instances needed for a valid program, that DriFT cannot generate
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
