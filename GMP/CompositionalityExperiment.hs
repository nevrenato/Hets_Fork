{-# OPTIONS -fglasgow-exts #-}
{- |
Module      :  $EmptyHeader$
Description :  <optional short description entry>
Copyright   :  (c) <Authors or Affiliations>
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<optional description>
-}
module Comp where

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map

data Boole a = F | T | And (Boole a) (Boole a) | Or (Boole a) (Boole a) |
              Not (Boole a) | At a deriving (Eq, Ord, Show)

data K l = K (Boole l) deriving (Eq, Ord, Show)

data RK = RK Int deriving Show

data KD l  = KD (Boole l) deriving (Eq, Ord, Show)

data RKD = RKDPos Int | RKDNeg Int deriving Show

data Segala a = S (Boole (KD (Boole (K (Segala a))))) deriving (Eq, Show)

data Clause a = Implies (Set.Set a) (Set.Set a) deriving (Eq, Ord, Show)

data Subst a = Subst (Map.Map Int a) deriving (Eq, Show)

class Logic a b | a -> b, b -> a where
  match :: Clause (a c) -> [(b, Subst (Boole c))]
  clauses :: b -> [Clause Int]
  subclauses :: Ord c => Clause (a c) -> Set.Set (Clause (a c))

instance Logic K RK where
  match ((Implies n p)::Clause (K c)) =
    let i = Set.size n
        strip (K x) = x
        substHead = (i+1,strip(head(Set.elems p)))
        substTail = zip [1..i] (map strip (Set.elems n))
    in [(RK i, Subst (Map.fromList (substHead:substTail)))]
  clauses (RK n) = [Implies (Set.fromList [1..n]) (Set.singleton (n+1))]
  subclauses (Implies n p) =
    Set.fromList [Implies n (Set.singleton l) | l <- Set.elems p]

instance Logic KD RKD where
  match ((Implies n p):: Clause (KD c)) =
    let i = Set.size n
        strip (KD x) = x
        substHead = (i+1,strip(head(Set.elems p)))
        substTail = zip [1..i] (map strip (Set.elems n))
    in [(RKDPos i, Subst (Map.fromList (substHead:substTail)))]
  clauses (RKDPos n) = [Implies (Set.fromList [1..n]) (Set.singleton (n+1))]
  clauses (RKDNeg n) = [Implies (Set.fromList [1..n]) (Set.singleton (n+1))]
  subclauses (Implies n p) =
    Set.fromList [Implies n (Set.singleton l) | l <- Set.elems p]
