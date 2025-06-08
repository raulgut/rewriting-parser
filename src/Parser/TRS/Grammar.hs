{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.TRS.Grammar
-- Copyright   :  (c) muterm development team
-- License     :  see LICENSE
--
-- Maintainer  :  r.gutierrez@upm.es
-- Stability   :  unstable
-- Portability :  non-portable
--
-- This module manage the grammar for TRSs
--
-----------------------------------------------------------------------------

module Parser.TRS.Grammar (

-- * Exported data

Spec(..), Decl(..), Condition (..), SimpleRule (..)
, Rule(..), Term (..), TId, CondType (..), TRSType (..)
, TRS (..), ProbType (..), INF (..), SimpleEquation (..)
, Equation (..), SimplePredicate (..), Predicate (..)
, Formula (..)

-- * Exported functions

, getTerms, getVars, getVars', lhs, rhs, getMSVars', conds
, eqlhs, eqrhs, matches, getTermsEq, getEqTerms, getPredTerms
, getFormulaTerms

) where

import Data.Typeable
import Data.Generics
import Data.Map as M (Map, lookup, insertWith, empty, insert, notMember)
import Data.Set as S (Set, member, unions, insert, (\\), null, union, fromList
 , singleton)
import Data.MultiSet as MS (MultiSet, unions, insert)
import Data.List (intersperse)
import Data.Maybe (isNothing, isJust)
import Control.Monad (zipWithM_)
import Control.Monad.State (State, evalState, execState, get, put)

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

-- | Specification declaration
data Spec = Spec [Decl] -- ^ List of declarations
            deriving (Eq, Show, Data, Typeable)

-- | List of declarations
data Decl = CType CondType -- ^ Type of conditional rules
   -- Standard declarations
   | Var [TId] -- ^ Set of variables
   | Signature [(TId,Int)] -- ^ Extended signature
   | Rules [Rule] -- ^ Set of rules
   | Context [(TId, [Int])] -- ^ Context-Sensitive strategy
   | Comment String -- ^ Extra information
   | Format TRSType
   -- Extended declarations for INF
   | Problem ProbType -- ^ Kind of problem
   | CVar [TId] -- ^ Set of condition variables
   | Conditions [[Condition]]
   -- Extended declarations for GTRSs/Maude Theories in infChecker
   | Equations [Equation] -- ^ Set of equations
   | Predicates [Predicate] -- ^ Set of equations
   | FOTheory Formula -- ^ FO formula
     deriving (Eq, Show, Data, Typeable)

-- | Condition declaration
data Condition = Term :==: Term         -- ^ Equation
               -- Extended declaration
               |Term ::-> Term          -- ^ One-step
               |Term ::->* Term         -- ^ Zero or more steps
               |Term ::->= Term         -- ^ Zero or one steps
               |Term ::->+ Term         -- ^ One or more steps
               |Term :-><- Term        -- ^ Joinability
               |Term :<--> Term         -- ^ Semi-equational one
               |Term :<-->* Term        -- ^ Semi-equational
               |Term ::\-> Term         -- ^ One-step mu-rewriting
               |Term ::\->* Term        -- ^ Zero or more mu-rewriting steps
               |Term ::\->= Term        -- ^ Zero or on mu-rewriting steps
               |Term ::\->+ Term        -- ^ One or more mu-rewriting steps
               |Term :\-><-/ Term      -- ^ Mu-Joinability
               |Term :<-/\-> Term       -- ^ Semi-equational one mu-rewriting
               |Term :<-/\->* Term      -- ^ Semi-equational mu-rewriting
               |Term :|>= Term          -- ^ Subterm
               |Term :|> Term           -- ^ Strict subterm
               | Term :=: Term          -- ^ Equational condition
               | PrAC TId [Term]         -- ^ Predicate Atom condition
               deriving (Eq, Data, Typeable)

-- | Formula declaration
data Formula = Formula Term
             deriving (Eq, Data, Typeable)

-- | Simple rule declaration
data SimpleRule = Term :-> Term -- ^ Rewriting rule
     deriving (Eq, Data, Typeable)

-- | Rule declaration
data Rule = Rule SimpleRule [Condition] -- ^ Conditional rewriting rule
            deriving (Eq, Data, Typeable)

-- | Simple equation declaration
data SimpleEquation = Term := Term -- ^ Equation
     deriving (Eq, Data, Typeable)

-- | Equation declaration
data Equation = Equation SimpleEquation [Condition] -- ^ Conditional Equation
            deriving (Eq, Data, Typeable)

-- | Simple Horn Clause declaration
data SimplePredicate = PrA TId [Term] -- ^ Predicate atom
     deriving (Eq, Data, Typeable)

-- | Horn Clause declaration
data Predicate = Predicate SimplePredicate [Condition] -- ^ Horn clause
            deriving (Eq, Data, Typeable)

-- | Term declaration
data Term = T TId [Term] -- ^ Term
            deriving (Eq, Ord, Data, Typeable)

-- | Condition Type
data CondType =
  SemiEquational
  | Join
  | Oriented  
  deriving (Eq, Show, Data, Typeable)

-- | Term Identifier
type TId = String

-- | TSR Type
data TRSType = TRSStandard
  | TRSConditional CondType
  | TRSContextSensitive 
  | TRSContextSensitiveConditional CondType
  deriving (Show, Eq, Data)

-- | Term Rewriting Systems (TRS, CTRS, CSTRS, CSCTRS)
data TRS 
  = TRS { trsSignature :: Map TId Int
        , trsVariables :: Set TId
        , trsRMap :: [(TId, [Int])]
        , trsRules :: [Rule]
        , trsEquations :: [Equation]
        , trsPredicates :: [Predicate]
        , trsFOTheory :: Maybe Formula
        , trsType :: TRSType
        } deriving (Show)

-- | Infeasibility condition
data INF 
  = INF { trsCondVariables :: Set TId
        , trsConditions :: [[Condition]]
        } deriving (Show)

-- | Problem type
data ProbType =
  INFEASIBILITY
  deriving (Eq, Show, Data, Typeable)

-----------------------------------------------------------------------------
-- Instances
-----------------------------------------------------------------------------

-- Show

instance Show Condition where
  show (t1 :==: t2) = show t1 ++ " == " ++ show t2
  show (t1 ::-> t2) = show t1 ++ " -> " ++ show t2
  show (t1 ::->* t2) = show t1 ++ " ->* " ++ show t2
  show (t1 ::->= t2) = show t1 ++ " ->= " ++ show t2
  show (t1 ::->+ t2) = show t1 ++ " ->+ " ++ show t2
  show (t1 :-><- t2) = show t1 ++ " -><- " ++ show t2
  show (t1 :<--> t2) = show t1 ++ " <--> " ++ show t2
  show (t1 :<-->* t2) = show t1 ++ " <-->* " ++ show t2
  show (t1 ::\-> t2) = show t1 ++ " \\-> " ++ show t2
  show (t1 ::\->* t2) = show t1 ++ " \\->* " ++ show t2
  show (t1 ::\->= t2) = show t1 ++ " \\->= " ++ show t2
  show (t1 ::\->+ t2) = show t1 ++ " \\->+ " ++ show t2
  show (t1 :\-><-/ t2) = show t1 ++ " \\-><-/ " ++ show t2
  show (t1 :<-/\-> t2) = show t1 ++ " <-/\\-> " ++ show t2
  show (t1 :<-/\->* t2) = show t1 ++ " <-/\\->* " ++ show t2
  show (t1 :|>= t2) = show t1 ++ " |>= " ++ show t2
  show (t1 :|> t2) = show t1 ++ " |> " ++ show t2
  show (t1 :=: t2) = show t1 ++ " = " ++ show t2
  show (PrAC f ts) = show (T f ts)

instance Show SimpleRule where 
  show (t1 :-> t2) = show t1 ++ " -> " ++ show t2

instance Show Rule where 
  show (Rule r []) = show r
  show (Rule r eqs) = show r ++ " | " ++ (concat . intersperse ", " . map show $ eqs)

instance Show SimpleEquation where 
  show (t1 := t2) = show t1 ++ " = " ++ show t2

instance Show Equation where 
  show (Equation eq []) = show eq
  show (Equation eq eqs) = show eq ++ " | " ++ (concat . intersperse ", " . map show $ eqs)

instance Show SimplePredicate where 
  show (PrA f ts) = show (T f ts)

instance Show Predicate where 
  show (Predicate p []) = show p
  show (Predicate p eqs) = show p ++ " | " ++ (concat . intersperse ", " . map show $ eqs)

instance Show Formula where 
  show (Formula p) = show p

instance Show Term where
    show (T f []) = f 
    show (T f terms) = f ++ "(" ++ (concat . intersperse "," . map show $ terms) ++ ")" 

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | gets all the different vars from a term
getVars :: Set TId -> Term -> Set TId
getVars vs (T idt ts) = let tsVars = S.unions . map (getVars vs) $ ts
                        in if member idt vs then
                             S.insert idt tsVars 
                           else 
                             tsVars

-- | gets all the different vars from a term
getVars' :: Map TId Int -> Term -> Set TId
getVars' fs (T idt ts) = let tsVars = S.unions . map (getVars' fs) $ ts
                        in if isNothing $ M.lookup idt fs then
                             S.insert idt tsVars 
                           else 
                             tsVars

-- | gets all the terms from a rule
getTerms :: Rule -> [Term]
getTerms (Rule (l :-> r) eqs) = (l:r:concatMap getTermsEq eqs)

-- | gets all the terms from a equation
getTermsEq :: Condition -> [Term]
getTermsEq (l :==: r) = [l,r]
getTermsEq (l ::-> r) = [l,r]
getTermsEq (l ::->* r) = [l,r]
getTermsEq (l ::->= r) = [l,r]
getTermsEq (l ::->+ r) = [l,r]
getTermsEq (l :-><- r) = [l,r]
getTermsEq (l :<--> r) = [l,r]
getTermsEq (l :<-->* r) = [l,r]
getTermsEq (l ::\-> r) = [l,r]
getTermsEq (l ::\->* r) = [l,r]
getTermsEq (l ::\->= r) = [l,r]
getTermsEq (l ::\->+ r) = [l,r]
getTermsEq (l :\-><-/ r) = [l,r]
getTermsEq (l :<-/\-> r) = [l,r]
getTermsEq (l :<-/\->* r) = [l,r]
getTermsEq (l :|>= r) = [l,r]
getTermsEq (l :|> r) = [l,r]
getTermsEq (l :=: r) = [l,r]
getTermsEq (PrAC f ts) = [T f ts]

-- | gets all the terms from a equation
getEqTerms :: Equation -> [Term]
getEqTerms (Equation (l := r) eqs) = (l:r:concatMap getTermsEq eqs)

-- | gets all the terms from a predicate
getPredTerms :: Predicate -> [Term]
getPredTerms (Predicate (PrA f terms) eqs) = ((T f terms):concatMap getTermsEq eqs)

-- | gets all the terms from a formula
getFormulaTerms :: Formula -> [Term]
getFormulaTerms (Formula t) = [t]

-- | get the left-hand side of a rule
lhs :: Rule -> Term
lhs (Rule (l :-> _) _) = l

-- | get the right-hand side of a rule
rhs :: Rule -> Term
rhs (Rule (_ :-> r) _) = r

-- | get the equations a rule
conds :: Rule -> [Condition]
conds (Rule _ eqs) = eqs

-- | get the left-hand side of a equation
eqlhs :: Condition -> Term
eqlhs (l :==: _) = l

-- | get the right-hand side of a equation
eqrhs :: Condition -> Term
eqrhs (_ :==: r) = r

-- | gets all the vars from a term
getMSVars' :: Map TId Int -> Term -> MultiSet TId
getMSVars' fs (T idt ts) = let tsVars = MS.unions . map (getMSVars' fs) $ ts
                           in if isNothing $ M.lookup idt fs then
                                MS.insert idt tsVars 
                              else 
                                tsVars

-- | Get all subterms
subterms :: Term -> Set Term
subterms t@(T _ ts)
    = foldr S.union (singleton t) (map subterms ts)

-- Matching

-- | If two terms match, returns its substitution in the
-- monad. Variables in terms must be disjoint
match :: TRS -> Term -> Term -> State (Maybe (Map TId Term)) ()
match trs = matchF 
  where
    sig = trsSignature trs
    matchF t s 
      = do t' <- findT trs t
           case (t', s) of
             (T x _,T y _) | notMember x sig && 
                             notMember y sig && 
                             x == y -> return ()
             (T x _, u) | notMember x sig 
               -> case (t, t') of
                    (T x' _, T y' _ ) | notMember x' sig && 
                                        notMember y' sig && 
                                        x' == y' -> do subst <- get
                                                       case subst of 
                                                        Just subst' -> do let newSubst = M.insert x u subst'
                                                                          put . Just $ newSubst
                                                        Nothing -> return ()
                    _ -> put Nothing -- Structure mismatch
             (u, v) -> zipTermM_ matchF u v
    zipTermM_ f (T f1 tt1) (T f2 tt2) | f1 == f2 = zipWithM_ f tt1 tt2
    zipTermM_ _ _ _ = put Nothing -- Structure mismatch

-- | We construct the most general unifier binding variables
findT :: TRS -> Term -> State (Maybe (Map TId Term)) Term
findT trs t0@(T v _) | notMember v sig = go v
    where
      sig = trsSignature trs
      go x = do subst <- get 
                case subst of 
                  Just subst' -> case M.lookup x subst' of
                                   Just (T x' _) | notMember x' sig -> go x'
                                   Just t -> do let newSubst = M.insert v t subst'
                                                put . Just $ newSubst
                                                return t
                                   Nothing -> return t0
findT _ t0 = return t0

-- | Returns if two terms matching. Variables in terms must be disjoint
matches :: TRS -> Term -> Term -> Bool
matches trs t u = isJust (execState (match trs t u) (Just M.empty))

-- | Returns if two terms match and returs the matching substitution. Variables
-- in terms must be disjoint
matchSubstitution :: TRS -> Term -> Term -> Maybe (Map TId Term)
matchSubstitution trs t u = execState (match trs t u) (Just M.empty)
