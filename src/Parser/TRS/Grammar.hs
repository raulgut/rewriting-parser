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

Spec(..), Decl(..), Equation (..), SimpleRule (..)
, Rule(..), Term (..), Id, CondType (..), TRSType (..)
, TRS (..), RuleType (..)

-- * Exported functions

, getTerms, nonVarLHS, nonVarLHS', isCRule, hasExtraVars, hasExtraVars'
, isCanonical, isTRSConditional

) where

import Data.Typeable
import Data.Generics
import Data.Map as M (Map, lookup, insertWith, empty)
import Data.Set as S (Set, member, unions, insert, (\\), null, union, fromList)
import Data.List (intersperse)
import Data.Maybe (isNothing)
import Control.Monad.State (MonadState (..), execState)

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

-- | Specification declaration
data Spec = Spec [Decl] -- ^ List of declarations
            deriving (Eq, Show, Data, Typeable)

-- | List of declarations
data Decl = CType CondType -- ^ Type of conditional rules
   | Var [Id] -- ^ Set of variables
   | Signature [(Id,Int)] -- ^ Extended signature
   | Rules [Rule] -- ^ Set of rules
   | Context [(Id, [Int])] -- ^ Context-Sensitive strategy
   | Comment String -- ^ Extra information
   | Format TRSType
     deriving (Eq, Show, Data, Typeable)

-- | Equation declaration
data Equation = Term :==: Term -- ^ Equation
              deriving (Eq, Data, Typeable)

-- | Simple rule declaration
data SimpleRule = Term :-> Term -- ^ Rewriting rule
     deriving (Eq, Data, Typeable)

-- | Rule declaration
data Rule = Rule SimpleRule [Equation] -- ^ Conditional rewriting rule
            deriving (Eq, Data, Typeable)

-- | Term declaration
data Term = T Id [Term] -- ^ Term
            deriving (Eq, Data, Typeable)

-- | TRS Type
data RuleType =
  Standard
  | SRS
  | LeftLinear
  | RightGround
  | Ground
  deriving (Eq, Show, Data, Typeable)

-- | Condition Type
data CondType =
  SemiEquational
  | Join
  | Oriented  
  deriving (Eq, Show, Data, Typeable)

-- | Identifier
type Id = String

-- | TSR Type
data TRSType = TRSStandard RuleType
  | TRSConditional CondType
  | TRSContextSensitive 
  | TRSContextSensitiveConditional CondType
  deriving (Show, Eq, Data)

-- | Term Rewriting Systems (TRS, CTRS, CSTRS, CSCTRS)
data TRS 
  = TRS { trsSignature :: Map Id Int
        , trsVariables :: Set Id
        , trsRMap :: [(Id, [Int])]
        , trsRules :: [Rule]
        , trsType :: TRSType
        } deriving (Show)

-----------------------------------------------------------------------------
-- Instances
-----------------------------------------------------------------------------

-- Show

instance Show Equation where
  show (t1 :==: t2) = show t1 ++ " == " ++ show t2

instance Show SimpleRule where 
  show (t1 :-> t2) = show t1 ++ " -> " ++ show t2

instance Show Rule where 
  show (Rule r []) = show r
  show (Rule r eqs) = show r ++ " | " ++ (concat . intersperse ", " . map show $ eqs)

instance Show Term where
    show (T f []) = f 
    show (T f terms) = f ++ "(" ++ (concat . intersperse "," . map show $ terms) ++ ")" 

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | gets all the terms from a rule
getVars :: Set Id -> Term -> Set Id
getVars vs (T idt ts) = let tsVars = unions . map (getVars vs) $ ts
                        in if member idt vs then
                             insert idt tsVars 
                           else 
                             tsVars

-- | gets all the terms from a rule
getVars' :: Map Id Int -> Term -> Set Id
getVars' fs (T idt ts) = let tsVars = unions . map (getVars' fs) $ ts
                        in if isNothing $ M.lookup idt fs then
                             insert idt tsVars 
                           else 
                             tsVars

-- | gets all the terms from a rule
getTerms :: Rule -> [Term]
getTerms (Rule (l :-> r) eqs) = (l:r:concatMap getTermsEq eqs)

-- | gets all the terms from a equation
getTermsEq :: Equation -> [Term]
getTermsEq (l :==: r) = [l,r]

-- | checks if the lhs is non-variable
nonVarLHS :: Set Id -> Rule -> Bool
nonVarLHS vs (Rule ((T idt _) :-> r) eqs) = not . member idt $ vs 

-- | checks if the lhs is non-variable
nonVarLHS' :: Map Id Int -> Rule -> Bool
nonVarLHS' fs (Rule ((T idt _) :-> r) eqs) = not . isNothing . M.lookup idt $ fs
                                                

-- | checks if the rule is conditional
isCRule :: Rule -> Bool
isCRule (Rule _ []) = False 
isCRule _ = True 

-- | checks if the non-conditional rule has extra variables
hasExtraVars :: Set Id -> Rule -> Bool
hasExtraVars vs (Rule (l :-> r) []) = not . S.null $ getVars vs r \\ getVars vs l
hasExtraVars _ _ = error $ "Error: hasExtraVars only applies to non-conditional rules"

-- | checks if the non-conditional rule has extra variables
hasExtraVars' :: Map Id Int -> Rule -> Bool
hasExtraVars' fs (Rule (l :-> r) []) = not . S.null $ getVars' fs r \\ getVars' fs l
hasExtraVars' _ _ = error $ "Error: hasExtraVars only applies to non-conditional rules"

-- | Extract canonical replacement map
extractCanonicalRepMap :: (MonadState (Map Id (Set Int)) m) => TRS -> m ()
extractCanonicalRepMap trs = do a <- sequence . Prelude.map (extractCanonicalRepMapRule trs) . trsRules $ trs
                                return ()

-- | Extract canonical replacement map from rule
extractCanonicalRepMapRule :: (MonadState (Map Id (Set Int)) m) => TRS -> Rule -> m ()
extractCanonicalRepMapRule trs (Rule (l :-> _) _) = extractCanonicalRepMapTerm trs l

-- | Extract canonical replacement map from term
extractCanonicalRepMapTerm :: (MonadState (Map Id (Set Int)) m) => TRS -> Term -> m ()
extractCanonicalRepMapTerm trs (T f tt) 
  = do a <- sequence . map (extractCanonicalRepMapTerm trs) . filter (not . isVTerm) $ tt
       mapping <- get
       let mapping' = M.insertWith (S.union) f compf' mapping
       put mapping'
       return ()
    where
      sig = trsSignature trs
      arity = case M.lookup f sig of 
                Just ar -> ar 
                Nothing -> error $ "Symbol " ++ f ++ " does not appear in the Signature.\n" 
      isVTerm (T v _) = isNothing $ M.lookup v sig  
      compf' = S.fromList [i | i <- [1..arity], not . isVTerm $ (tt!!(i - 1))]

-- | Checks if the replacement map of the TRS is canonical
isCanonical :: TRS -> Bool
isCanonical trs =
  let repmap = execState (extractCanonicalRepMap trs) (M.empty)
  in and . map (checkCanonical trs repmap) . trsRMap $ trs

-- | Checks if the replamcement map of the symbol is canonical
checkCanonical :: TRS -> Map Id (Set Int) -> (Id,[Int]) -> Bool
checkCanonical trs rMap (f,rMapL) 
  = case M.lookup f rMap of
      Nothing -> let ar = trsSignature trs
                 in case M.lookup f ar of 
                      Nothing -> error $ "Symbol " ++ f ++ " does not appear in the Signature.\n"
                      Just arity -> (S.fromList [1..arity]) == (S.fromList rMapL)
      Just rMapS -> rMapS == (S.fromList rMapL)

-- | isTRSConditional checks if trsType is Conditional
isTRSConditional :: TRS -> Bool
isTRSConditional trs = case trsType trs of 
                         TRSConditional _ -> True 
                         TRSContextSensitiveConditional _ -> True
                         _ -> False