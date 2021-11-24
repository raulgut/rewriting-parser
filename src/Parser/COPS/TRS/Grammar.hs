{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.COPS.TRS.Grammar
-- Copyright   :  (c) muterm development team
-- License     :  see LICENSE
--
-- Maintainer  :  r.gutierrez@upm.es
-- Stability   :  unstable
-- Portability :  non-portable
--
-- This module manage the grammar for TRSs in COPS
--
-----------------------------------------------------------------------------

module Parser.COPS.TRS.Grammar (

-- * Exported data

Spec(..), Decl(..), Equation (..), SimpleRule (..)
, Rule(..), Term (..), Id, CondType (..), TRSType (..)
, TRS (..)

-- * Exported functions

, getTerms, nonVarLHS

) where

import Data.Typeable
import Data.Generics
import Data.Map (Map)
import Data.Set (Set, member)
import Data.List (intersperse)

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

-- | Specification declaration
data Spec = Spec [Decl] -- ^ List of declarations
            deriving (Eq, Show, Data, Typeable)

-- | List of declarations
data Decl = CType CondType -- ^ Type of conditional rules
   | Var [Id] -- ^ Set of variables
--   | Signature [(Id,Int)] -- ^ Extended signature
   | Rules [Rule] -- ^ Set of rules
   | Context [(Id, [Int])] -- ^ Context-Sensitive strategy
   | Comment String -- ^ Extra information
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

-- | Condition Type
data CondType =
  SemiEquational
  | Join
  | Oriented
  deriving (Eq, Show, Data, Typeable)

-- | Identifier
type Id = String

-- | TSR Type
data TRSType = TRSStandard
  | TRSConditional CondType
  | TRSContextSensitive 
  | TRSContextSensitiveConditional CondType
  deriving (Show)

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
getTerms :: Rule -> [Term]
getTerms (Rule (l :-> r) eqs) = (l:r:concatMap getTermsEq eqs)

-- | gets all the terms from a equation
getTermsEq :: Equation -> [Term]
getTermsEq (l :==: r) = [l,r]

-- | checks if the lhs is non-variable
nonVarLHS :: Rule -> Set Id -> Bool
nonVarLHS (Rule ((T idt _) :-> r) eqs) vs = not . member idt $ vs 
