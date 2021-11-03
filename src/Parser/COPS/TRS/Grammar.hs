{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.TPDB.TRS.Grammar
-- Copyright   :  (c) muterm development team
-- License     :  see LICENSE
--
-- Maintainer  :  jiborra@dsic.upv.es
-- Stability   :  unstable
-- Portability :  non-portable
--
-- This module manage the grammar for TRSs in the TPDB
--
-----------------------------------------------------------------------------

module Parser.TPDB.TRS.Grammar (

-- * Exported data

Spec(..), Decl(..), Equation (..), SimpleRule (..)
, Rule(..), Term (..), AnyContent(..), Id
  , CondType (..), Strategy (..)

) where

import Data.Typeable
import Data.Generics

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

-- | Specification declaration
data Spec = Spec [Decl]                -- ^ List of declarations
            deriving (Eq, Show, Data, Typeable)

-- | List of declarations
data Decl = Var [Id]                    -- ^ Set of variables
   | Rules [Rule]                       -- ^ Set of rules
   | Strategy Strategy                  -- ^ Strategy
   | Any (Maybe String) [AnyContent]    -- ^ Extra information
   | CType CondType
   | CVar [Id]
     deriving (Eq, Show, Data, Typeable)

-- | Equation declaration
data Equation = Term :==: Term         -- ^ Equation
              |Term ::-> Term          -- ^ One-step
              |Term ::->* Term         -- ^ Zero or more steps
              |Term ::->+ Term         -- ^ One or more steps
              |Term :-><- Term        -- ^ Joinability
              |Term :<--> Term         -- ^ Semi-equational one
              |Term :<-->* Term        -- ^ Semi-equational
              |Term ::\-> Term         -- ^ One-step mu-rewriting
              |Term ::\->* Term        -- ^ Zero or more mu-rewriting steps
              |Term ::\->+ Term        -- ^ One or more mu-rewriting steps
              |Term :\-><-/ Term      -- ^ Mu-Joinability
              |Term :<-/\-> Term       -- ^ Semi-equational one mu-rewriting
              |Term :<-/\->* Term      -- ^ Semi-equational mu-rewriting
              |Term :|>= Term          -- ^ Subterm
              |Term :|> Term           -- ^ Strict subterm
              |Term ::->= Term         -- ^ One or zero steps
              |Term ::\->= Term         -- ^ One or zero mu-rewriting steps
              deriving (Eq, Show, Data, Typeable)

-- | Simple rule declaration
data SimpleRule = Term :-> Term         -- ^ Rewriting rule
   | Term :->= Term                     -- ^ Rewriting rule
     deriving (Eq, Show, Data, Typeable)

-- | Rule declaration
data Rule = Rule SimpleRule [Equation]      -- ^ Conditional rewriting rule
            deriving (Eq, Show, Data, Typeable)

-- | Term declaration
data Term = T Id [Term]                 -- ^ Term
            deriving (Eq, Show, Data, Typeable)

-- | Extra information
data AnyContent = AnyI Id               -- ^ Numeric
                | AnyS String           -- ^ String
                | AnyA [AnyContent]     -- ^ List
                  deriving (Eq, Show, Data, Typeable)

-- | Condition Type
data CondType =
  SemiEquational
  | Join
  | Oriented
  deriving (Eq, Show, Data, Typeable)

-- | Strategy information
data Strategy = InnerMost               -- ^ Innermost
              | OuterMost               -- ^ Outermost
              | Context [(Id, [Int])]   -- ^ Context-Sensitive
                deriving (Eq, Show, Data, Typeable)

-- | Identifier
type Id = String
