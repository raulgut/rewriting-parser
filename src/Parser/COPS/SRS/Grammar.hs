{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.TPDB.SRS.Grammar
-- Copyright   :  (c) muterm development team
-- License     :  see LICENSE
-- 
-- Maintainer  :  jiborra@dsic.upv.es
-- Stability   :  unstable
-- Portability :  non-portable 
--
-- This module manage the grammar for SRSs in the TPDB
--
-----------------------------------------------------------------------------

module Parser.TPDB.SRS.Grammar (

-- * Exported data

Spec(..), Decl(..), Rule(..), StratDecl(..), AnyContent(..), Id

) where

import Text.PrettyPrint (Doc, parens, sep, punctuate, comma, text, fsep, (<+>), vcat, ptext)
import Data.Maybe (Maybe, maybe)

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------
 
-- | Specification declaration
data Spec = Spec [Decl]                -- ^ List of declarations
            deriving (Eq, Show)

-- | List of declarations
data Decl = Rules [Rule]                    -- ^ Set of rules
          | Strat StratDecl                 -- ^ Strategy
          | Any (Maybe String) [AnyContent] -- ^ Extra information
          | Pairs [Rule]                    -- ^ Set of pairs (extension not 
                                            --   in TPDB)
          | UnhRules [Rule]                 -- ^ Set of unhiding rules
                                            --   (extension not in
                                            --   TPDB)
            deriving (Eq, Show)
 
-- | Rule declaration
data Rule = Rule :-> Rule              -- ^ Rewriting rule 
          | Rule :->= Rule             -- ^ Rewriting rule
          | Single [Id]                -- ^ Single
            deriving (Eq, Show)
 
-- | Strategy information
data StratDecl = LeftMost              -- ^ Leftmost
               | RightMost             -- ^ Rightmost
                 deriving (Eq, Show)
 
-- | Extra information
data AnyContent = AnyI Id              -- ^ Numeric
                | AnyS String          -- ^ String
                | AnyA [AnyContent]    -- ^ List
                  deriving (Eq, Show)
 
-- | Identifier
type Id = String

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------
 
-- | Pretty Print specification 
pprintSpec :: Spec -> Doc
pprintSpec (Spec dd) = vcat (map (parens.pprintDecl) dd)

-- | Pretty Print declaration
pprintDecl :: Decl -> Doc
pprintDecl (Rules rr) = text "RULES" <+> (sep$ punctuate comma (map pprintRule rr))
pprintDecl (Strat x) = text "STRATEGY" <+> text (show x)
pprintDecl (Any name cc) = text (maybe "" id name) <+> fsep (map pprintAnyC cc)
pprintDecl (Pairs rr) = text "PAIRS" <+> (sep$ punctuate comma (map pprintRule rr))
pprintDecl (UnhRules rr) = text "UNHRULES" <+> (sep$ punctuate comma (map pprintRule rr))

-- | Pretty Print rule
pprintRule :: Rule -> Doc
pprintRule (r1 :-> r2) = pprintRule r1 <+> ptext ":->" <+> pprintRule r2
pprintRule (r1 :->= r2) = pprintRule r1 <+> ptext ":->=" <+> pprintRule r2
pprintRule (Single w1) = text (unwords w1)

-- | Pretty Print extra information
pprintAnyC :: AnyContent -> Doc
pprintAnyC (AnyI i) = text i
pprintAnyC (AnyS s) = text s
pprintAnyC (AnyA aa) = fsep (map pprintAnyC aa)