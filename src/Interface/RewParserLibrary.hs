-----------------------------------------------------------------------------
-- |
-- Module      :  Interface.CLI
-- Copyright   :  (c) muterm development team
-- License     :  see LICENSE
--
-- Maintainer  :  rgutierrez@dsic.upv.es
-- Stability   :  unstable
-- Portability :  non-portable
--
-- This module manage the Library that parses INF checker.
--
-----------------------------------------------------------------------------

module Interface.RewParserLibrary (

-- * Exported data

TRS (..), INF (..), TRSType (..), CondType (..), Rule (..), SimpleRule (..)
, Term (..), Condition (..), Equation (..), SimpleEquation (..), Predicate (..)
, SimplePredicate (..), Formula (..)

-- * Exported functions

, executeRewParser

) where

import Interface.CLI (autoparse)
import Parser.TRS.Grammar (TRS (..), INF (..), TRSType (..), CondType (..), Rule (..)
  , SimpleRule (..), Term (..), Condition (..), Equation (..), SimpleEquation (..)
  , Predicate (..), SimplePredicate (..), Formula (..))

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

executeRewParser :: String -> String -> (TRS,Maybe INF)
executeRewParser fname strInput
  = autoparse fname strInput
