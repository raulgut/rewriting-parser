{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.COPS.Parser
-- Copyright   :  (c) muterm development team
-- License     :  see LICENSE
--
-- Maintainer  :  r.gutierrez@upm.es
-- Stability   :  unstable
-- Portability :  non-portable
--
-- This module manage the parser for TRSs in the COPS format.
--
-----------------------------------------------------------------------------

module Parser.COPS.Parser (

-- * Exported functions

parseCOPS

)  where

import Text.ParserCombinators.Parsec (parse, Parser, ParseError)
import Parser.COPS.TRS.Parser (trsParser)
import Parser.COPS.TRS.Grammar (Spec)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | Parse a COPS problem and return a COPS Problem
parseCOPS :: String -> Either ParseError Spec
parseCOPS = parseTRS

-- | Parse a term rewriting system in COPS format
parseTRS :: String -> Either ParseError Spec
parseTRS s = doParse s trsParser

-- | Parses the system an returns the parsing error or the succesful
-- parsed system.
doParse :: String -> Parser a -> Either ParseError a
doParse s p = parse p "" s
