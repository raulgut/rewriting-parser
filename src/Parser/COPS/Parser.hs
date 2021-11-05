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
import Parser.COPS.TRS.Grammar (Spec (..), Decl (..))

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | Parse a COPS problem and return a COPS Problem
parseCOPS :: String -> Either ParseError Spec
parseCOPS = checkConsistency . parseTRS

-- | Parse a term rewriting system in COPS format
parseTRS :: String -> Either ParseError Spec
parseTRS s = doParse s trsParser

-- | Parses the system an returns the parsing error or the succesful
-- parsed system.
doParse :: String -> Parser a -> Either ParseError a
doParse s p = parse p "" s

-- | Check consistency (order, arguments and replacement map)
checkConsistency :: Either ParseError Spec -> Either ParseError Spec 
checkConsistency (Left parseError) = Left parseError
checkConsistency (Right (Spec decls)) = foldl checkDeclaration (Right . Spec $ []) decls

-- | Check declarations by order
checkDeclaration :: Either ParseError Spec -> Decl -> Either ParseError Spec
checkDeclaration (Left parseError) _ = Left parseError
checkDeclaration (Right (Spec [])) (CType ctype) = Right . Spec $ [CType ctype]
checkDeclaration (Right (Spec [CType ctype])) (Var vs) = Right . Spec $ [Var vs, CType ctype]
checkDeclaration (Right (Spec [])) (Var vs) = Right . Spec $ [Var vs]
checkDeclaration (Right (Spec [CType ctype])) (Context rmap) = Right . Spec $ [Context rmap, CType ctype]
checkDeclaration (Right (Spec (Var vs:rest))) (Context rmap) = Right . Spec $ (Context rmap:Var vs:rest)
checkDeclaration (Right (Spec [])) (Context rmap) = Right . Spec $ [Context rmap]
checkDeclaration (Right (Spec [CType ctype])) (Rules rs) = Right . Spec $ [Rules rs, CType ctype]
--checkDeclaration (Right (Spec (Var vs:rest))) (Context rmap) = Right . Spec $ (Context rmap:Var vs:rest)
--checkDeclaration (Right (Spec [])) (Context rmap) = Right . Spec $ [Context rmap]
checkDeclaration foo1 foo2 = foo1

{--

Var [Id] -- ^ Set of variables
   | Rules [Rule] -- ^ Set of rules
   | Context [(Id, [Int])] -- ^ Context-Sensitive strategy
   | Comment String -- ^ Extra information
   | CType CondType
   | CVar [Id]
   | Signature [(Id,Int)]

   --}