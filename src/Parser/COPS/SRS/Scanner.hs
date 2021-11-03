-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.TPDB.SRS.Scanner
-- Copyright   :  (c) muterm development team
-- License     :  see LICENSE
-- 
-- Maintainer  :  jiborra@dsic.upv.es
-- Stability   :  unstable
-- Portability :  non-portable 
--
-- This module manage the scanner options for SRSs in the TPDB
--
-----------------------------------------------------------------------------

module Parser.TPDB.SRS.Scanner (

-- * Exported functions

srsDef , lexer, whiteSpace, lexeme, symbol, natural, parens, comma
, semi, identifier, reserved, reservedOp, commaSep, stringLiteral

) where

import Text.ParserCombinators.Parsec (alphaNum, (<|>), oneOf, CharParser)
import qualified Text.ParserCombinators.Parsec.Token as P (commaSep, stringLiteral, reserved, reservedOp, identifier, semi, comma, parens, natural, symbol, lexeme, whiteSpace, makeTokenParser, TokenParser, caseSensitive, reservedNames, reservedOpNames, opLetter, opStart, identLetter, identStart, nestedComments, commentEnd, commentStart, commentLine, LanguageDef)
import Text.ParserCombinators.Parsec.Language( haskellStyle, emptyDef)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | Parsec list of reserved symbols.
srsDef :: P.LanguageDef st
srsDef = emptyDef {
     P.commentStart = ""
   , P.commentEnd = ""
   , P.commentLine = ""
   , P.nestedComments = True
   , P.identStart = alphaNum <|> oneOf "<>~!\\·$%&/.:;-_{}[]^*+ç¡'¿?=#@|" -- noneOf " ()\","
   , P.identLetter = P.identStart srsDef
   , P.opStart = oneOf ")(\"-"
   , P.opLetter = oneOf ")(\",>="
   , P.reservedNames= ["RULES","STRATEGY","LEFTMOST","RIGHTMOST","PAIRS","UNHRULES"]
   , P.reservedOpNames = ["->","->="]
   , P.caseSensitive = True
   }

-- | Create lexer with the 'srsDef' definition.
lexer :: P.TokenParser ()
lexer = P.makeTokenParser srsDef
         
-- | white Space 
whiteSpace :: CharParser () ()
whiteSpace= P.whiteSpace lexer

-- | Lexeme
lexeme :: CharParser () a -> CharParser () a
lexeme = P.lexeme lexer

-- | Symbol
symbol :: String -> CharParser () String
symbol = P.symbol lexer

-- | Natural
natural :: CharParser () Integer
natural = P.natural lexer

-- | Parens
parens :: CharParser () a -> CharParser () a
parens = P.parens lexer

-- | Comma
comma :: CharParser () String
comma = P.comma lexer

-- | Semi
semi :: CharParser () String
semi = P.semi lexer

-- | Identifier
identifier :: CharParser () String
identifier= P.identifier lexer

-- | Reserved
reserved :: String -> CharParser () ()
reserved = P.reserved lexer

-- | Reserved option
reservedOp :: String -> CharParser () ()
reservedOp= P.reservedOp lexer

-- | Separated by comma
commaSep :: CharParser () a -> CharParser () [a]
commaSep = P.commaSep lexer

-- | String literal
stringLiteral :: CharParser () String
stringLiteral = P.stringLiteral lexer
