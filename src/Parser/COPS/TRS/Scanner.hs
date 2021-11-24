-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.COPS.TRS.Scanner
-- Copyright   :  (c) muterm development team
-- License     :  see LICENSE
--
-- Maintainer  :  r.gutierrez@upm.es
-- Stability   :  unstable
-- Portability :  non-portable
--
-- This module manage the scanner options for TRSs in COPS format
--
-----------------------------------------------------------------------------

module Parser.COPS.TRS.Scanner (

-- * Exported functions

lexer, whiteSpace, lexeme, symbol, natural, parens, comma, semi
, identifier, reserved, reservedOp, commaSep, stringLiteral, brackets

) where

import Text.ParserCombinators.Parsec (oneOf, CharParser, (<|>), alphaNum)
import qualified Text.ParserCombinators.Parsec.Token as P (stringLiteral, reserved, reservedOp, identifier, semi, comma, parens, brackets, natural, symbol, lexeme, whiteSpace, makeTokenParser, TokenParser, caseSensitive, reservedNames, reservedOpNames, opLetter, opStart, identLetter, identStart, nestedComments, commentEnd, commentStart, commentLine, LanguageDef, commaSep)
import Text.ParserCombinators.Parsec.Language(haskellStyle, emptyDef)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | Parsec list of reserved symbols
trsDef :: P.LanguageDef st
trsDef = emptyDef {
     P.commentStart = ""
   , P.commentEnd = ""
   , P.commentLine = ""
   , P.nestedComments = True
   , P.identStart = alphaNum <|> oneOf "<>~!·$%&/.:;-_{}[]^*+ç¡'¿?=#@"
   , P.identLetter = P.identStart trsDef
   , P.opStart = oneOf ")(\"-"
   , P.opLetter = oneOf ")(\",><="
   , P.reservedNames= [ "RULES", "VAR", "ORIENTED", "SEMI-EQUATIONAL", "JOIN" , "CONDITIONTYPE"
                      , "REPLACEMENT-MAP", "COMMENT", "->", "==" {-- , "SIG" --}]
   , P.reservedOpNames = ["->","=="]
   , P.caseSensitive = True
   }

-- | Create lexer using 'srsDef' definition.
lexer :: P.TokenParser ()
lexer = P.makeTokenParser $ trsDef

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

-- | Brackets
brackets :: CharParser () a -> CharParser () a
brackets = P.brackets lexer

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
