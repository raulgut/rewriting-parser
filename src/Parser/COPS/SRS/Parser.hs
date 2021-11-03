-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.TPDB.SRS.Parser
-- Copyright   :  (c) muterm development team
-- License     :  see LICENSE
-- 
-- Maintainer  :  jiborra@dsic.upv.es
-- Stability   :  unstable
-- Portability :  non-portable 
--
-- This module manage the parser for SRSs in the TPDB
--
-----------------------------------------------------------------------------

module Parser.TPDB.SRS.Parser (

-- * Exported functions

srsParser

) where

import Parser.TPDB.SRS.Grammar
import Parser.TPDB.SRS.Scanner

import Text.ParserCombinators.Parsec (Parser(..), many, (<|>), many1, chainl1, sepEndBy)
import Text.ParserCombinators.Parsec.Prim (GenParser)
import Control.Monad (liftM)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | parse SRS specification 
srsParser :: Parser Spec
srsParser = liftM Spec (many (parens decl))
 
-- | A declaration is form by a set of rules, a strategy an extra
-- information
decl :: Parser Decl
decl = declRules <|> declStrategy <|> declAny <|> declPairs <|> declUnhRules
   
-- | Rules declaration is formed by a reserved word plus a set of
-- rules
declRules :: Parser Decl
declRules =
 do reserved "RULES" >> liftM Rules (commaSep' rule)

-- | A strategy is form by a reserved rule and the strategy
declStrategy :: Parser Decl
declStrategy =
 do reserved "STRATEGY" >> liftM Strat (leftMost <|> rightMost)

-- | Extra information
declAny :: Parser Decl
declAny =
 do name <- identifier
    decls <- many anyContent
    return$ Any (Just name) decls

-- | Pairs declaration is formed by a reserved word plus a set of
-- rules (extension not in TPDB)
declPairs :: Parser Decl
declPairs =
 do reserved "PAIRS" >> liftM Pairs (commaSep' rule)

-- | Unhiding rules declaration is formed by a reserved word plus a
-- set of rules (extension not in TPDB)
declUnhRules :: Parser Decl
declUnhRules =
 do reserved "UNHRULES" >> liftM UnhRules (commaSep' rule)
 
-- | Extra information set
anyContent :: Parser AnyContent
anyContent = anyI <|> anyS <|> anyA <|> (comma >> anyContent)

-- | Identifiers    
anyI :: Parser AnyContent
anyI = liftM AnyI identifier

-- | Strings
anyS :: Parser AnyContent
anyS = liftM AnyS stringLiteral

-- | Others
anyA :: Parser AnyContent
anyA = liftM AnyA (parens$ many anyContent)

-- | Rule
rule :: Parser Rule
rule = liftM Single word `chainl1` ruleOps
 
-- | Rule options
ruleOps :: Parser (Rule -> Rule -> Rule)
ruleOps = (reservedOp "->" >> return (:->))
   <|>
             (reservedOp "->=" >> return (:->=))
 
-- | Word
word :: Parser [Id]
word = many1$ identifier
 
-- | Leftmost strategy
leftMost :: Text.ParserCombinators.Parsec.Prim.GenParser Char () StratDecl
leftMost = reserved "LEFTMOST" >> return LeftMost

-- | Rightmost strategy
rightMost :: Text.ParserCombinators.Parsec.Prim.GenParser Char () StratDecl
rightMost = reserved "RIGHTMOST" >> return RightMost

-- | Separated by comma
commaSep' :: Text.ParserCombinators.Parsec.Prim.GenParser Char () a
             -> Text.ParserCombinators.Parsec.Prim.GenParser Char () [a]
commaSep' = (`sepEndBy` comma)
