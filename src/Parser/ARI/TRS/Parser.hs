-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.ARI.TRS.Parser
-- Copyright   :  (c) muterm development team
-- License     :  see LICENSE
--
-- Maintainer  :  r.gutierrez@upm.es
-- Stability   :  unstable
-- Portability :  non-portable
--
-- This module manage the parser for TRSs in ARI format
--
-----------------------------------------------------------------------------
module Parser.ARI.TRS.Parser (

-- * Exported functions

trsParser, term

) where

import Parser.TRS.Grammar
import Parser.ARI.TRS.Scanner

import Text.ParserCombinators.Parsec (Parser(..), (<|>), many1, sepEndBy
  , many, option, char, sepBy, try, noneOf, digit, (<?>), eof, manyTill)
import Text.ParserCombinators.Parsec.Prim (GenParser)
import Control.Monad (liftM)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- |parse TRS specification
trsParser :: Parser Spec
trsParser = liftM Spec decls

-- | We consider different types of declarations
decls :: Parser [Decl]
decls = do { try whiteSpace
           ; char '('
           ; reserved "format"
           ; whiteSpace
           ; declTRSs
           }

-- | A declaration for unsorted TRSs without theory
declTRSs = do { dformat <- declTRSFormat <|> declCTRSFormat <|> declCSTRSFormat <|> declCSCTRSFormat
              ; dfuncs <- (whiteSpace >> declSignature)
              ; drules <- (whiteSpace >> declRules)
              ; return $ (dformat:dfuncs) ++ [drules]
              }

-- Formats

-- | TRS format
declTRSFormat :: Parser Decl
declTRSFormat = do { reserved "TRS" 
                    ; char ')'
                    ; return . Format $ TRSStandard
                    }


-- | CTRS format
declCTRSFormat :: Parser Decl
declCTRSFormat = do { reserved "CTRS" 
                    ; condtyp <- (semiEq <|> join <|> oriented)
                    ; char ')'
                    ; return . Format $ TRSConditional condtyp
                    }

-- | CSTRS format
declCSTRSFormat :: Parser Decl
declCSTRSFormat = reserved "CSTRS" >> char ')' >> return (Format TRSContextSensitive)

-- | CSCTRS format
declCSCTRSFormat :: Parser Decl
declCSCTRSFormat = do { reserved "CSCTRS" 
                      ; condtyp <- (semiEq <|> join <|> oriented)
                      ; char ')'
                      ; return . Format $ TRSContextSensitiveConditional condtyp
                      }

-- Conditions

-- | Semi-equational conditions
semiEq :: Parser CondType
semiEq = reserved "semi-equational" >> return SemiEquational

-- | Join conditions
join :: Parser CondType
join = reserved "join" >> return Join

-- | Oriented conditions
oriented :: Parser CondType
oriented = reserved "oriented" >> return Oriented

-- Rules

-- | Rules declaration is formed by a reserved word plus a set of
--   rules
declRules :: Parser Decl
declRules = liftM Rules (many (parens rule))
   
-- | A term
term :: Parser Term
term =
 do { char '('
    ; n <- identifier
    ; terms <- option [] (many1 term)
    ; char ')'
    ; many (char ' ')
    ; return (T n terms) 
    }
 <|>
 do { m <- identifier -- remove white spaces
    ; return (T m [])
    }

-- | Rule
rule :: Parser Rule
rule =
 do reserved "rule"
    t1 <- term
    t2 <- term
    conds <- option [] (many1 $ parens cond)
    return $ Rule (t1 :-> t2) conds

-- | Condition
cond :: Parser Condition
cond =
 do reservedOp "="
    t1 <- term
    t2 <- term
    return (t1 :==: t2)

-- Signature

-- | Signature declaration is formed by list of functions with arity
declSignature :: Parser [Decl]
declSignature = do { funcs <- (many (try $ parens fun))
                   ; return [Signature $ map (\(f,ar,_) -> (f,ar)) funcs
                            ,Context $ map (\(f,ar,rp) -> (f,getRepMap f ar rp)) funcs]
                   }
  where
   getRepMap f ar Nothing = [1..ar]
   getRepMap f ar (Just replM) = replM

-- | Function symbol
fun :: Parser (TId,Int, Maybe [Int])
fun =
 do reserved "fun"
    n <- identifier
    m <- many1 digit
    replM <- option Nothing replacementMap
    return (n,read m,replM)

-- | Replacement Map
replacementMap :: Parser (Maybe [Int])
replacementMap = do { whiteSpace
                    ; reserved ":replacement-map" -- remove 
                    ; ps <- parens (blankSep' natural)
                    ; return . Just $ (map fromInteger ps)
                    }

-- | Separated by comma
blankSep' :: Text.ParserCombinators.Parsec.Prim.GenParser Char () a
             -> Text.ParserCombinators.Parsec.Prim.GenParser Char () [a]
blankSep' = (`sepEndBy` whiteSpace)