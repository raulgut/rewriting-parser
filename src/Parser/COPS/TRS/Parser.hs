-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.COPS.TRS.Parser
-- Copyright   :  (c) muterm development team
-- License     :  see LICENSE
--
-- Maintainer  :  r.gutierrez@upm.es
-- Stability   :  unstable
-- Portability :  non-portable
--
-- This module manage the parser for TRSs in COPS format
--
-----------------------------------------------------------------------------
module Parser.COPS.TRS.Parser (

-- * Exported functions

trsParser, trsExtParser, term

) where

import Parser.TRS.Grammar
import Parser.COPS.TRS.Scanner

import Text.ParserCombinators.Parsec (Parser(..), many, (<|>), many1, sepEndBy
  , option, char, sepBy, try, noneOf, digit, sepEndBy1)
import Text.ParserCombinators.Parsec.Prim (GenParser)
import Control.Monad (liftM)
-- Debugging
--import Text.Parsec (ParsecT, getParserState, stateInput)
--import Data.Functor.Identity (Identity)
--import Debug.Trace (trace)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- |parse TRS specification
trsParser :: Parser Spec
trsParser = liftM Spec (many1 (whiteSpace >> parens decl))

-- |parse extended TRS specification
trsExtParser :: Parser Spec
trsExtParser = liftM Spec (many1 (whiteSpace >> parens declExt))

-- | A declaration is form by a set of variables, a theory, a set of
-- rules, a strategy an extra information
decl :: Parser Decl
decl = declCondType <|> declVar <|> {-- declSignature <|> --} declCSStrategy <|> declRules <|> declComment

-- | A declaration is form by a set of variables, a theory, a set of
-- rules, a strategy an extra information
declExt :: Parser Decl
declExt = declProblem <|> declCondType <|> declVar <|> {-- declSignature <|> --} declCSStrategy 
            -- <|> declCOPSCSStrategy
            <|> declFOTheory <|> declHornClauses <|> declEquations 
            <|> declRules <|> declConditions <|> declComment

-- | Problem type declaration
declProblem :: Parser Decl
declProblem = reserved "PROBLEM INFEASIBILITY" >> return (Problem INFEASIBILITY)

-- | Condition type declaration is formed by a reserved word plus SEMI-EQUATIONAL, JOIN, or ORIENTED
declCondType :: Parser Decl
declCondType = reserved "CONDITIONTYPE" >> liftM CType (semiEq <|> join <|> oriented)

-- | Semi-equational conditions
semiEq :: Parser CondType
semiEq = reserved "SEMI-EQUATIONAL" >> return SemiEquational

-- | Join conditions
join :: Parser CondType
join = reserved "JOIN" >> return Join

-- | Oriented conditions
oriented :: Parser CondType
oriented = reserved "ORIENTED" >> return Oriented


-- | Rules declaration is formed by a reserved word plus a set of
--   rules
declRules :: Parser Decl
declRules = reserved "RULES" >> liftM Rules (many rule)

-- | Equations declaration is formed by a reserved word plus a set of
--   equations
declEquations :: Parser Decl
declEquations = reserved "EQUATIONS" >> liftM Equations (many equation)

-- | Horn clause declaration is formed by a reserved word plus a set of
--   Horn clauses
declHornClauses :: Parser Decl
declHornClauses = reserved "HORN-CLAUSES" >> liftM Predicates (many hclause)

-- | First-order theory declaration is formed by a reserved word plus a set of
--   formulae
declFOTheory :: Parser Decl
declFOTheory = reserved "FO-THEORY" >> liftM FOTheory formula

-- | Infesibility Conditions
declConditions = reserved "CONDITION" >> liftM Conditions (semicolonSep' (commaSep' cond))

-- | Variables declaration is formed by a reserved word plus a set of
--   variables
declVar :: Parser Decl
declVar = reserved "VAR" >> do { idList <- phrase
                               ; return . Var $ idList
                               }

-- | A formula term
formulaTerm :: Parser Term
formulaTerm = try formulaTerm3 <|> parens formulaTerm <|> formulaTerm'

-- | A formula term
formulaTerm3 :: Parser Term
formulaTerm3 = do t1 <- parens formulaTerm <|> formulaTerm'
                  op <- formulaOps
                  t2 <- parens formulaTerm <|> formulaTerm'
                  return $ T op [t1,t2]

-- | A formula term
formulaTerm' :: Parser Term
formulaTerm' =
 do n <- identifier
    terms <- option [] (parens (commaSep' formulaTerm))
    return (T n terms)

-- | A term
term :: Parser Term
term =
 do n <- identifier
    terms <- option [] (parens (commaSep' term))
    return (T n terms)

-- | Rule
rule :: Parser Rule
rule =
 do sr <- simpleRule
    conds <- option [] (reservedOp "|" >> commaSep' cond)
    return (Rule sr conds)

-- | Simple rule
simpleRule =
 do t1 <- term
    op <- ruleOps
    t2 <- term
    return (op t1 t2)

-- | Rule options
ruleOps = (reservedOp "->" >> return (:->))

-- | Equation
equation :: Parser Equation
equation =
 do seq <- simpleEquation
    conds <- option [] (reservedOp "|" >> commaSep' cond)
    return (Equation seq conds)

-- | Simple equation
simpleEquation =
 do t1 <- term
    op <- eqOps
    t2 <- term
    return (op t1 t2)

-- | Equation options
eqOps = (reservedOp "=" >> return (:=))

-- | Horn clause
hclause :: Parser Predicate
hclause =
 do sr <- simplePredicate
    conds <- option [] (reservedOp "|" >> commaSep' cond)
    return (Predicate sr conds)

-- | Simple atom
{--
simplePredicate = 
   do (T n ts) <- term
      return (PrA n ts)
--}
simplePredicate = do option 1 (brackets natural)
                     t1 <- term
                     apply <- try infixRelPred <|> (return (\(T n ts) -> (PrA n ts)))
                     return (apply t1)

-- | checks if predicate
infixRelPred = do op <- (do { a <- identifier 
                            ; return (\x y -> PrA a [x,y])
                            })
                  t2 <- term
                  return (\t -> op t t2)

-- | Formula
formula :: Parser Formula
formula =
 do sr <- formulaTerm
    return (Formula sr)

-- | Condition
{--
cond =
 do option 1 (brackets natural)
    t1 <- term
    op <- condOps
    t2 <- term
    return (op t1 t2)
--}
cond = do option 1 (brackets natural)
          t1 <- term
          apply <- try infixRel <|> (return (\(T n ts) -> (PrAC n ts)))
          return (apply t1)

-- | checks if it is =, ->, ->*...
infixRel = do op <- condOps
              t2 <- term
              return (\t -> op t t2)

-- | Condition options
--condOps = (reservedOp "==" >> return (:==:))
condOps = (reservedOp "==" >> return (:==:)) <|>
          (reservedOp "->*" >> return (::->*)) <|>
          (reservedOp "->=" >> return (::->=)) <|>
          (reservedOp "->+" >> return (::->+)) <|>
          (reservedOp "->*<-" >> return (:-><-)) <|>
          (reservedOp "->" >> return (::->)) <|>
          (reservedOp "\\->*" >> return (::\->*)) <|>
          (reservedOp "\\->=" >> return (::\->=)) <|>
          (reservedOp "\\->+" >> return (::\->+)) <|>
          (reservedOp "\\->*<-/" >> return (:\-><-/)) <|>
          (reservedOp "\\->" >> return (::\->)) <|>
          (reservedOp "<-->*" >> return (:<-->*)) <|>
          (reservedOp "<-->" >> return (:<-->)) <|>
          (reservedOp "<-/\\->*" >> return (:<-/\->*)) <|>
          (reservedOp "<-/\\->" >> return (:<-/\->)) <|>
          (reservedOp "=" >> return (:=:)) <|>
          (reservedOp "|>=" >> return (:|>=)) <|>
          (reservedOp "|>" >> return (:|>=)) <|>
          (do a <- identifier 
              return (\x y -> PrAC a [x,y]))

-- | Condition options
--condOps = (reservedOp "==" >> return (:==:))
formulaOps = (reservedOp "==" >> return ("==")) <|>
          (reservedOp "->*" >> return ("->*")) <|>
          (reservedOp "->=" >> return ("->=")) <|>
          (reservedOp "->+" >> return ("->+")) <|>
          (reservedOp "->*<-" >> return ("->*<-")) <|>
          (reservedOp "->" >> return ("->")) <|>
          (reservedOp "\\->*" >> return ("\\->*")) <|>
          (reservedOp "\\->=" >> return ("->=")) <|>
          (reservedOp "\\->+" >> return ("\\->+")) <|>
          (reservedOp "\\->*<-/" >> return ("\\-><-/")) <|>
          (reservedOp "\\->" >> return ("\\->")) <|>
          (reservedOp "<-->*" >> return ("<-->*")) <|>
          (reservedOp "<-->" >> return ("<-->")) <|>
          (reservedOp "<-/\\->*" >> return ("<-/\\->*")) <|>
          (reservedOp "<-/\\->" >> return ("<-/\\->")) <|>
          (reservedOp "=" >> return ("=")) <|>
          (reservedOp "|>=" >> return ("|>=")) <|>
          (reservedOp "|>" >> return ("|>=")) <|>
          (reservedOp "/\\" >> return ("/\\")) <|>
          (reservedOp "\\/" >> return ("\\/")) <|>
          (reservedOp "=>" >> return ("=>")) <|>
          (reservedOp "<=>" >> return ("<=>")) <|>
          (do a <- identifier 
              return a)

-- | Context-sensitive strategy
declCSStrategy :: Parser Decl
declCSStrategy =
 do reserved "REPLACEMENT-MAP"
    strats <- many$ parens (do a <- identifier
                               b <- commaSep' natural
                               return (a, map fromInteger b)
                           )
    return $ Context strats

-- | Extra information
declComment =
 do reserved "COMMENT"
    decls <- many $ noneOf ")"
    return$ Comment decls

-- | A phrase
phrase = many identifier

-- | Separated by comma
commaSep' :: Text.ParserCombinators.Parsec.Prim.GenParser Char () a
             -> Text.ParserCombinators.Parsec.Prim.GenParser Char () [a]
commaSep' = (`sepEndBy` comma)

-- | Separated by semicolon
semicolonSep' :: Text.ParserCombinators.Parsec.Prim.GenParser Char () a
             -> Text.ParserCombinators.Parsec.Prim.GenParser Char () [a]
semicolonSep' = (`sepBy` semi)

-- | Separated by white space
whiteSpaceSep' :: Text.ParserCombinators.Parsec.Prim.GenParser Char () a
             -> Text.ParserCombinators.Parsec.Prim.GenParser Char () [a]
whiteSpaceSep' = (`sepEndBy1` whiteSpace)

{-- | Signature declaration is formed by list of functions with arity
declSignature :: Parser Decl
declSignature = reserved "SIG" >> liftM Signature (many (parens fun))

-- | Function symbol
fun :: Parser (TId,Int)
fun =
 do n <- identifier
    m <- many1 digit
    return (n,read m)
--}

-------------
-- Debugging 
-------------

{--
-- | print trace message
println msg = trace (show msg) $ return ()

-- | Use seeNext 10 shows next 10 characters to be consumed
seeNext :: Int -> ParsecT String u Identity ()
seeNext n = do
  s <- getParserState
  let out = take n (stateInput s)
  println out
--}