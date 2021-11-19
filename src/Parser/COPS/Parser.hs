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

import Parser.COPS.TRS.Parser (trsParser)
import Parser.COPS.TRS.Grammar (Spec (..), Decl (..), TRSType(..), TRS (..), CondType (..)
  , Term (..), Rule (..), Id, TRSType (..), getTerms)

import Text.ParserCombinators.Parsec (parse, Parser, ParseError)
import Text.ParserCombinators.Parsec.Error (Message (..), newErrorMessage)
import Text.Parsec.Pos (newPos)
import Data.Map as M (empty, lookup, insert)
import Data.Set as S (empty, fromList, member)
import Data.List (sort)
import Control.Monad.State (State, evalState, get, put)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | Parses a COPS problem and return a COPS Problem
parseCOPS :: String -> Either ParseError TRS
parseCOPS = checkConsistency . parseTRS

-- | Parses a term rewriting system in COPS format
parseTRS :: String -> Either ParseError Spec
parseTRS s = doParse s trsParser

-- | Parses the system an returns the parsing error or the succesful
-- parsed system.
doParse :: String -> Parser a -> Either ParseError a
doParse s p = parse p "" s

-- | Checks consistency (order, arguments and replacement map)
checkConsistency :: Either ParseError Spec -> Either ParseError TRS 
checkConsistency (Left parseError) = Left parseError
checkConsistency (Right (Spec decls)) 
  = case foldl checkDeclaration (Right (Spec [])) decls of 
     Left parseError -> Left parseError
     Right (Spec _) -> evalState (checkWellFormed decls) (TRS M.empty S.empty [] [] TRSStandard)

-- | Checks declaration order
checkDeclaration :: Either ParseError Spec -> Decl -> Either ParseError Spec
checkDeclaration (Left parseError) _ = Left parseError
checkDeclaration (Right (Spec [])) (CType ctype) = Right . Spec $ [CType ctype]
checkDeclaration (Right (Spec [])) (Var vs) = Right . Spec $ [Var vs]
checkDeclaration (Right (Spec [])) (Context rmap) = Right . Spec $ [Context rmap]
checkDeclaration (Right (Spec [])) (Rules rs) = Right . Spec $ [Rules rs]
checkDeclaration (Right (Spec [CType ctype])) (Var vs) = Right . Spec $ [Var vs, CType ctype]
checkDeclaration (Right (Spec [CType ctype])) (Context rmap) = Right . Spec $ [Context rmap, CType ctype]
checkDeclaration (Right (Spec [CType ctype])) (Rules rs) = Right . Spec $ [Rules rs, CType ctype]
checkDeclaration (Right (Spec (Var vs:rest))) (Context rmap) = Right . Spec $ (Context rmap:Var vs:rest)
checkDeclaration (Right (Spec (Var vs:rest))) (Rules rs) = Right . Spec $ (Rules rs:Var vs:rest)
checkDeclaration (Right (Spec (Context rmap:rest))) (Rules rs) = Right . Spec $ (Rules rs:Context rmap:rest)
checkDeclaration (Right (Spec (Rules rs:rest))) (Comment c) = Right . Spec $ (Comment c:Rules rs:rest)
checkDeclaration _ (CType _) = Left $ newErrorMessage (UnExpect "CONDITIONTYPE block") (newPos "" 0 0)
checkDeclaration _ (Var _) = Left $ newErrorMessage (UnExpect "VAR block") (newPos "" 0 0)
checkDeclaration _ (Context _) = Left $ newErrorMessage (UnExpect "REPLACEMENT-MAP block") (newPos "" 0 0)
checkDeclaration _ (Rules _) = Left $ newErrorMessage (UnExpect "RULES block") (newPos "" 0 0)
checkDeclaration _ (Comment _) = Left $ newErrorMessage (UnExpect "COMMENT block") (newPos "" 0 0)

-- | Extracts the signature and checks if the rules are well-formed wrt that
-- signature. Precondition: Declarations are in order.
checkWellFormed :: [Decl] -> State TRS (Either ParseError TRS)
checkWellFormed [] = do { myTRS <- get 
                        ; return . Right $ myTRS}
checkWellFormed (CType SemiEquational:rest) = do { myTRS <- get 
                                                 ; put $ myTRS { trsType = TRSConditional SemiEquational }
                                                 ; checkWellFormed rest
                                                 }
checkWellFormed (CType Join:rest) = do { myTRS <- get 
                                       ; put $ myTRS { trsType = TRSConditional Join }
                                       ; checkWellFormed rest
                                       }
checkWellFormed (CType Oriented:rest) = do { myTRS <- get 
                                           ; put $ myTRS { trsType = TRSConditional Oriented }
                                           ; checkWellFormed rest
                                           }
checkWellFormed (Var vs:rest) = do { myTRS <- get 
                                   ; put $ myTRS { trsVariables = S.fromList vs }
                                   ; checkWellFormed rest
                                   }
checkWellFormed (Context rmap:rest) = do { myTRS <- get 
                                         ; put $ myTRS { trsRMap = rmap
                                                       , trsType = case trsType myTRS of
                                                                     TRSStandard -> TRSContextSensitive
                                                                     TRSConditional typ -> TRSContextSensitiveConditional typ}
                                         ; checkWellFormed rest}
checkWellFormed (Rules rs:rest) = do { result <- checkRules rs
                                     ; case result of
                                         Left parseError -> return . Left $ parseError
                                         Right _ -> do { myTRS <- get
                                                       ; put $ myTRS {trsRules = rs}
                                                       ; checkWellFormed rest
                                                       }
                                     }

-- | Checks if the rules are well-formed wrt the extracted signature
checkRules :: [Rule] -> State TRS (Either ParseError ())
checkRules [] = do { myTRS <- get
                     -- first, we extract the arity of symbols, then we check RMap 
                   ; case trsType myTRS of
                       TRSContextSensitive -> checkRMap . trsRMap $ myTRS
                       TRSContextSensitiveConditional _ -> checkRMap . trsRMap $ myTRS
                       _ -> return . Right $ ()
                   }
checkRules (r:rs) = do { result <- checkTerms . getTerms $ r 
                       ; case result of
                           Left parseError -> return . Left $ parseError 
                           Right _ -> checkRules rs
                       }

-- | Checks if the terms are well-formed wrt the extracted signature
checkTerms :: [Term] -> State TRS (Either ParseError ())
checkTerms [] = return . Right $ ()
checkTerms (t:ts) = do { result <- checkTerm t 
                       ; case result of
                           Left parseError -> return . Left $ parseError
                           Right _ -> checkTerms ts
                       }

-- | Checks if the term is well-formed wrt the extracted signature
checkTerm :: Term -> State TRS (Either ParseError ())
checkTerm (T id terms) = do { myTRS <- get
                            ; let vars = trsVariables myTRS
                            ; let funcs = trsSignature myTRS
                            ; let arglen = length terms
                            ; case (S.member id vars, M.lookup id funcs) of 
                               (False, Nothing) -> do { put $ myTRS { trsSignature = M.insert id (length terms) $ funcs }
                                                      ; checkTerms terms
                                                      }
                               (False, Just len) -> if (arglen == len) then 
                                                      checkTerms terms 
                                                    else
                                                      return . Left $ newErrorMessage (UnExpect $ "symbol " ++ id ++ " with arity " ++ (show arglen) ++ " in term " ++ (show $ T id terms)) (newPos "" 0 0)
                               (True, Nothing) -> if (arglen == 0) then 
                                                    return . Right $ ()
                                                  else
                                                    return . Left $ newErrorMessage (UnExpect $ "arguments in variable " ++ id) (newPos "" 0 0)
                               -- next case is not possible
                               _ -> return . Left $ newErrorMessage (UnExpect $ "variable and function symbols declaration " ++ id) (newPos "" 0 0)
                            }

-- | Checks if the replacement map satisfies arity restriction and increasing order
checkRMap :: [(Id, [Int])] -> State TRS (Either ParseError ())
checkRMap [] = return . Right $ ()
checkRMap ((f,[]):rmaps) = do { myTRS <- get 
                              ; case M.lookup f (trsSignature myTRS) of 
                                  Nothing -> return . Left $ newErrorMessage (UnExpect $ "function symbol " ++ f ++ " in replacement map (the symbol does not appear in rules)") (newPos "" 0 0)
                                  Just arity -> checkRMap rmaps 
                              }
checkRMap ((f,rmap):rmaps) = do { myTRS <- get 
                                ; case M.lookup f (trsSignature myTRS) of 
                                   Nothing -> return . Left $ newErrorMessage (UnExpect $ "function symbol " ++ f ++ " in replacement map (the symbol does not appear in rules)") (newPos "" 0 0)
                                   Just arity -> let srmap = sort rmap in
                                                 if (rmap == srmap) && (head rmap >= 1) && (last rmap <= arity) then
                                                   checkRMap rmaps 
                                                 else
                                                   return . Left $ newErrorMessage (UnExpect $ "replacement map for symbol " ++ f ++ " (must be empty" ++ (if arity > 0 then " or an ordered list of numbers in [1.." ++ (show arity) ++ "] separated by commas" else "") ++ ")") (newPos "" 0 0) 
                                }
