{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.ARI.Parser
-- Copyright   :  (c) muterm development team
-- License     :  see LICENSE
--
-- Maintainer  :  r.gutierrez@upm.es
-- Stability   :  unstable
-- Portability :  non-portable
--
-- This module manage the parser for TRSs in the ARI format.
--
-----------------------------------------------------------------------------

module Parser.ARI.Parser (

-- * Exported functions

parseARI, parseExtARI

)  where

import Parser.ARI.TRS.Parser (trsParser)
import Parser.TRS.Grammar (Spec (..), Decl (..), TRSType(..), TRS (..), CondType (..)
  , Term (..), Rule (..), TId, TRSType (..), getTerms, INF (..), Formula (..)
  , SimplePredicate (..), ProbType (..), Condition (..), getTermsEq)
import Parser.TRS.Properties (nonVarLHS', isConditional, hasExtraVars, hasExtraVars'
  , isCRule)

import Text.ParserCombinators.Parsec (parse, Parser, ParseError)
import Text.ParserCombinators.Parsec.Error (Message (..), newErrorMessage)
import Text.Parsec.Pos (newPos)
import Data.Map as M (empty, lookup, insert, fromList)
import Data.Set as S (empty, fromList, member, insert)
import Data.List (sort, nub)
import Control.Monad.State (State, evalState, get, put, runState)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | Parses a COPS problem and return a COPS Problem
parseARI :: String -> Either ParseError TRS
parseARI = checkConsistency . parseTRS

-- | Parses a COPS problem and return a COPS Problem
parseExtARI :: String -> Either ParseError (TRS, Maybe INF)
parseExtARI str 
  = case (checkExtConsistency . parseTRS $ str) of
      Left parseError -> Left parseError
      Right (myTRS,myINF) -> Right (myTRS, myINF)

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
  = evalState (checkWellFormed decls) (TRS M.empty S.empty [] [] [] [] Nothing TRSStandard)

-- | Checks consistency (order, arguments and replacement map)
checkExtConsistency :: Either ParseError Spec -> Either ParseError (TRS, Maybe INF) 
checkExtConsistency (Left parseError) = Left parseError
checkExtConsistency (Right (Spec decls)) 
  = evalState (checkExtWellFormed decls) (TRS M.empty S.empty [] [] [] [] Nothing TRSStandard, Nothing)

-- | Extracts the signature and checks if the rules are well-formed wrt that
-- signature. Precondition: Declarations are in order.
checkWellFormed :: [Decl] -> State TRS (Either ParseError TRS)
checkWellFormed [] = do { myTRS <- get 
                        ; return . Right $ myTRS}
checkWellFormed (Format trsType':rest) = do { myTRS <- get 
                                            ; put $ myTRS { trsType = trsType'}
                                            ; checkWellFormed rest
                                            }
checkWellFormed (Signature fs:rest) = if (length . nub $ fs) == (length fs) then
                                        do { myTRS <- get
                                           ; put $ myTRS {trsSignature = M.fromList fs}
                                           ; checkWellFormed rest
                                           }
                                      else 
                                        return . Left $ newErrorMessage (UnExpect $ "duplicated symbols in signature declaration") (newPos "" 0 0)
checkWellFormed (Context rmap:rest) = do { myTRS <- get 
                                         ; if (length . nub . map fst $ rmap) == length rmap then
                                             do { put $ myTRS { trsRMap = rmap }
                                                ; checkWellFormed rest
                                                }
                                           else 
                                             return . Left $ newErrorMessage (UnExpect $ "duplicated symbols in replacement map declaration") (newPos "" 0 0)
                                         }
checkWellFormed (Rules rs:rest) = do { result <- checkRules rs
                                     ; case result of
                                         Left parseError -> return . Left $ parseError
                                         Right _ -> do { myTRS <- get
                                                       ; put $ myTRS {trsRules = rs}
                                                       ; checkWellFormed rest
                                                       }
                                     }

-- | Extracts the signature and checks if the rules are well-formed wrt that
-- signature. Precondition: Declarations are in order.
checkExtWellFormed :: [Decl] -> State (TRS, Maybe INF) (Either ParseError (TRS, Maybe INF))
checkExtWellFormed [] = do { (myTRS,myINF) <- get 
                        ; return . Right $ (myTRS,myINF)}
checkExtWellFormed (Format trsType':rest) = do { (myTRS,myINF) <- get 
                                            ; put $ (myTRS { trsType = trsType'},myINF)
                                            ; checkExtWellFormed rest
                                            }
checkExtWellFormed (Problem INFEASIBILITY:rest) = do { (myTRS,myINF) <- get 
                                                  ; put $ (myTRS,Just $ INF S.empty [])
                                                  ; checkExtWellFormed rest
                                                  }
checkExtWellFormed (Signature fs:rest) = if (length . nub $ fs) == (length fs) then
                                        do { (myTRS,myINF) <- get
                                           ; put $ (myTRS {trsSignature = M.fromList fs},myINF)
                                           ; checkExtWellFormed rest
                                           }
                                      else 
                                        return . Left $ newErrorMessage (UnExpect $ "duplicated symbols in signature declaration") (newPos "" 0 0)
checkExtWellFormed (Context rmap:rest) = do { (myTRS,myINF) <- get 
                                         ; if (length . nub . map fst $ rmap) == length rmap then
                                             do { put $ (myTRS { trsRMap = rmap },myINF)
                                                ; checkExtWellFormed rest
                                                }
                                           else 
                                             return . Left $ newErrorMessage (UnExpect $ "duplicated symbols in replacement map declaration") (newPos "" 0 0)
                                         }
checkExtWellFormed (Rules rs:rest) = do { (myTRS,myINF) <- get 
                                        ; let (result,myNewTRS) = runState (checkRules rs) myTRS
                                        ; case result of
                                            Left parseError -> return . Left $ parseError
                                            Right _ -> do { put $ (myNewTRS {trsRules = rs},myINF)
                                                          ; checkExtWellFormed rest
                                                          }
                                        }
checkExtWellFormed (Conditions cs:rest) = do { (myTRS,maybeMyINF) <- get
                                             ; case maybeMyINF of
                                                  Just myINF -> do { let (result, (myNewTRS, myNewINF)) = runState (checkConditions cs) (myTRS, myINF)
                                                                   ; case result of
                                                                       Left parseError -> return . Left $ parseError
                                                                       Right _ -> do { put $ (myNewTRS,Just $ myNewINF {trsConditions = cs})
                                                                                     ; checkExtWellFormed rest
                                                                                     }
                                                                   }
                                                  Nothing -> return . Left $ newErrorMessage (UnExpect $ "conditions are only allowed in infeasibility problems") (newPos "" 0 0)
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
checkRules (r:rs) = do { myTRS <- get
                       ; let fs = trsSignature myTRS
                       ; if nonVarLHS' fs r then -- lhs is non-variable 
                           if (isConditional myTRS) || (not . isCRule $ r) then -- conditional rules not allowed in TRSs 
                             if (isConditional myTRS) || (not . hasExtraVars' fs $ r) then -- extra variables not allowed in non-conditional rules
                               do { result <- checkTerms . getTerms $ r 
                                   ; case result of
                                       Left parseError -> return . Left $ parseError 
                                       Right _ -> checkRules rs
                                   }
                             else
                               return . Left $ newErrorMessage (UnExpect $ "extra variables in the rule " ++ (show r)) (newPos "" 0 0)
                           else
                             return . Left $ newErrorMessage (UnExpect $ "conditional rule: " ++ (show r) ++ " is not allowed in TRSs and CS-TRSs") (newPos "" 0 0)
                         else
                           return . Left $ newErrorMessage (UnExpect $ "variable in the left-hand side of the rule " ++ (show r)) (newPos "" 0 0)
                       }

-- | Checks if the conditions are well-formed wrt the extracted signature
checkConditions :: [[Condition]] -> State (TRS,INF) (Either ParseError ())
checkConditions [] = return . Right $ ()
checkConditions (c:cs) = do { result <- checkAndConditions c
                            ; case result of
                                Left parseError -> return . Left $ parseError 
                                Right _ -> checkConditions cs
                            }

-- | Checks if the conditions are well-formed wrt the extracted signature
checkAndConditions :: [Condition] -> State (TRS,INF) (Either ParseError ())
checkAndConditions [] = return . Right $ ()
checkAndConditions (c:cs) = do { result <- checkCTerms . getTermsEq $ c 
                               ; case result of
                                   Left parseError -> return . Left $ parseError 
                                   Right _ -> checkAndConditions cs
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
                            ; case M.lookup id funcs of 
                               Nothing -> if (arglen == 0) then
                                            do { put $ myTRS { trsVariables = S.insert id $ vars }
                                               ; return . Right $ ()
                                               }
                                          else
                                            return . Left $ newErrorMessage (UnExpect $ "arguments in variable " ++ id) (newPos "" 0 0)
                               Just len -> if (arglen == len) then 
                                             checkTerms terms 
                                           else
                                             return . Left $ newErrorMessage (UnExpect $ "symbol " ++ id ++ " with arity " ++ (show arglen) ++ " in term " ++ (show $ T id terms)) (newPos "" 0 0)
                            }

-- | Checks if the terms are well-formed wrt the extracted signature
checkCTerms :: [Term] -> State (TRS,INF) (Either ParseError ())
checkCTerms [] = return . Right $ ()
checkCTerms (t:ts) = do { result <- checkCTerm t 
                        ; case result of
                            Left parseError -> return . Left $ parseError
                            Right _ -> checkCTerms ts
                        }

-- | Checks if the term is well-formed wrt the extracted signature
checkCTerm :: Term -> State (TRS,INF) (Either ParseError ())
checkCTerm (T id terms) = do { (myTRS,myINF) <- get
                            ; let cvars = trsCondVariables myINF
                            ; let funcs = trsSignature myTRS
                            ; let arglen = length terms
                            ; case (M.lookup id funcs) of
                               Nothing -> do { put $ (myTRS,myINF { trsCondVariables = S.insert id cvars })
                                             ; checkCTerms terms
                                             }
                               Just len -> if (arglen == len) then 
                                             checkCTerms terms 
                                           else
                                             return . Left $ newErrorMessage (UnExpect $ "symbol " ++ id ++ " with arity " ++ (show arglen) ++ " in term " ++ (show $ T id terms)) (newPos "" 0 0)
                            }

-- | Checks if the replacement map satisfies arity restriction and increasing order
checkRMap :: [(TId, [Int])] -> State TRS (Either ParseError ())
checkRMap [] = return . Right $ ()
checkRMap ((f,[]):rmaps) = checkRMap rmaps 
checkRMap ((f,rmap):rmaps) = do { myTRS <- get 
                                ; case M.lookup f (trsSignature myTRS) of 
                                   Nothing -> return . Left $ newErrorMessage (UnExpect $ "function symbol " ++ f ++ " in replacement map (the symbol does not appear in signature)") (newPos "" 0 0)
                                   Just arity -> let srmap = sort rmap in
                                                 if (rmap == srmap) && (head rmap >= 1) && (last rmap <= arity) then
                                                   checkRMap rmaps 
                                                 else
                                                   return . Left $ newErrorMessage (UnExpect $ "replacement map for symbol " ++ f ++ " (must be empty" ++ (if arity > 0 then " or an ordered list of numbers in [1.." ++ (show arity) ++ "] separated by whitespaces" else "") ++ ")") (newPos "" 0 0) 
                                }
