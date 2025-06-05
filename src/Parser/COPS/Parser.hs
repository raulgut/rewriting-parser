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

parseCOPS, parseExtCOPS

)  where

import Parser.COPS.TRS.Parser (trsParser, trsExtParser)
import Parser.TRS.Grammar (Spec (..), Decl (..), TRSType(..), TRS (..), CondType (..)
  , Term (..), Rule (..), TId, TRSType (..), getTerms, INF (..), ProbType (..)
  , Condition (..), getTermsEq, Equation (..), getEqTerms, getPredTerms, Predicate (..)
  , Formula (..), SimplePredicate (..), getFormulaTerms)
import Parser.TRS.Properties (nonVarLHS, isCRule, isCEquation, hasExtraVars, isConditional)

import Text.ParserCombinators.Parsec (parse, Parser, ParseError)
import Text.ParserCombinators.Parsec.Error (Message (..), newErrorMessage)
import Text.Parsec.Pos (newPos)
import Data.Map as M (empty, lookup, insert)
import Data.Set as S (empty, fromList, member)
import Data.List (sort, nub)
import Control.Monad.State (State, evalState, get, put, runState)
--import Debug.Trace (trace)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | Parses a COPS problem and return a COPS Problem
parseCOPS :: String -> Either ParseError TRS
parseCOPS = checkConsistency . parseTRS

-- | Parses a COPS problem and return a COPS Problem
parseExtCOPS :: String -> Either ParseError (TRS, Maybe INF)
parseExtCOPS = checkExtConsistency . parseExtTRS

-- | Parses a term rewriting system in COPS format
parseTRS :: String -> Either ParseError Spec
parseTRS s = doParse s trsParser

-- | Parses a term rewriting system in COPS format
parseExtTRS :: String -> Either ParseError Spec
parseExtTRS s = doParse s trsExtParser

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
     Right (Spec _) -> evalState (checkWellFormed decls) (TRS M.empty S.empty [] [] [] [] (Formula $ T "true" []) TRSStandard)

-- | Checks consistency (order, arguments and replacement map)
checkExtConsistency :: Either ParseError Spec -> Either ParseError (TRS, Maybe INF) 
checkExtConsistency (Left parseError) = Left parseError
checkExtConsistency (Right (Spec (Problem ptype:decls))) -- Infeasibility unfolded problem 
  = case foldl checkExtDeclaration (Right (Spec [])) (Problem ptype:decls) of 
     Left parseError -> Left parseError
     Right (Spec checkedDecls) -> evalState (checkExtWellFormed (reverse checkedDecls)) (TRS M.empty S.empty [] [] [] [] (Formula $ T "true" []) TRSStandard, Nothing)
checkExtConsistency (Right (Spec decls)) -- Confluence problem 
  = case foldl checkDeclaration (Right (Spec [])) decls of 
     Left parseError -> Left parseError
     Right (Spec _) -> 
      case (evalState (checkWellFormed decls) (TRS M.empty S.empty [] [] [] [] (Formula $ T "true" []) TRSStandard)) of
        Left parseError -> Left parseError
        Right myTRS -> Right (myTRS, Nothing)

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
checkDeclaration _ (Problem _) = Left $ newErrorMessage (UnExpect "PROBLEM block") (newPos "" 0 0)
checkDeclaration _ (Conditions _) = Left $ newErrorMessage (UnExpect "CONDITION block") (newPos "" 0 0)

-- | Checks declaration order
checkExtDeclaration :: Either ParseError Spec -> Decl -> Either ParseError Spec
checkExtDeclaration (Left parseError) _ = Left parseError
checkExtDeclaration (Right (Spec [])) (Problem ptype) = Right . Spec $ [Problem ptype]
checkExtDeclaration (Right (Spec [])) (CType ctype) = Right . Spec $ [CType ctype]
checkExtDeclaration (Right (Spec [])) (Var vs) = Right . Spec $ [Var vs]
checkExtDeclaration (Right (Spec [])) (Context rmap) = Right . Spec $ [Context rmap]
checkExtDeclaration (Right (Spec [])) (Rules rs) = Right . Spec $ [Rules rs]
checkExtDeclaration (Right (Spec [Problem ptype])) (CType ctype) = Right . Spec $ [CType ctype, Problem ptype]
checkExtDeclaration (Right (Spec [Problem ptype])) (Var vs) = Right . Spec $ [Var vs, Problem ptype]
checkExtDeclaration (Right (Spec [Problem ptype])) (Context rmap) = Right . Spec $ [Context rmap, Problem ptype]
checkExtDeclaration (Right (Spec [Problem ptype])) (Rules rs) = Right . Spec $ [Rules rs, Problem ptype]
checkExtDeclaration (Right (Spec [Problem ptype])) (Comment c) = Right . Spec $ [Comment c, Problem ptype]
checkExtDeclaration (Right (Spec (Comment c:rest))) (CType ctype) = Right . Spec $ (CType ctype:Comment c:rest)
checkExtDeclaration (Right (Spec (Comment c:rest))) (Var vs) = Right . Spec $ (Var vs:Comment c:rest)
checkExtDeclaration (Right (Spec (Comment c:rest))) (Context rmap) = Right . Spec $ (Context rmap:Comment c:rest)
checkExtDeclaration (Right (Spec (Comment c:rest))) (Rules rs) = Right . Spec $ (Rules rs:Comment c:rest)
checkExtDeclaration (Right (Spec (CType ctype:rest))) (Var vs) = Right . Spec $ (Var vs:CType ctype:rest)
checkExtDeclaration (Right (Spec (CType ctype:rest))) (Context rmap) = Right . Spec $ (Context rmap:CType ctype:rest)
checkExtDeclaration (Right (Spec (CType ctype:rest))) (Rules rs) = Right . Spec $ (Rules rs:CType ctype:rest)
checkExtDeclaration (Right (Spec (Var vs:rest))) (Context rmap) = Right . Spec $ (Context rmap:Var vs:rest)
checkExtDeclaration (Right (Spec (Var vs:rest))) (FOTheory f) = Right . Spec $ (FOTheory f:Var vs:rest)
checkExtDeclaration (Right (Spec (Context rmap:rest))) (FOTheory f) = Right . Spec $ (FOTheory f:Context rmap:rest)
checkExtDeclaration (Right (Spec (Var vs:rest))) (Predicates ps) = Right . Spec $ (Predicates ps:Var vs:rest)
checkExtDeclaration (Right (Spec (Context rmap:rest))) (Predicates ps) = Right . Spec $ (Predicates ps:Context rmap:rest)
checkExtDeclaration (Right (Spec (FOTheory f:rest))) (Predicates ps) = Right . Spec $ (Predicates ps:FOTheory f:rest)
checkExtDeclaration (Right (Spec (Var vs:rest))) (Equations eqs) = Right . Spec $ (Equations eqs:Var vs:rest)
checkExtDeclaration (Right (Spec (Context rmap:rest))) (Equations eqs) = Right . Spec $ (Equations eqs:Context rmap:rest)
checkExtDeclaration (Right (Spec (FOTheory f:rest))) (Equations eqs) = Right . Spec $ (Equations eqs:FOTheory f:rest)
checkExtDeclaration (Right (Spec (Predicates ps:rest))) (Equations eqs) = Right . Spec $ (Equations eqs:Predicates ps:rest)
checkExtDeclaration (Right (Spec (Var vs:rest))) (Rules rs) = Right . Spec $ (Rules rs:Var vs:rest)
checkExtDeclaration (Right (Spec (Context rmap:rest))) (Rules rs) = Right . Spec $ (Rules rs:Context rmap:rest)
checkExtDeclaration (Right (Spec (Equations eqs:rest))) (Rules rs) = Right . Spec $ (Rules rs:Equations eqs:rest)
checkExtDeclaration (Right (Spec (FOTheory f:rest))) (Rules rs) = Right . Spec $ (Rules rs:FOTheory f:rest)
checkExtDeclaration (Right (Spec (Predicates ps:rest))) (Rules rs) = Right . Spec $ (Rules rs:Predicates ps:rest)
checkExtDeclaration (Right (Spec (Rules rs:rest))) (Var vs) = Right . Spec $ (Var vs:Rules rs:rest)
checkExtDeclaration (Right (Spec (Rules rs:rest))) (Conditions cs) = Right . Spec $ (Conditions cs:Rules rs:rest)
checkExtDeclaration (Right (Spec (Rules rs:rest))) (Comment c) = Right . Spec $ (Comment c:Rules rs:rest)
checkExtDeclaration (Right (Spec (Var vs:Rules rs:rest))) (Conditions cs) = Right . Spec $ (Conditions cs:CVar vs:Rules rs:rest)
checkExtDeclaration (Right (Spec (Conditions cs:rest))) (Comment c) = Right . Spec $ (Comment c:Conditions cs:rest)
checkExtDeclaration _ (CType _) = Left $ newErrorMessage (UnExpect "CONDITIONTYPE block") (newPos "" 0 0)
checkExtDeclaration _ (Var _) = Left $ newErrorMessage (UnExpect "VAR block") (newPos "" 0 0)
checkExtDeclaration _ (Context _) = Left $ newErrorMessage (UnExpect "REPLACEMENT-MAP block") (newPos "" 0 0)
checkExtDeclaration _ (Rules _) = Left $ newErrorMessage (UnExpect "RULES block") (newPos "" 0 0)
checkExtDeclaration _ (Comment _) = Left $ newErrorMessage (UnExpect $ "COMMENT block") (newPos "" 0 0)
checkExtDeclaration _ (Problem _) = Left $ newErrorMessage (UnExpect "PROBLEM block") (newPos "" 0 0)
checkExtDeclaration _ (Conditions _) = Left $ newErrorMessage (UnExpect "CONDITION block") (newPos "" 0 0)

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
                                         ; if (length . nub . map fst $ rmap) == length rmap then
                                             do { put $ myTRS { trsRMap = rmap
                                                              , trsType 
                                                                  = case trsType myTRS of
                                                                      TRSStandard -> TRSContextSensitive
                                                                      TRSConditional typ -> TRSContextSensitiveConditional typ
                                                              }
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
checkWellFormed (Comment _:rest) = checkWellFormed rest

-- | Extracts the signature and checks if the rules are well-formed wrt that
-- signature. Precondition: Declarations are in order.
checkExtWellFormed :: [Decl] -> State (TRS, Maybe INF) (Either ParseError (TRS, Maybe INF))
checkExtWellFormed [] = do { (myTRS,myINF) <- get 
                        ; return . Right $ (myTRS,myINF)}
checkExtWellFormed (Problem INFEASIBILITY:rest) = do { (myTRS,myINF) <- get 
                                                 ; put $ (myTRS,Just $ INF S.empty [])
                                                 ; checkExtWellFormed rest
                                                 }
checkExtWellFormed (CType SemiEquational:rest) = do { (myTRS,myINF) <- get 
                                                 ; put $ (myTRS { trsType = TRSConditional SemiEquational },myINF)
                                                 ; checkExtWellFormed rest
                                                 }
checkExtWellFormed (CType Join:rest) = do { (myTRS,myINF) <- get 
                                       ; put $ (myTRS { trsType = TRSConditional Join },myINF)
                                       ; checkExtWellFormed rest
                                       }
checkExtWellFormed (CType Oriented:rest) = do { (myTRS,myINF) <- get 
                                           ; put $ (myTRS { trsType = TRSConditional Oriented },myINF)
                                           ; checkExtWellFormed rest
                                           }
checkExtWellFormed (Var vs:rest) = do { (myTRS,myINF) <- get 
                                   ; put $ (myTRS { trsVariables = S.fromList vs },myINF)
                                   ; checkExtWellFormed rest
                                   }
checkExtWellFormed (Context rmap:rest) = do { (myTRS,myINF) <- get 
                                         ; if (length . nub . map fst $ rmap) == length rmap then
                                             do { put $ (myTRS { trsRMap = rmap
                                                               , trsType 
                                                                   = case trsType myTRS of
                                                                       TRSStandard -> TRSContextSensitive
                                                                       TRSConditional typ -> TRSContextSensitiveConditional typ
                                                               },myINF)
                                                ; checkExtWellFormed rest
                                                }
                                           else 
                                             return . Left $ newErrorMessage (UnExpect $ "duplicated symbols in replacement map declaration") (newPos "" 0 0)
                                         }
checkExtWellFormed (Rules rs:rest) = do { (myTRS,myINF) <- get 
                                     ; let (result,myNewTRS) = runState (checkRules rs) myTRS
                                     ; case result of
                                         Left parseError -> return . Left $ parseError
                                         Right _ -> do { (myTRS,myINF) <- get
                                                       ; put $ (myNewTRS {trsRules = rs},myINF)
                                                       ; checkExtWellFormed rest
                                                       }
                                     }
checkExtWellFormed (Equations eqs:rest) = do { (myTRS,myINF) <- get 
                                             ; let (result,myNewTRS) = runState (checkEquations eqs) myTRS
                                             ; case result of
                                                 Left parseError -> return . Left $ parseError
                                                 Right _ -> do { (myTRS,myINF) <- get
                                                               ; put $ (myNewTRS {trsEquations = eqs},myINF)
                                                               ; checkExtWellFormed rest
                                                               }
                                             }
checkExtWellFormed (Predicates ps:rest) = do { (myTRS,myINF) <- get 
                                             ; let (result,myNewTRS) = runState (checkPredicates ps) myTRS
                                             ; case result of
                                                 Left parseError -> return . Left $ parseError
                                                 Right _ -> do { (myTRS,myINF) <- get
                                                               ; put $ (myNewTRS {trsPredicates = ps},myINF)
                                                               ; checkExtWellFormed rest
                                                               }
                                             }
checkExtWellFormed (FOTheory f:rest) = do { (myTRS,myINF) <- get 
                                             ; let (result,myNewTRS) = runState (checkFormula f) myTRS
                                             ; case result of
                                                 Left parseError -> return . Left $ parseError
                                                 Right _ -> do { (myTRS,myINF) <- get
                                                               ; put $ (myNewTRS {trsFOTheory = f},myINF)
                                                               ; checkExtWellFormed rest
                                                               }
                                             }
checkExtWellFormed (CVar vs:rest) = do { (myTRS,maybeMyINF) <- get
                                       ; case maybeMyINF of
                                          Just myINF -> do { put $ (myTRS,Just $ myINF { trsCondVariables = S.fromList vs })
                                                           ; checkExtWellFormed rest
                                                           }
                                          Nothing -> return . Left $ newErrorMessage (UnExpect $ "condition variables are only allowed in infeasibility problems") (newPos "" 0 0)
                                       }                                     
checkExtWellFormed (Conditions cs:rest) = do { (myTRS,maybeMyINF) <- get
                                             ; case maybeMyINF of
                                                  Just myINF -> do { let result = evalState (checkConditions cs) (myTRS, myINF)
                                                                   ; case result of
                                                                       Left parseError -> return . Left $ parseError
                                                                       Right _ -> do { (myTRS,maybeMyINF) <- get
                                                                                     ; case maybeMyINF of
                                                                                         Just myINF -> do { put $ (myTRS,Just myINF  {trsConditions = cs})
                                                                                                           ; checkExtWellFormed rest
                                                                                                           }
                                                                                         Nothing -> return . Left $ newErrorMessage (UnExpect $ "conditions are only allowed in infeasibility problems") (newPos "" 0 0)
                                                                                     }
                                                                    }
                                                  Nothing -> return . Left $ newErrorMessage (UnExpect $ "conditions are only allowed in infeasibility problems") (newPos "" 0 0)
                                             }

checkExtWellFormed (Comment _:rest) = checkExtWellFormed rest
checkExtWellFormed a = error $ show a

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
                       ; let vs = trsVariables myTRS
                       ; if nonVarLHS vs r then -- lhs is non-variable 
                           if (isConditional myTRS) || (not . isCRule $ r) then -- conditional rules not allowed in TRSs 
                             if isConditional myTRS || (not . hasExtraVars vs $ r) then -- extra variables not allowed in non-conditional rules
                               do { result <- checkTerms . getTerms $ r 
                                  ; case result of
                                      Left parseError -> return . Left $ parseError 
                                      Right _ -> checkRules rs
                                  }
                             else
                               return . Left $ newErrorMessage (UnExpect $ "extra variables in the rule " ++ (show r)) (newPos "" 0 0)
                           else
                             return . Left $ newErrorMessage (UnExpect $ "conditional rule: " ++ (show r) ++ ", CONDITIONTYPE is missing") (newPos "" 0 0)
                         else
                           return . Left $ newErrorMessage (UnExpect $ "variable in the left-hand side of the rule " ++ (show r)) (newPos "" 0 0)
                       }

-- | Checks if the equations are well-formed wrt the extracted signature
checkEquations :: [Equation] -> State TRS (Either ParseError ())
checkEquations [] = do { myTRS <- get
                         -- first, we extract the arity of symbols, then we check RMap 
                       ; case trsType myTRS of
                           TRSContextSensitive -> checkRMap . trsRMap $ myTRS
                           TRSContextSensitiveConditional _ -> checkRMap . trsRMap $ myTRS
                           _ -> return . Right $ ()
                       }
checkEquations (eq:eqs) = do { myTRS <- get
                             ; if (isConditional myTRS) || (not . isCEquation $ eq) then -- conditional equations not allowed in TRSs 
                                 do { result <- checkTerms . getEqTerms $ eq 
                                    ; case result of
                                        Left parseError -> return . Left $ parseError 
                                        Right _ -> checkEquations eqs
                                    }
                               else
                                 return . Left $ newErrorMessage (UnExpect $ "conditional equation: " ++ (show eq) ++ ", CONDITIONTYPE is missing") (newPos "" 0 0)
                             }

-- | Checks if the predicates are well-formed wrt the extracted signature
checkPredicates :: [Predicate] -> State TRS (Either ParseError ())
checkPredicates [] = do { myTRS <- get
                          -- first, we extract the arity of symbols, then we check RMap 
                        ; case trsType myTRS of
                            TRSContextSensitive -> checkRMap . trsRMap $ myTRS
                            TRSContextSensitiveConditional _ -> checkRMap . trsRMap $ myTRS
                            _ -> return . Right $ ()
                        }
checkPredicates (p:ps) = do { myTRS <- get
                            ; do { result <- checkTerms . getPredTerms $ p 
                                  ; case result of
                                      Left parseError -> return . Left $ parseError 
                                      Right _ -> checkPredicates ps
                                  }
                            }

-- | Checks if the formula is well-formed wrt the extracted signature
checkFormula :: Formula -> State TRS (Either ParseError ())
checkFormula f = do { myTRS <- get
                    ; do { result <- checkTerms . getFormulaTerms $ f 
                        ; case result of
                            Left parseError -> return . Left $ parseError 
                            Right _ -> case trsType myTRS of
                                          TRSContextSensitive -> checkRMap . trsRMap $ myTRS
                                          TRSContextSensitiveConditional _ -> checkRMap . trsRMap $ myTRS
                                          _ -> return . Right $ ()
                        }
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
                            ; case (S.member id cvars, M.lookup id funcs) of 
                               (False, Nothing) -> return . Left $ newErrorMessage (UnExpect $ "function symbol " ++ id ++ " not in signature") (newPos "" 0 0)
                               (False, Just len) -> if (arglen == len) then 
                                                      checkCTerms terms 
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
checkRMap :: [(TId, [Int])] -> State TRS (Either ParseError ())
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
