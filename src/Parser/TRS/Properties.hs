{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.TRS.Properties
-- Copyright   :  (c) muterm development team
-- License     :  see LICENSE
--
-- Maintainer  :  raguti@upv.es
-- Stability   :  unstable
-- Portability :  non-portable
--
-- This module manage the property checkings for TRSs
--
-----------------------------------------------------------------------------

module Parser.TRS.Properties (

-- * Exported data

Property (..)

-- * Exported functions

, nonVarLHS, nonVarLHS', isCRule, hasExtraVars, hasExtraVars'
, isCanonical, isConditional, isSRS, isLeftLinear
, isRightGround, isGround, isOriented, isJoin, isSemiEquational
, isNormal, isOneCTRS, isTwoCTRS, isThreeCTRS, isTRSConditional


) where

import Parser.TRS.Grammar as G

import Data.Map as M (Map, empty, lookup, insert, fromList, insertWith, elems)
import Data.Set as S (Set, empty, fromList, member, unions, insert, (\\)
  , null, union, isSubsetOf)
import Data.MultiSet as MS (occur, findMax, null)
import Data.List (sort, nub)
import Control.Monad.State (State, evalState, get, put)
import Data.List (intersperse)
import Data.Maybe (isNothing)
import Control.Monad.State (MonadState (..), execState)

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

-- | Property
data Property = None
              | Canonical
              | SRS
              | LeftLinear
              | RightGround
              | Ground 
              | Oriented
              | Join
              | SemiEquational
              | Normal
              | OneCTRS
              | TwoCTRS
              | ThreeCTRS

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | checks if the lhs is non-variable
nonVarLHS :: Set TId -> Rule -> Bool
nonVarLHS vs (Rule ((T idt _) :-> r) eqs) = not . member idt $ vs 

-- | checks if the lhs is non-variable
nonVarLHS' :: Map TId Int -> Rule -> Bool
nonVarLHS' fs (Rule ((T idt _) :-> r) eqs) = not . isNothing . M.lookup idt $ fs

-- | checks if the rule is conditional
isCRule :: Rule -> Bool
isCRule (Rule _ []) = False 
isCRule _ = True 

-- | checks if the non-conditional rule has extra variables
hasExtraVars :: Set TId -> Rule -> Bool
hasExtraVars vs (Rule (l :-> r) []) = not . S.null $ getVars vs r \\ getVars vs l
hasExtraVars _ _ = error $ "Error: hasExtraVars only applies to non-conditional rules"

-- | checks if the non-conditional rule has extra variables
hasExtraVars' :: Map TId Int -> Rule -> Bool
hasExtraVars' fs (Rule (l :-> r) []) = not . S.null $ getVars' fs r \\ getVars' fs l
hasExtraVars' _ _ = error $ "Error: hasExtraVars only applies to non-conditional rules"

-- | isConditional checks if trsType is TRSConditional or TRSContextSensitiveConditional
isConditional :: TRS -> Bool
isConditional trs = case trsType trs of 
                      TRSConditional _ -> True 
                      TRSContextSensitiveConditional _ -> True
                      _ -> False

-- | isTRSConditional checks if trsType is TRSConditional
isTRSConditional :: TRSType -> Bool
isTRSConditional ty = case ty of 
                        TRSConditional _ -> True 
                        _ -> False

-- | Checks if the replacement map of the TRS is canonical
isCanonical :: TRS -> Bool
isCanonical trs =
  let repmap = execState (extractCanonicalRepMap trs) (M.empty)
  in and . map (checkCanonical trs repmap) . trsRMap $ trs

-- | Extract canonical replacement map
extractCanonicalRepMap :: (MonadState (Map TId (Set Int)) m) => TRS -> m ()
extractCanonicalRepMap trs = do a <- sequence . Prelude.map (extractCanonicalRepMapRule trs) . trsRules $ trs
                                return ()

-- | Extract canonical replacement map from rule
extractCanonicalRepMapRule :: (MonadState (Map TId (Set Int)) m) => TRS -> Rule -> m ()
extractCanonicalRepMapRule trs (Rule (l :-> _) _) = extractCanonicalRepMapTerm trs l

-- | Extract canonical replacement map from term
extractCanonicalRepMapTerm :: (MonadState (Map TId (Set Int)) m) => TRS -> Term -> m ()
extractCanonicalRepMapTerm trs (T f tt) 
  = do a <- sequence . map (extractCanonicalRepMapTerm trs) . filter (not . isVTerm) $ tt
       mapping <- get
       let mapping' = M.insertWith (S.union) f compf' mapping
       put mapping'
       return ()
    where
      sig = trsSignature trs
      arity = case M.lookup f sig of 
                Just ar -> ar 
                Nothing -> error $ "Symbol " ++ f ++ " does not appear in the Signature.\n" 
      isVTerm (T v _) = isNothing $ M.lookup v sig  
      compf' = S.fromList [i | i <- [1..arity], not . isVTerm $ (tt!!(i - 1))]

-- | Checks if the replamcement map of the symbol is canonical
checkCanonical :: TRS -> Map TId (Set Int) -> (TId,[Int]) -> Bool
checkCanonical trs rMap (f,rMapL) 
  = case M.lookup f rMap of
      Nothing -> let ar = trsSignature trs
                 in case M.lookup f ar of 
                      Nothing -> error $ "Symbol " ++ f ++ " does not appear in the Signature.\n"
                      Just arity -> (S.fromList [1..arity]) == (S.fromList rMapL)
      Just rMapS -> rMapS == (S.fromList rMapL)

-- | Checks if the all the symbols have arity 1
isSRS :: TRS -> Bool
isSRS trs = Prelude.null . Prelude.filter (/=1) . M.elems . trsSignature $ trs

-- | Checks if the TRS is left-linear
isLeftLinear :: TRS -> Bool
isLeftLinear trs = Prelude.null . Prelude.filter (>1) . Prelude.map getMaxOccur . trsRules $ trs
  where getMaxOccur rl = let vs = getMSVars' (trsSignature trs) . lhs $ rl 
                         in if MS.null vs then
                              0
                            else 
                              occur (findMax vs) vs

-- | Checks if the TRS is rigth-ground
isRightGround :: TRS -> Bool
isRightGround trs = and . Prelude.map (isGroundTerm trs . rhs) . trsRules $ trs

-- | Checks if the TRS is ground
isGround :: TRS -> Bool
isGround trs = and . Prelude.map (isGroundRule trs) . trsRules $ trs

-- | Checks if the CTRS conditions are oriented
isOriented :: TRS -> Bool
isOriented trs = case trsType trs of 
                   TRSConditional G.Oriented -> True
                   TRSContextSensitiveConditional G.Oriented -> True
                   _ -> False

-- | Checks if the CTRS conditions are join
isJoin :: TRS -> Bool
isJoin trs = case trsType trs of 
               TRSConditional G.Join -> True
               TRSContextSensitiveConditional G.Join -> True
               _ -> False

-- | Checks if the CTRS conditions are semi-equational
isSemiEquational :: TRS -> Bool
isSemiEquational trs = case trsType trs of 
                         TRSConditional G.SemiEquational -> True
                         TRSContextSensitiveConditional G.SemiEquational -> True
                         _ -> False

-- | Checks if the CTRS is normal
isNormal :: TRS -> Bool
isNormal trs = and . Prelude.map (isNormalTerm trs . eqrhs)  . Prelude.concatMap conds . trsRules $ trs

-- | Checks if it is a 1-CTRS
isOneCTRS :: TRS -> Bool
isOneCTRS trs = let sig = trsSignature trs
                    rls = trsRules trs 
                    ls = Prelude.map lhs rls
                    rs = Prelude.map rhs rls
                    cs = Prelude.concatMap (\c -> [eqlhs c, eqrhs c]) . Prelude.concatMap conds $ rls
                    lsVs = S.unions . Prelude.map (getVars' sig) $ ls
                    rsVs = S.unions . Prelude.map (getVars' sig) $ rs
                    csVs = S.unions . Prelude.map (getVars' sig) $ cs
                in (S.union rsVs csVs) `isSubsetOf` lsVs
    

-- | Checks if it is a 2-CTRS
isTwoCTRS :: TRS -> Bool
isTwoCTRS trs = let sig = trsSignature trs
                    rls = trsRules trs 
                    ls = Prelude.map lhs rls
                    rs = Prelude.map rhs rls
                    lsVs = S.unions . Prelude.map (getVars' sig) $ ls
                    rsVs = S.unions . Prelude.map (getVars' sig) $ rs
                in rsVs `isSubsetOf` lsVs

-- | Checks if it is a 3-CTRS
isThreeCTRS :: TRS -> Bool
isThreeCTRS trs = let sig = trsSignature trs
                      rls = trsRules trs 
                      ls = Prelude.map lhs rls
                      rs = Prelude.map rhs rls
                      cs = Prelude.concatMap (\c -> [eqlhs c, eqrhs c]) . Prelude.concatMap conds $ rls
                      lsVs = S.unions . Prelude.map (getVars' sig) $ ls
                      rsVs = S.unions . Prelude.map (getVars' sig) $ rs
                      csVs = S.unions . Prelude.map (getVars' sig) $ cs
                  in rsVs `isSubsetOf` (S.union lsVs csVs)

-- | Checks if the term is ground
isGroundTerm :: TRS -> Term -> Bool
isGroundTerm trs t = let vs = getVars' (trsSignature trs) t
                     in S.null vs

-- | Checks if the non-conditional rule is ground
isGroundRule :: TRS -> Rule -> Bool
isGroundRule trs rl = let lvs = getVars' (trsSignature trs) . lhs $ rl
                          rvs = getVars' (trsSignature trs) . rhs $ rl
                      in S.null lvs && S.null rvs

-- | Checks if the term is a ground normal form wrt R_u
isNormalTerm :: TRS -> Term -> Bool 
isNormalTerm trs t = isGroundTerm trs t && (not . or . Prelude.map (\s -> matches trs s t) $ ts)
  where ts = Prelude.map lhs . trsRules $ trs
