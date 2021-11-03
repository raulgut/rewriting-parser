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
-- This module manage the parser for TRSs and SRSs in the COPS format.
--
-----------------------------------------------------------------------------

module Parser.COPS.Parser (

-- * Exported functions

parseCOPS

)  where

import Text.ParserCombinators.Parsec (parse, Parser (..), ParseError)
import Parser.COPS.TRS.Parser (trsParser)
import Parser.COPS.SRS.Parser (srsParser)
import Parser.COPS.TRS.Grammar as TRSGrammar (Spec(..), Decl(..), Id, Rule(..)
  , Term(..), SimpleRule(..), Equation(..), CondType(..), Strategy(..))
import Parser.COPS.SRS.Grammar as SRSGrammar (Spec(..))

import Control.Monad.State (MonadState, runState)
import Data.List (nub, find)
import Data.Set as S (Set (..), empty, fromList, union, unions, singleton, difference, elems, toAscList, intersection, filter)
import Data.Maybe (fromMaybe)
import Data.Map as M (Map (..), fromAscList, lookup, map, fromList, null)
import Data.Graph.Inductive as G (Gr,empty, mkUGraph)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | Parse a COPS problem and return an MProblem
parseCOPS :: String -> Either ParseError (MProblem info)
parseCOPS = transform . eitherCompleteVars . parseTrs

-- | ensures there are no empty var sets and transform condition vars into cvars
eitherCompleteVars :: Either ParseError TRSGrammar.Spec
          -> Either ParseError TRSGrammar.Spec
eitherCompleteVars (Left l)
    = Left l
eitherCompleteVars (Right (TRSGrammar.Spec decls)) =
  Right . TRSGrammar.Spec . completeVars $ decls

-- | ensures there are no empty var sets and transform condition vars into cvars
completeVars :: [TRSGrammar.Decl] -> [TRSGrammar.Decl]
completeVars [] = []
completeVars ((TRSGrammar.Var vv):(TRSGrammar.Rules rr):decls) = (TRSGrammar.Var ("vNonEmpty":vv)):(TRSGrammar.Rules rr):completeVars(decls)
completeVars ((TRSGrammar.Rules rr):decls) = (TRSGrammar.Var ["vNonEmpty"]):(TRSGrammar.Rules rr):completeVars(decls)
completeVars (d:decls) = d:completeVars(decls)

-- | Parse a string rewriting system of the TPDB. Actually not used.
parseSrs :: String -> Either ParseError SRSGrammar.Spec
parseSrs s = doParse s srsParser

-- | Parse a term rewriting system of the TPDB.
parseTrs :: String -> Either ParseError TRSGrammar.Spec
parseTrs s = doParse s trsParser

-- | Parses the system an returns the parsing error or the succesful
-- parsed system.
doParse :: String -> Parser a -> Either ParseError a
doParse s p = parse p "" s

-- | We obtain the sorts set with unique numeric identifiers.
transformSorts :: MonadState Int m => [(Id,[ExtraPropertySort])] -> m [Sort]
transformSorts insorts
    = do let outsorts = sequence . Prelude.map setCounter $ insorts
         outsorts
      where setCounter (s',props') = do counter <- getInt
                                        return (toSort counter s' props')

-- | Get sort from its name
getSort :: HasName s => [s] -> String -> s
getSort sorts s = case (find (\x -> getName x == s) $ sorts) of
                    Just existingsort -> existingsort
                    Nothing -> error ("Error (TPDB.Parser): Sort " ++ s ++ "not found")

-- | We obtain the variables set with unique numeric identifiers.
transformVars :: MonadState Int m => [Sort] -> [(String,String)] -> m [IdVOS Sort IdVTRS]
transformVars sorts invars
    = do let outvars = sequence . Prelude.map setCounter $ invars
         outvars
      where setCounter (v', sort') = do counter <- getInt
                                        return $ toVOS (toVTRS counter (Just v')) (getSort sorts sort')

-- | We obtain the TRS function symbols set with unique numeric
-- identifiers.
transformSymbols :: MonadState Int m =>
                    [Sort]
                 -> Map Id [Int]
                 -> [ExtraProperty]
                 -> Set (Id, [[String]])
                 -> m [IdFOS Sort (IdFCS IdFTRS)]
transformSymbols sorts rmap props insymbols
    = do let outsymbols = sequence . Prelude.map setCounter . elems $ insymbols
         outsymbols
      where mu f ss = case M.lookup f rmap of
                        Nothing -> [1..(length . finsorts . head $ ss)]
                        Just mu' -> mu'
            setCounter (name', fsorts)
                = do counter <- getInt
                     let fsorts' = Prelude.map ((\fs -> toFSort fs props) . Prelude.map (getSort sorts)) fsorts
                     return $ toFTOS (toFTCS (toFTRS counter name' (length . finsorts . head $ fsorts')) (mu name' fsorts')) fsorts'

-- | Transforms the TPDB specification into a MProblem. If a parsing
-- error has occurred, then it is returned.
transform :: Either ParseError TRSGrammar.Spec
          -> Either ParseError (MProblem info)
transform (Left l)
    = Left l
transform (Right (TRSGrammar.Spec decls)) =
  Right $!
    if isCond crules then
      MCRewriting $ mkProblem (CRewriting (Problems.M,Problems.A)) (InConfCSCTRS crewtrsList)
    else
      MRewriting $ mkProblem (Rewriting (Problems.M,Problems.A)) (InConfCSTRS rewtrsList) -- MCSRew  rewtrs
  where
    (sorts', sortscount)
      = runState (transformSorts [("S",[])]) 0
    allVars
      = nub . concat $ [("vNonEmpty","S")]:[Prelude.map (\x -> (x,"S")) vv | TRSGrammar.Var vv <- decls] ++ [Prelude.map (\x -> (x,"S")) vv | TRSGrammar.CVar vv <- decls]
        -- with nub we avoid duplicating variables
    (vars, varcount)
      = runState (transformVars sorts' allVars) 0
    setvars
      = S.fromList vars
    ctypes
      = [ct | TRSGrammar.CType ct <- decls]
    ctype
      = if Prelude.null ctypes then
          Nothing
        else
          case head ctypes of
            t -> Just t
    crules
      = concatMap (crewruleT ctype allVars) decls
    rules
      = Prelude.map rule crules
    (allDefined, allRSymbols)
      = extractCRsArity crules
    rels = S.fromList [("_->R_",[["S","S","S"]]), ("_->R*_",[["S","S","S"]])]
    nonEmptySymbols = S.fromList [("fSNonEmpty",[["S"]])]
    allSymbols = allRSymbols `S.union` nonEmptySymbols
    isCond rules = not . and . Prelude.map Prelude.null $ [c | (r :=> c) <- rules]
    -- strategy
    strategies = [strat | Strategy strat <- decls]
    isInn = not . Prelude.null $ [InnerMost | InnerMost <- strategies]
    isOut = not . Prelude.null $ [OuterMost | OuterMost <- strategies]
    rmap
        = M.fromList $ ("fSNonEmpty",[]):(concat [rmap' | Context rmap' <- strategies])
    -- CS-(C)TRS
    (trsrelations, symbolscount)
      = runState (transformSymbols sorts' rmap [MP (Just 2)] rels) 0
    (trsdefined, symbolscount')
      = runState (transformSymbols sorts' rmap [] allDefined) symbolscount
    (trsconstructors, symbolscount'')
      = runState (transformSymbols sorts' rmap [] (allSymbols
                                                   `difference`
                                                   allDefined)) symbolscount'
    settrsrelations = S.filter ((\x -> (x == "_->R_") || (x == "_->R*_")) . getName) . S.fromList $ trsrelations
    settrsdefined = S.fromList trsdefined
    settrsconstructors = S.fromList trsconstructors
    trsSorts = S.fromList sorts'
    -- rewriting rules
    trsrules = setRsIds (settrsdefined `union` settrsconstructors) setvars rules
    -- conditional rewriting rules
    ctrsrules  = setCRsIds (settrsdefined `union` settrsconstructors) setvars crules
    -- resulting TRSs
    rewtrsList = OSTRS { ostrsprev = TRS { trsSignature
                                                 = Signature { defined      = settrsdefined
                                                             , constructors = settrsconstructors
                                                             , relations    = settrsrelations
                                                             }
                                             , trsVariables = setvars
                                             , trsRules = trsrules
                                             , trsLabel = RULES
                                             }
                           , ossignature = OSSignature { ossorts = trsSorts
                                                       , osprec  = mkUGraph [i | i <- [0..(length . S.elems $ trsSorts)-1]] [] :: Gr () ()
                                                       }
                           }
    crewtrsList = OSTRS { ostrsprev = CTRS { ctrsSignature
                                                 = Signature { defined      = settrsdefined
                                                             , constructors = settrsconstructors
                                                             , relations    = settrsrelations
                                                             }
                                             , ctrsVariables = setvars
                                             , ctrsRules = ctrsrules
                                             , ctrsLabel = RULES
                                             }
                           , ossignature = OSSignature { ossorts = trsSorts
                                                       , osprec  = mkUGraph [i | i <- [0..(length . S.elems $ trsSorts)-1]] [] :: Gr () ()
                                                       }
                           }

-- Extract arity functions: extractRsArity, extractRArity and
-- extractTArity

-- | Extract defined and all symbols from a set of rules with its
-- arity.
extractRsArity :: [Rs.Rule (Ts.Term Id Id)]
              -> (Set (Id,[[String]]), Set (Id,[[String]]))
extractRsArity
    = foldl tupleunion (S.empty, S.empty) . Prelude.map extractRArity
    where tupleunion x y = (fst x `union` fst y, snd x `union` snd y)

-- | Extract defined and all symbols from a rule with its arity.
extractRArity :: Rs.Rule (Ts.Term Id Id)
              -> (Set (Id,[[String]]), Set (Id,[[String]]))
extractRArity ((F id tt) Rs.:-> r)
    = ( defined
      , defined
        `union`
        (unions . Prelude.map extractTArity $ tt)
        `union`
        extractTArity r
      )
    where defined = singleton (id, [["S" | i <- [0..length tt]]])

-- | Extract all symbols from a term with its arity.
extractTArity :: Ts.Term Id Id
              -> Set (Id,[[String]])
extractTArity (F id tt)
    = singleton (id,[["S" | i <- [0..length tt]]])
      `union`
      (unions . Prelude.map extractTArity $ tt)
extractTArity _
    = S.empty

-- | Extract defined and all symbols from a set of conditional rules
-- with its arity.
extractCRsArity :: [Rs.CRule (Ts.Term Id Id)]
              -> (Set (Id,[[String]]), Set (Id,[[String]]))
extractCRsArity
    = foldl tupleunion (S.empty, S.empty) . Prelude.map extractCRArity
    where tupleunion x y = (fst x `union` fst y, snd x `union` snd y)

-- | Extract defined and all symbols from a conditional rule with its arity.
extractCRArity :: Rs.CRule (Ts.Term Id Id)
              -> (Set (Id,[[String]]), Set (Id,[[String]]))
extractCRArity (((F id tt) Rs.:-> r) :=> cs)
    = ( defined
      , defined
        `union`
        (unions . Prelude.map extractTArity $ tt)
        `union`
        extractTArity r
        `union`
        extractCondsArity cs
      )
    where defined = singleton (id, [["S" | i <- [0..length tt]]])

-- | Extract defined and all symbols from a set of conditions with its
-- arity.
extractCondsArity :: [Rs.Cond (Ts.Term Id Id)]
              -> Set (Id,[[String]])
extractCondsArity
    = foldl union S.empty . Prelude.map extractCondArity

-- | Extract defined and all symbols from a condition with its arity.
extractCondArity :: Rs.Cond (Ts.Term Id Id)
              -> Set (Id,[[String]])
extractCondArity (l Rs.:->* r)
    = extractTArity l `union` extractTArity r
extractCondArity (l Rs.:~> r)
    = extractTArity l `union` extractTArity r
extractCondArity (l Rs.:->+ r)
    = extractTArity l `union` extractTArity r
extractCondArity (l Rs.:->*<- r)
    = extractTArity l `union` extractTArity r
extractCondArity (l Rs.:<--> r)
    = extractTArity l `union` extractTArity r
extractCondArity (l Rs.:<-->* r)
    = extractTArity l `union` extractTArity r
extractCondArity (l Rs.:\->* r)
    = extractTArity l `union` extractTArity r
extractCondArity (l Rs.:\-> r)
    = extractTArity l `union` extractTArity r
extractCondArity (l Rs.:\->+ r)
    = extractTArity l `union` extractTArity r
extractCondArity (l Rs.:\->*<-/ r)
    = extractTArity l `union` extractTArity r
extractCondArity (l Rs.:<-/\-> r)
    = extractTArity l `union` extractTArity r
extractCondArity (l Rs.:<-/\->* r)
    = extractTArity l `union` extractTArity r
extractCondArity (l Rs.:|>= r)
    = extractTArity l `union` extractTArity r
extractCondArity (l Rs.:->= r)
    = extractTArity l `union` extractTArity r
extractCondArity (l Rs.:\->= r)
    = extractTArity l `union` extractTArity r

-- Set variables in rules: rewruleT, pairsT and termT

-- | We get the rules from the system. Since when we get the
-- specification do not know which symbols are variables and which are
-- constant symbols, we have to re-process the rules to distinguish
-- the variables into the rules.
crewruleT :: Maybe CondType
         -> [(Id,String)]
         -> TRSGrammar.Decl
         -> [Rs.CRule (Ts.Term Id Id)]
crewruleT ctype vars (TRSGrammar.Rules rr)
    = extractRules ctype vars rr
crewruleT _ _ _ = []

-- | Extract the rules with variables
extractRules :: Maybe CondType
             -> [(Id,String)]
             -> [TRSGrammar.Rule]
             -> [Rs.CRule (Ts.Term Id Id)]
extractRules ctype vars rr
    = Prelude.map fromRule rr
      where fromRule (TRSGrammar.Rule (t1 TRSGrammar.:->  t2) cond)
                = ((termT vars t1) Rs.:-> (termT vars t2)) Rs.:=> (Prelude.map (fromCond ctype vars) cond)
            fromRule (TRSGrammar.Rule (t1 TRSGrammar.:->= t2) _)
                = ((termT vars t1) Rs.:-> (termT vars t2)) Rs.:=> []

-- | Extract conditions
fromCond :: Maybe CondType -> [(Id,String)] -> TRSGrammar.Equation -> Rs.Cond (Ts.Term Id Id)
fromCond Nothing vars (t1 TRSGrammar.:==: t2) = (termT vars t1) Rs.:->* (termT vars t2)
fromCond (Just TRSGrammar.Oriented) vars (t1 TRSGrammar.:==: t2) = (termT vars t1) Rs.:->* (termT vars t2)
fromCond (Just TRSGrammar.Join) vars (t1 TRSGrammar.:==: t2) = (termT vars t1) Rs.:->*<- (termT vars t2)
fromCond (Just TRSGrammar.SemiEquational) vars (t1 TRSGrammar.:==: t2) = (termT vars t1) Rs.:<-->* (termT vars t2)
fromCond _ vars (t1 TRSGrammar.::-> t2) = (termT vars t1) Rs.:~> (termT vars t2)
fromCond _ vars (t1 TRSGrammar.::->* t2) = (termT vars t1) Rs.:->* (termT vars t2)
fromCond _ vars (t1 TRSGrammar.::->+ t2) = (termT vars t1) Rs.:->+ (termT vars t2)
fromCond _ vars (t1 TRSGrammar.:-><- t2) = (termT vars t1) Rs.:->*<- (termT vars t2)
fromCond _ vars (t1 TRSGrammar.:<--> t2) = (termT vars t1) Rs.:<--> (termT vars t2)
fromCond _ vars (t1 TRSGrammar.:<-->* t2) = (termT vars t1) Rs.:<-->* (termT vars t2)
fromCond _ vars (t1 TRSGrammar.:|>= t2) = (termT vars t1) Rs.:|>= (termT vars t2)
fromCond _ vars (t1 TRSGrammar.:|> t2) = (termT vars t1) Rs.:|> (termT vars t2)
fromCond _ vars (t1 TRSGrammar.::\-> t2) = (termT vars t1) Rs.:\-> (termT vars t2)
fromCond _ vars (t1 TRSGrammar.::\->* t2) = (termT vars t1) Rs.:\->* (termT vars t2)
fromCond _ vars (t1 TRSGrammar.::\->+ t2) = (termT vars t1) Rs.:\->+ (termT vars t2)
fromCond _ vars (t1 TRSGrammar.:\-><-/ t2) = (termT vars t1) Rs.:\->*<-/ (termT vars t2)
fromCond _ vars (t1 TRSGrammar.:<-/\-> t2) = (termT vars t1) Rs.:<-/\-> (termT vars t2)
fromCond _ vars (t1 TRSGrammar.:<-/\->* t2) = (termT vars t1) Rs.:<-/\->* (termT vars t2)
fromCond _ vars (t1 TRSGrammar.::->= t2) = (termT vars t1) Rs.:->= (termT vars t2)
fromCond _ vars (t1 TRSGrammar.::\->= t2) = (termT vars t1) Rs.:\->= (termT vars t2)

-- | Set variables to terms.
termT :: [(Id,String)]
      -> TRSGrammar.Term
      -> Ts.Term Id Id
termT vars (T id [])
    = if (id,"S") `elem` vars then (V id) else (F id [])
termT vars (T id tt)
    = F id (Prelude.map (termT vars) tt)

-- Set function symbols ids in rules: setRsIds, setRIds and setTIds

-- | Set the identifiers with unique numeric identifiers to the rules.
setRsIds :: (HasName a, HasName b, Ord a, Ord b) => Set a -> Set b -> [Rs.Rule (Ts.Term Id Id)] -> Set (Rs.Rule (Ts.Term a b))
setRsIds symbols vars = S.fromList . Prelude.map (setRIds symbols vars)

-- | Set the identifiers with unique numeric identifiers to a rule.
setRIds :: (HasName a, HasName b) => Set a -> Set b -> Rs.Rule (Ts.Term Id Id) -> Rs.Rule (Ts.Term a b)
setRIds symbols vars (l Rs.:-> r) = (setTIds symbols vars l) Rs.:-> (setTIds symbols vars r)

-- | Set the identifiers with unique numeric identifiers to the conditional rules.
setCRsIds :: (HasName a, HasName b, Ord a, Ord b) => Set a -> Set b -> [Rs.CRule (Ts.Term Id Id)] -> Set (Rs.CRule (Ts.Term a b))
setCRsIds symbols vars = S.fromList . Prelude.map (setCRIds symbols vars)

-- | Set the identifiers with unique numeric identifiers to a conditonal rule.
setCRIds :: (HasName a, HasName b) => Set a -> Set b -> Rs.CRule (Ts.Term Id Id) -> Rs.CRule (Ts.Term a b)
setCRIds symbols vars (rule Rs.:=> conds) = (setRIds symbols vars rule) :=> (Prelude.map (setCIds symbols vars) conds)

-- | Set the identifiers with unique numeric identifiers to a conditonal rule.
setCIds :: (HasName a, HasName b) => Set a -> Set b -> Rs.Cond (Ts.Term Id Id) -> Rs.Cond (Ts.Term a b)
setCIds symbols vars (l Rs.:->* r) = (setTIds symbols vars l) Rs.:->* (setTIds symbols vars r)
setCIds symbols vars (l Rs.:~> r) = (setTIds symbols vars l) Rs.:~> (setTIds symbols vars r)
setCIds symbols vars (l Rs.:->+ r) = (setTIds symbols vars l) Rs.:->+ (setTIds symbols vars r)
setCIds symbols vars (l Rs.:->*<- r) = (setTIds symbols vars l) Rs.:->*<- (setTIds symbols vars r)
setCIds symbols vars (l Rs.:<--> r) = (setTIds symbols vars l) Rs.:<--> (setTIds symbols vars r)
setCIds symbols vars (l Rs.:<-->* r) = (setTIds symbols vars l) Rs.:<-->* (setTIds symbols vars r)
setCIds symbols vars (l Rs.:\->* r) = (setTIds symbols vars l) Rs.:\->* (setTIds symbols vars r)
setCIds symbols vars (l Rs.:\-> r) = (setTIds symbols vars l) Rs.:\-> (setTIds symbols vars r)
setCIds symbols vars (l Rs.:\->+ r) = (setTIds symbols vars l) Rs.:\->+ (setTIds symbols vars r)
setCIds symbols vars (l Rs.:\->*<-/ r) = (setTIds symbols vars l) Rs.:\->*<-/ (setTIds symbols vars r)
setCIds symbols vars (l Rs.:<-/\-> r) = (setTIds symbols vars l) Rs.:<-/\-> (setTIds symbols vars r)
setCIds symbols vars (l Rs.:<-/\->* r) = (setTIds symbols vars l) Rs.:<-/\->* (setTIds symbols vars r)
setCIds symbols vars (l Rs.:|>= r) = (setTIds symbols vars l) Rs.:|>= (setTIds symbols vars r)
setCIds symbols vars (l Rs.:->= r) = (setTIds symbols vars l) Rs.:->= (setTIds symbols vars r)
setCIds symbols vars (l Rs.:\->= r) = (setTIds symbols vars l) Rs.:\->= (setTIds symbols vars r)

-- | Set the identifiers with unique numeric identifiers to a term.
setTIds :: (HasName a, HasName b) => Set a -> Set b -> (Ts.Term Id Id) -> (Ts.Term a b)
setTIds symbols vars (F fid tt)
    = F myid (Prelude.map (setTIds symbols vars) tt)
    where myid = case (find (\x -> getName x == fid) . elems $ symbols) of
                   Just existingid
                       -> existingid
                   Nothing
                       -> error ("Error (Parser): Id " ++ fid ++ " not found")
setTIds symbols vars (V vid)
    = V myid
    where myid = case (find (\x -> (getName x) == vid) . elems $ vars) of
                   Just existingid
                       -> existingid
                   Nothing
                       -> error ("Error (Parser): Id " ++ vid ++ "not found")
