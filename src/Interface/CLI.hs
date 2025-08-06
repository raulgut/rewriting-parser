{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Interface.CLI
-- Copyright   :  (c) muterm development team
-- License     :  see LICENSE
--
-- Maintainer  :  r.gutierrez@dsic.upv.es
-- Stability   :  unstable
-- Portability :  non-portable
--
-- This module manage the command line interface. Based on
-- <http://www.haskell.org/pipermail/haskell/2004-January/013412.html>
--
-----------------------------------------------------------------------------

module Interface.CLI (

-- * Exported data

Opt(..), Property (..)

-- * Exported functions

, parseOptions, autoparse, parseARI

) where

import Parser.TRS.Grammar (TRS, INF)
import Parser.TRS.Properties (Property (..))
import Parser.COPS.Parser (parseCOPS, parseExtCOPS)
import Parser.ARI.Parser (parseARI, parseExtARI)
import System.Environment (getProgName, getArgs)
import System.Exit (ExitCode(ExitSuccess,ExitFailure), exitWith, exitFailure)
import System.IO (hPutStrLn, stderr, hFlush)
import System.Console.GetOpt (OptDescr(Option), ArgDescr(NoArg, ReqArg), ArgOrder(RequireOrder), usageInfo, getOpt)
import Text.ParserCombinators.Parsec.Error(ParseError)
import Control.Monad (msum, MonadPlus (..))
import Data.List (isSuffixOf)
import Control.Monad (when)

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

-- | Command line options
data Opt = Opt { inputName :: String -- ^ Input file name
               , inputContent :: IO String -- ^ Input file content
               , optProperty :: Property -- ^ Check property
               }

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | Default parameters
startOpt :: Opt
startOpt 
  = Opt { inputName = "foo.trs"  
        , inputContent = exitErrorHelp "use -i option to set input"
          -- a simple way to handle mandatory flags
        , optProperty = None
        }

-- | Command line options
options :: [OptDescr (Opt -> IO Opt)]
options = [ Option "h" ["help"]
                   (NoArg (\opt -> exitHelp))
                   "Show usage info"
          , Option "i" ["input"]
                   (ReqArg (\arg opt -> do return opt { inputName = arg 
                                                      , inputContent = readFile arg})
                           "FILE"
                   )
                   "Input file"
          , Option "p" ["check-property"]
                   (ReqArg (\arg opt -> do return opt { optProperty = case arg of 
                                                                       "canonical" -> Canonical
                                                                       "srs" -> SRS
                                                                       "left-linear" -> LeftLinear
                                                                       "right-ground" -> RightGround
                                                                       "ground" -> Ground
                                                                       "oriented" -> Oriented
                                                                       "join" -> Join
                                                                       "semi-equational" -> SemiEquational
                                                                       "normal" -> Normal
                                                                       "type1" -> OneCTRS
                                                                       "type2" -> TwoCTRS
                                                                       "type3" -> ThreeCTRS
                                                                       _ -> None
                                                      })
                          "PROPERTY (see --show-properties)"
                   )
                   "Checks if satisfies PROPERTY"
          , Option "" ["show-properties"]
                   (NoArg (\_ -> do hPutStrLn stderr ppString
                                    exitWith ExitSuccess))
                   "Show properties"
          , Option "v" ["version"]
                   (NoArg (\_ -> do hPutStrLn stderr "csrs-check, version 0.2"
                                    exitWith ExitSuccess))
                   "Print version"
          ]

-- | Properties message
ppString = "Properties:\n" 
  ++ " * For TRSs:\n"
  ++ " -> srs: checks if the TRS is a SRS\n"
  ++ " -> left-linear: checks if the TRS is a left-linear\n"
  ++ " -> right-ground: checks if the TRS is a right-ground\n"
  ++ " -> ground: checks if the TRS is a ground\n"
  ++ " * For CSTRSs:\n"
  ++ " -> canonical: checks if the replament map is canonical\n"
  ++ " * For CTRSs and CSCTRSs:\n"
  ++ " -> oriented: checks if the conditions are oriented\n"
  ++ " -> join: checks if the conditions are join\n"
  ++ " -> semi-equational: checks if the conditions are semi-equational\n"
  ++ " -> type1: checks if it is a Type 1 CTRS\n"
  ++ " -> type2: checks if it is a Type 2 CTRS\n"
  ++ " -> type3: checks if it is a Type 3 CTRS\n"
  ++ " * For CTRSs:\n"
  ++ " -> normal: checks if the CTRS is normal\n"
  ++ " -> left-linear: checks if the CTRS is a left-linear"

-- | Help information
showHelp :: IO ()
showHelp = do prg <- getProgName
              hPutStrLn stderr (usageInfo prg options)
              hFlush stderr

-- | -h (--help) Show help
exitHelp :: IO Opt
exitHelp = do showHelp
              exitWith ExitSuccess

-- | Show error and help information
exitErrorHelp :: String -> IO a
exitErrorHelp msg = do hPutStrLn stderr msg
                       hPutStrLn stderr ""
                       showHelp
                       exitFailure

-- | Parse options
parseOptions :: IO (Opt, [String])
parseOptions = do (optsActions, rest, errors) <- getArgs
                    >>= return . getOpt RequireOrder options
                    -- parse the arguments with the given options
                  when (not (null errors)) $ do mapM_ (hPutStrLn stderr) errors
                                                -- show all errors in the stderr output
                                                showHelp
                                                exitFailure
                  opts <- foldl (>>=) (return startOpt) optsActions
                  -- apply actions to the default parameters
                  return (opts, rest)

-- | File extensions
--fileExtensions :: [(String, String -> Either ParseError TRS)]
-- fileExtensions = [(".ari", parseARI),(".trs", parseCOPS)]
fileExtensions :: [(String, String -> Either ParseError (TRS, Maybe INF))]
fileExtensions = [(".ari", parseExtARI),(".trs", parseExtCOPS)]

-- | Parse file into a TRS
autoparse :: String -> String -> (TRS, Maybe INF)
autoparse fname = maybe (error "Error (CLI): File Extension not supported")
                        parseWithFailure
                        matchParser
   where matchParser
             = msum $ map (\(ext,p)-> if fname `endsWith` ext then Just p else Nothing) fileExtensions
         endsWith
             = flip isSuffixOf
         parseWithFailure parser contents
             = case parser contents of
                 Left parseerror
                     -> error$ "Parse Error (CLI): " ++ show parseerror
                 Right sys
                     -> sys
