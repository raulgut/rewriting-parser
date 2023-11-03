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

, parseOptions, autoparse

) where

import Parser.TRS.Grammar (TRS)
import Parser.COPS.Parser (parseCOPS)
import Parser.ARI.Parser (parseARI)
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

-- | Property
data Property = None
              | Canonical

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
                                                                       _ -> None
                                                      })
                          "canonical (only CSTRSs)"
                   )
                   "Checks if satisfy the property: canonical (only for CSTRSs)"
          , Option "v" ["version"]
                   (NoArg (\_ -> do hPutStrLn stderr "csrs-check, version 0.2"
                                    exitWith ExitSuccess))
                   "Print version"
          ]

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
fileExtensions :: [(String, String -> Either ParseError TRS)]
fileExtensions = [(".ari", parseARI),(".trs", parseCOPS)]

-- | Parse file into a TRS
autoparse :: String -> String -> TRS
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
