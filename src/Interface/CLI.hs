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

Opt(..)

-- * Exported functions

, parseOptions, autoparse

) where

import Parser.COPS.Parser (parseCOPS)
import Parser.COPS.TRS.Grammar (Spec)
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
data Opt = Opt {inputName :: String -- ^ Input file name
               ,inputContent :: IO String -- ^ Input file content
               }

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | Default parameters
startOpt :: Opt
startOpt 
  = Opt {inputName = "foo.trs"
        ,inputContent = exitErrorHelp "use -i option to set input"
        -- a simple way to handle mandatory flags
        }

-- | Command line options
options :: [OptDescr (Opt -> IO Opt)]
options = [ Option "h" ["help"]
                   (NoArg (\opt -> exitHelp))
                   "Show usage info"
          , Option "i" ["input"]
                   (ReqArg (\arg opt -> do return opt {inputName = arg 
                                                      ,inputContent = readFile arg})
                           "FILE"
                   )
                   "Input COPS file"
          , Option "v" ["version"]
                   (NoArg (\_ -> do hPutStrLn stderr "csrs-check, version 0.1"
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
fileExtensions :: [(String, String -> Either ParseError Spec)]
fileExtensions = [(".trs", parseCOPS)]

-- | Parse file into a TRS
autoparse :: String -> String -> Spec
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
