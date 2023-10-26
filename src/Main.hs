{-# LANGUAGE BangPatterns #-}
--------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) muterm development team
-- License     :  see LICENSE
--
-- Maintainer  :  r.gutierrez@upm.es
-- Stability   :  unstable
-- Portability :  non-portable
--
-- This is the CSRS Syntax Checker Main Module
--
-----------------------------------------------------------------------------

module Main (

-- * Exported functions

main

) where

import Parser.TRS.Grammar (isCanonical)

import Interface.CLI (Opt (..), parseOptions, autoparse)
import System.IO (hPutStr, stdout)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | The 'main' function parses an input file and returns nothing if correct, an
-- error if incorrect
main :: IO ()
main =
  do (opts, _) <- parseOptions
     let Opt { inputName = filename 
             , inputContent = input
             , optCanonical = canonical } = opts
     filedata <- input
     let !trs = autoparse filename filedata
     if canonical && (isCanonical trs) then
       hPutStr stdout "The replacement map is not canonical!\n"
      else
        hPutStr stdout ""
