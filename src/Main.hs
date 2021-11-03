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
-- This is the CSRS Syntax Checker Main Module.
--
-----------------------------------------------------------------------------

module Main (

-- * Exported functions

main

) where

import Interface.CLI (Opt (..), parseOptions, autoparse)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | The 'main' function parses an input file and returns nothing if correct, an
-- error if incorrect
main :: IO ()
main =
  do (opts, _) <- parseOptions
     let Opt { optInput = input} = opts
     filedata <- input
     let trs = autoparse name filedata
