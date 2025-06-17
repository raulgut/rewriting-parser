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

import Parser.TRS.Grammar (TRS (..), TRSType (..))
import Parser.TRS.Properties (Property (..), isCanonical, isSRS, isLeftLinear
  , isRightGround, isGround, isOriented, isJoin, isSemiEquational
  , isNormal, isOneCTRS, isTwoCTRS, isThreeCTRS, isTRSConditional, isConditional)
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
             , optProperty = prop } = opts
     filedata <- input
     let !(trs,inf) = autoparse filename filedata
     case prop of 
      Canonical -> if (not . isCanonical $ trs) then
                     hPutStr stdout "NO\n\nProperty canonical:\n -> The replacement map is not canonical!\n"
                   else
                     hPutStr stdout "YES\n" 
      SRS -> if (trsType trs == TRSStandard) then
               if (not . isSRS $ trs) then
                 hPutStr stdout "NO\n\nProperty SRS:\n -> The system is not a String Rewriting System!\n"
               else
                 hPutStr stdout "YES\n" 
             else
               hPutStr stdout "NO\n\nProperty SRS:\n -> The format is not TRS\n"
      LeftLinear -> if (trsType trs == TRSStandard) || (isTRSConditional . trsType $ trs) then
                      if (not . isLeftLinear $ trs) then
                        hPutStr stdout "NO\n\nProperty Left Linear:\n -> The system is not Left Linear!\n"
                      else
                        hPutStr stdout "YES\n" 
                    else
                      hPutStr stdout "NO\n\nProperty Left Linear:\n -> The format is neither TRS nor CTRS\n"
      RightGround -> if (trsType trs == TRSStandard) then
                       if (not . isRightGround $ trs) then
                         hPutStr stdout "NO\n\nProperty Right Ground:\n -> The system is not Right Ground!\n"
                       else
                         hPutStr stdout "YES\n" 
                     else
                       hPutStr stdout "NO\n\nProperty Right Ground:\n -> The format is not TRS\n"
      Ground -> if (trsType trs == TRSStandard) then
                  if (not . isGround $ trs) then
                    hPutStr stdout "NO\n\nProperty Ground:\n -> The system is not Ground!\n"
                  else
                    hPutStr stdout "YES\n" 
                else
                  hPutStr stdout "NO\n\nProperty Ground:\n -> The format is not TRS\n"
      Oriented -> if (isConditional $ trs) then
                    if (not . isOriented $ trs) then
                        hPutStr stdout "NO\n\nProperty Oriented:\n -> The condition relation is not Oriented!\n"
                      else
                        hPutStr stdout "YES\n" 
                  else
                    hPutStr stdout "NO\n\nProperty Oriented:\n -> The format is neither CTRS nor CSCTRS\n"
      Join -> if (isConditional $ trs) then
                if (not . isJoin $ trs) then
                  hPutStr stdout "NO\n\nProperty Join:\n -> The condition relation is not Oriented!\n"
                else
                  hPutStr stdout "YES\n" 
              else
                hPutStr stdout "NO\n\nProperty Join:\n -> The format is neither CTRS nor CSCTRS\n"
      SemiEquational -> if (isConditional $ trs) then
                          if (not . isSemiEquational $ trs) then
                            hPutStr stdout "NO\n\nProperty Semi-Equational:\n -> The condition relation is not Semi-Equational!\n"
                          else
                            hPutStr stdout "YES\n" 
                        else
                          hPutStr stdout "NO\n\nProperty Semi-Equational:\n -> The format is neither CTRS nor CSCTRS\n"
      Normal -> if (isTRSConditional . trsType $ trs) then
                  if (not . isNormal $ trs) then
                        hPutStr stdout "NO\n\nProperty Normal:\n -> The CTRS is not Normal!\n"
                      else
                        hPutStr stdout "YES\n" 
                else
                  hPutStr stdout "NO\n\nProperty Normal:\n -> The format is not CTRS\n"
      OneCTRS -> if (isConditional $ trs) then
                   if (not . isOneCTRS $ trs) then
                     hPutStr stdout "NO\n\nProperty Type 1 CTRS:\n -> The system is not a Type 1 CTRS!\n"
                   else
                     hPutStr stdout "YES\n" 
                 else
                   hPutStr stdout "NO\n\nProperty Type 1 CTRS:\n -> The format is neither CTRS nor CSCTRS\n"
      TwoCTRS -> if (isConditional $ trs) then
                  if (not . isTwoCTRS $ trs) then
                        hPutStr stdout "NO\n\nProperty Type 2 CTRS:\n -> The system is not a Type 2 CTRS!\n"
                      else
                        hPutStr stdout "YES\n" 
                 else
                   hPutStr stdout "NO\n\nProperty Type 2 CTRS:\n -> The format is neither CTRS nor CSCTRS\n"
      ThreeCTRS -> if (isConditional $ trs) then
                     if (not . isThreeCTRS $ trs) then
                         hPutStr stdout "NO\n\nProperty Type 3 CTRS:\n -> The system is not a Type 3 CTRS!\n"
                       else
                         hPutStr stdout "YES\n" 
                   else
                     hPutStr stdout "NO\n\nProperty Type 3 CTRS:\n -> The format is neither CTRS nor CSCTRS\n"
      _ -> hPutStr stdout ""
     
