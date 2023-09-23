-- Copyright (C) 2023 karamellpelle@hotmail.com
-- 
-- This file is part of 'subject'.
-- 
-- 'subject' is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- 'subject' is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with 'subject'.  If not, see <http://www.gnu.org/licenses/>.
--
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Run.RunData
  (
    RunData (..),
    RunType (..),
    Version (..),

    TestA (..),
    TestB (..),
  ) where

import MyPrelude 

import Data.Version

import Development.GitRev
import Paths_subject



--------------------------------------------------------------------------------
--  global settings

-- | global data for application
data RunData = 
  RunData
  {
      -- meta information
      runMetaName :: String                 -- ^ application name
    , runMetaVersion :: Version             -- ^ Version of program
    , runMetaVersionInfo :: String          -- ^ tags, like Git commit
    , runMetaSynopsis :: String             -- ^ short application description
    , runMetaCopyright :: String            -- ^ copyright
    , runMetaHomepage :: String             -- ^ homepage of application

    , runConfigPath :: FilePath         -- ^ user configuration file path
    , runConfigDocumentFormat :: String -- ^ file format of configuration file

      -- run settings
    , runVerbose :: Bool          -- ^ be verbose?
    , runType :: RunType          -- ^ type of UI for program

    -- test record patsh
    , runTestA :: TestA  -- ^ test record path
    , runTestB :: TestB  -- ^ test record path
  }


instance Default RunData where
    def                         = RunData {
        runMetaName             = ""
      , runMetaVersion          = makeVersion [0,0]
      , runMetaVersionInfo      = ""
      , runMetaSynopsis         = ""
      , runMetaHomepage         = ""
      , runMetaCopyright        = ""

      , runConfigPath           = ""
      , runConfigDocumentFormat = ""

      , runVerbose              = False
      , runType                 = RunTypeTerm

      , runTestA                = def
      , runTestB                = def
    }

--------------------------------------------------------------------------------
--  

-- | how to run application (UI) aka frontend
--   FIXME: rename into RunFrontend?
data RunType =
    RunTypeTerm |
    RunTypeGUI String


--------------------------------------------------------------------------------
--  Test fields

data TestA =
    TestA {
        testaId :: UInt
      , testaName :: String
      , testaTestB :: TestB
    }

data TestB =
    TestB {
        testbChar0 :: Char
      , testbChar1 :: Char
      , testbChar2 :: Char
    }
instance Default TestA where
    def = TestA { testaId = 0, testaName = "TestA", testaTestB = def }

instance Default TestB where
    def = TestB { testbChar0 = 'A', testbChar1 = 'B', testbChar2 = 'C'}


