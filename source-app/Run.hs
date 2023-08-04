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
{-# LANGUAGE TemplateHaskell #-}
module Run
  (
    RunData (..),
    RunM,
    RunType (..),

    getRunData,
  ) where

import MyPrelude

import Data.Version
import Data.YAML
import System.Directory
import System.FilePath

import Development.GitRev
import Paths_subject

import ConfigFile



-- | our Reader monad
type RunM a = ReaderT RunData IO a

    
--------------------------------------------------------------------------------
--  global settings

-- | global data for application
data RunData = 
  RunData
  {
      -- meta information
      runVersion :: Version             -- ^ Version of program
    , runVersionInfo :: String          -- ^ tags, like Git commit
    , runConfigPath :: FilePath         -- ^ user configuration file path
    , runConfigVersion :: (UInt, UInt)  -- ^ file format of configuration file

      -- run settings
    , runVerbose :: Bool          -- ^ be verbose?
    , runType :: RunType          -- ^ type of UI for program

  }


instance Default RunData where
    def = RunData {
        runVersion    = makeVersion [0,0]
      , runVersionInfo = $(gitBranch) ++ " @ " ++ $(gitHash)
      , runConfigPath = "" 
      , runConfigVersion = (0, 0)

      , runVerbose    = False
      , runType       = RunTypeTerm
    }


--------------------------------------------------------------------------------
--  

-- | how to run application (UI) aka frontend
--   FIXME: rename into RunFrontend?
data RunType =
    RunTypeTerm |
    RunTypeGUI String


--------------------------------------------------------------------------------
--  Paths_

-- version :: Version
-- 
-- getBinDir :: IO FilePath
-- getLibDir :: IO FilePath
-- getDynLibDir :: IO FilePath
-- getDataDir :: IO FilePath
-- getLibexecDir :: IO FilePath
-- getSysconfDir :: IO FilePath

--------------------------------------------------------------------------------
--  create data for program execution

-- | create initial 'RunData' for program execution
--    0. from default value
--    1. from system config (file)
--    2. from user config (file)
--    3. from environment variables
getRunData :: IO RunData
getRunData = (flip execStateT) def $ do

    globalCfg <- readConfigFile =<< (io $ getGlobalFileName "config.yaml")
    localCfg  <- readConfigFile =<< (io $ getLocalFileName "config.yaml")

    return ()    
  where
    getGlobalFileName :: FilePath -> IO FilePath
    getGlobalFileName path = getDataFileName ("subject" </> path)

    getLocalFileName :: FilePath -> IO FilePath
    getLocalFileName path = do
        dir <- getXdgDirectory XdgConfig "subject" 
        return $ dir </> path

-- TODO: use Relude::guarded
