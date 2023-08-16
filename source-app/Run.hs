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
module Run
  (
    RunData (..),
    RunM,
    RunType (..),

    getRunData,
  ) where

import MyPrelude 

import Data.Version
import Data.YAML as YAML
import System.Directory
import System.FilePath

import Development.GitRev
import Paths_subject

import RecordField
import Parser.RecordField

--import Relude.Extra.Lens


-- | our Reader monad
type RunM a = ReaderT RunData IO a


--------------------------------------------------------------------------------
--  system data

--    * https://cabal.readthedocs.io/en/3.10/cabal-package.html#accessing-data-files-from-package-code
--    * TODO: use cabal >= 3.10 and access package fields (https://cabal.readthedocs.io/en/latest/cabal-package.html#accessing-package-related-informations)
--        * name :: String
--        * version :: Version
--        * synopsis :: String
--        * copyright :: String
--        * homepage :: String
--      , for now: define them here:


packageInfo_name :: String
packageInfo_name = "subject"

packageInfo_version :: Version
packageInfo_version = version

packageInfo_synopsis :: String
packageInfo_synopsis = "Naming, packing and sending image folders as subjects"

packageInfo_copyright :: String
packageInfo_copyright = "Copyright karamellpelle@hotmail.com (c) 2023"

packageInfo_homepage :: String
packageInfo_homepage = "https://github.com/karamellpelle/subject"



getGlobalFileName :: MonadIO m => FilePath -> m FilePath
getGlobalFileName path = io $ getDataFileName (packageInfo_name </> path)

getLocalFileName :: MonadIO m => FilePath -> m FilePath
getLocalFileName path = do
    dir <- io $ getXdgDirectory XdgConfig packageInfo_name 
    return $ dir </> path

    
--------------------------------------------------------------------------------
--  TODO: REmove
{-
log :: MonadIO m => Text -> m ()
log = putTextLn

logError :: MonadIO m => Text -> m ()
logError str = putTextLn $ "ERROR: " <> str

logWarn :: MonadIO m => Text -> m ()
logWarn str = putTextLn $ "WARNING: " <> str
-}

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
    , runMetaCopyright :: String             -- ^ copyright
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


--------------------------------------------------------------------------------
--  create data for program execution

type StateR = StateT RunData IO

type ErrorString = Text

type ExceptStateR = ExceptT ErrorString StateR

-- | create initial 'RunData' for program execution
--    0. from default value
--    1. from system config (file)
--    2. from user config (file)
--    3. from environment variables
--
-- TODO: use lenses
getRunData :: IO (Either ErrorString RunData)
getRunData = evaluatingStateT def $ 
    runExceptT $ do

        -- application name
        modify $ \run -> run { runMetaName = packageInfo_name }

        -- version
        modify $ \run -> run { runMetaVersion = packageInfo_version }

        -- version info
        modify $ \run -> run { runMetaVersionInfo = $(gitBranch) ++ " @ " ++ $(gitHash) }

        -- application synopsis
        modify $ \run -> run { runMetaSynopsis = runMetaSynopsis run }

        -- application homepage
        modify $ \run -> run { runMetaHomepage = runMetaHomepage run }

        -- copyright
        modify $ \run -> run { runMetaCopyright = packageInfo_copyright }

        -- apply configuration from system files
        -- TODO: abstract document format, don't rely on YAML
        -- TODO: more specific document format name
        configGlobal <- getGlobalFileName "config.yaml"
        configLocal  <- getLocalFileName "config.yaml"
        modify $ \run -> run { runConfigPath = configLocal }
        modify $ \run -> run { runConfigDocumentFormat = "YAML" }

        applyConfigFile configGlobal
        applyConfigFile configLocal

        -- TODO: environment variables

        -- TODO: other settings

        -- return either RunData or error
        get


--------------------------------------------------------------------------------
--  configuration file
--    * a configuration is defined by a file with a specific format
--    * we choose YAML here

-- document format: YAML
type DocYAML = Doc (Node Pos)

-- | read config file and apply on 'RunData'.
--   will terminate program if necessary data couldn't be read
applyConfigFile :: FilePath -> ExceptStateR ()
applyConfigFile path = do
    let yaml = (undefined :: DocYAML)
    --required yaml "version" $ \lens -> modify $ over 
    --required yaml "testA > testB > char0" $ \v -> modify $ over 
    --optional yaml "testB > char0" $ (modify $ over (receiverL . fingerprintL))
    
    return ()




--------------------------------------------------------------------------------
--  manipulate record fields from RunData

{-}
required :: FromYAML a => DocYAML -> Text -> (a -> StateR b) -> ExceptStateR b
required doc recordfields f = 
    undefined ()
    --lift $ case fieldPath recordfields of 
    
        --Left err -> return $ Left $ quote recordfields <> " is not a valid record path; " <> err
        --Right fs -> do
        --    lift $ f undefined
        --    -- find lens; if not Left "application has no such setting: "
        --    -- map fpath into document, then read node: print parse errors and throw error if problems
-}

--------------------------------------------------------------------------------
--  tables of record field -> Lens

instance LookupLensFrom RunData where
    lookupLensFrom = lensRecordFieldTable [
            lensRecordField "config-documentformat" runConfigDocumentFormatL
          , lensRecordField "testA" runTestAL
          , lensRecordField "testB" runTestBL
        ]

instance LookupLensFrom TestB where
    lookupLensFrom = lensRecordFieldTable [
            lensRecordField "char0" testbChar0L
          , lensRecordField "char1" testbChar1L
          , lensRecordField "char2" testbChar2L
        ]

instance LookupLensFrom TestA where
    lookupLensFrom = lensRecordFieldTable [
            lensRecordField "id" testaIdL
          , lensRecordField "name" testaNameL
          , lensRecordField "testB" testaTestBL
        ]

-- lenses
runConfigDocumentFormatL = runConfigDocumentFormat 
runTestAL = runTestA
runTestBL = runTestB
testaTestBL = testaTestB
testaIdL = testaId
testaNameL = testaName
testbChar0L = testbChar0
testbChar1L = testbChar1
testbChar2L = testbChar2
