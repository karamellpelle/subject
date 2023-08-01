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
{-# LANGUAGE ApplicativeDo #-}
module Main (main) where

import Development.GitRev
import qualified Paths_subject

import MyPrelude

import Options.Applicative
import Data.Version

import qualified Parser as Atto
import Data.Subject
import Data.Subject.Parser

import Cmd
import Run
import Run.Term
import Run.GUI
 

--------------------------------------------------------------------------------
--  main

main :: IO ()
main = do

    -- create initial 'RunData' for program execution
    --    0. from default value
    --    1. from system config (file)
    --    2. from user config (file)
    --    3. from environment variables
    run <- getRunData 
    -- ^ TODO: exception handling 

    -- finialize 'RunData' and retrieve command(s) to perform
    --    4. from command line
    (run, cmd) <- getOptionsAndCmd run

    -- run program in environment
    usingReaderT run $ do
         
        --case run .^ rundataRunType of
        case RunTypeTerm of
            RunTypeTerm -> term cmd
            RunTypeGUI  -> gui cmd

        pure ()


--------------------------------------------------------------------------------
--  create data for program execution

getRunData :: IO RunData
getRunData = do
    pure $ def 
-- TODO: use Relude::guarded


--------------------------------------------------------------------------------
-- parse command line
-- * https://cabal.readthedocs.io/en/latest/cabal-package.html#accessing-data-files-from-package-code
getOptionsAndCmd :: RunData -> IO (RunData, Cmd)
getOptionsAndCmd run =

    customExecParser preferences $ info (parser <**> helper) $ mconcat [ fullDesc 
        , progDesc $ "Handling subjects in different ways"
        , header   $ "* * * subject * * *"
        , footer   $ "Copyright karamellpelle@hotmail.com (c) 2023" ++ showVersion Paths_subject.version ++ " (" ++ $(gitBranch) ++ "@" ++ $(gitHash) ++ ")"
        --, failureCode 1
      ]
    where
      preferences = prefs $ showHelpOnEmpty -- <> showHelpOnError
      -- parse general settings and/or subcommand
      parser = do
          -- options for RunData
          --a <- flag def $ "name" <> short 'n'
          --a <- flag' $ long "name" <> short 'n'
          --a <- switch $ long "name" <> short 'n'

          -- commands and their options
          cmd <- parseCmd 
          
          pure (run, cmd)


--------------------------------------------------------------------------------
--  parse commands


-- | find a subcommand and parse that command's settings
parseCmd :: Parser Cmd
parseCmd = 
    subparser $ parseCmdConfig <> parseCmdID <> parseCmdPack <> parseCmdSend <> parseCmdGUI

-- | "send" command and parse its settings
parseCmdSend :: Mod CommandFields Cmd
parseCmdSend = 
    command "send" $ info (parser <**> helper) $ progDesc "Send subject to server" <> briefDesc
    where
      parser = CmdSend <$> do
          --a <- flag def $ "name" <> short 'n'
          --a <- flag' $ long "name" <> short 'n'
          --a <- switch $ long "name" <> short 'n'
          --a <- strOption $ long "name" <> short "n" <> metavar "ARG"
          --a <- option auto $long "name" <> short 'n' <> metavar "ARG"
          --a <- argument auto $ metavar "ARG" <> value "default value"
          --a <- strArgument $ metavar "ARG" <> value "default value"
          --a <- attoOption $ long "name" <> short 'n' <> metavar "ARG"
          pure $ CmdDataSend


-- | "config" command and parse its settings
parseCmdConfig :: Mod CommandFields Cmd 
parseCmdConfig = 
    command "config" $ info (parser <**> helper) $ progDesc "Send subject to server" <> briefDesc 
    where
      parser = CmdConfig <$> do
          --a <- switch $ long "set" <> short 's'
          --b <- switch $ long "get" <> short 'g'
          c <- strOption $ long "test" <> short 'n' <> metavar "TEST-NAME"
          pure $ CmdDataConfig

-- | "id" command and parse its settings
parseCmdID :: Mod CommandFields Cmd 
parseCmdID = 
    command "id" $ info (parser <**> helper) $ progDesc "convert SubjectID (number <-> magic sentence)" <> briefDesc
    where
      parser = CmdID <$> do
          pure $ CmdDataID

-- | "pack" command and parse its settings
parseCmdPack :: Mod CommandFields Cmd 
parseCmdPack = 
    command "pack" $ info (parser <**> helper) $ progDesc "pack subject" <> briefDesc
    where
      parser = CmdPack <$> do
          pure $ CmdDataPack

-- | "gui" command parse its settings
parseCmdGUI :: Mod CommandFields Cmd 
parseCmdGUI = 
    command "gui" $ info (parser <**> helper) $ progDesc "run as GUI" <> briefDesc
    where
      parser = CmdGUI <$> do
          pure $ CmdDataGUI





