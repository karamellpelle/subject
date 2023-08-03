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
import qualified Parser.SubjectID as Atto

import Data.Subject

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

        case runType run of
            RunTypeTerm      -> term cmd
            RunTypeGUI front -> gui front cmd

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
        , header   $ "* * * subject * * *"
        , progDesc $ "Naming, packing and sending image folders as subjects"
        , footer   $ "Copyright karamellpelle@hotmail.com (c) 2023. Version " ++ showVersion Paths_subject.version ++ "\n" ++ 
                     "Git: " ++ $(gitBranch) ++ " @ " ++ $(gitHash)
        --, failureCode 1
      ]
    where
      preferences = prefs $ showHelpOnEmpty -- <> showHelpOnError
      -- parse general settings and/or subcommand
      parser = do

          -- GUI? choose frontend to run command
          runtype <- (option (fmap RunTypeGUI str) $ long "gui" <> metavar "FRONTEND" <> help "Run application in GUI mode using given frontend") <|> pure RunTypeTerm

          -- parse options for RunData
          --

          -- parse commands and their options
          -- TODO: use lenses and update multiple fields of 'run'
          cmd <- parseCmd-- <|> pure CmdEmpty

          --pure (run, cmd)
          pure (run { runType = runtype }, cmd) 


-- | find a subcommand and parse that command's settings
parseCmd :: Parser Cmd
parseCmd = 
    subparser $ parseCmdConfig <> parseCmdID <> parseCmdPack <> parseCmdSend <> parseCmdInfo <> parseCmdGUI 


--------------------------------------------------------------------------------
--  parse commands
--a <- flag def $ "name" <> short 'n'
--a <- flag' $ long "name" <> short 'n'
--a <- switch $ long "name" <> short 'n'
--a <- strOption $ long "name" <> short "n" <> metavar "ARG"
--a <- option auto $long "name" <> short 'n' <> metavar "ARG" -- str, auto, maybeReader, eitherReader
--a <- argument auto $ metavar "ARG" <> value "default value"
--a <- strArgument $ metavar "ARG" <> value "default value"
--a <- attoOption $ long "name" <> short 'n' <> metavar "ARG"


-- | "send" command and parse its settings
--   send a folder or packed subject to server. TODO: shall we ignore prepacked folder to prevent sending non-encrypted subjects?
parseCmdSend :: Mod CommandFields Cmd
parseCmdSend = 
    command "send" $ info (parser <**> helper) $ progDesc "Send folder or packed subject to server" <> briefDesc
    -- ^ 
    where
      parser = CmdSend <$> do
          recipient <- option (str) $ long "recipient" <> metavar "RECIPIENT" <> help "Receiver"
          arg <- argument str $ metavar "FOLDER|PACKED_FILE" <> help "Packed file or folder"
          pure $ CmdDataSend {
              cmdsendPath = arg
            , cmdsendRecipient = recipient
          }


-- | "config" command and parse its settings
--   config write or get 
parseCmdConfig :: Mod CommandFields Cmd 
parseCmdConfig = 
    command "config" $ info (parser <**> helper) $ progDesc "Configure user settings" <> briefDesc 
    where
      parser = CmdConfig <$> do
          -- TODO: implement set
          getset <- (fmap Just $ Get <$> (option str $ long "get" <> short 'g' <> metavar "VARIABLE" <> help "Get variable")) <|> 
                    (fmap Just $ Set <$> (option str $ long "set" <> short 's' <> metavar "VARIABLE" <> help "Set variable") <*> (option str $ long "value" <> short 'v' <> metavar "VALUE" <> help "Value to set"))
          pure $ CmdDataConfig {
              cmdconfigGetSet = getset
                      
          }

-- | "id" command and parse its settings
--   FIXME: Use SubjectID instead of String
parseCmdID :: Mod CommandFields Cmd 
parseCmdID = 
    command "id" $ info (parser <**> helper) $ progDesc "Convert SubjectID (number <-> magic sentence)" <> briefDesc
    where
      parser = CmdID <$> do
          short <- switch $ long "short" <> help "minimal output"
          args <- some $ argument str $ metavar "ID"
          
          pure $ CmdDataID {
              cmdidShort = short
            , cmdidArgs = args
          }

-- | "pack" command and parse its settings
parseCmdPack :: Mod CommandFields Cmd 
parseCmdPack = 
    command "pack" $ info (parser <**> helper) $ progDesc "Pack subject directory" <> briefDesc
    where
      parser = CmdPack <$> do
          -- FIXME: SubjectID instead of string
          recipient <- option (str) $ long "recipient" <> metavar "RECIPIENT" <> help "Receiver"
          sid <- option (str) $ long "subject-id" <> metavar "SUBJECT-ID" <> help "Naming folder (SubjectID)"
          arg <- argument str $ metavar "FOLDER" <> help "Folder to pack"

          pure $ CmdDataPack {
              cmdpackRecipient = recipient
            , cmdpackPath = arg
            , cmdpackSubjectID = sid
          }

-- | "gui" command parse its settings
parseCmdGUI :: Mod CommandFields Cmd 
parseCmdGUI = 
    command "gui" $ info (parser <**> helper) $ progDesc "Run command in GUI" <> briefDesc 
    where
      parser = CmdGUI <$> do
          frontend <- option (str) $ long "frontend" <> metavar "FRONTEND" <> value guiDefaultFrontend <> help "Name of frontend"
          pure $ CmdDataGUI {
              cmdguiFrontend = frontend
          }

-- | "info" command and parse its settings
parseCmdInfo :: Mod CommandFields Cmd 
parseCmdInfo = 
    command "info" $ info (parser <**> helper) $ progDesc "Show specific info" <> briefDesc
    where
      parser = CmdInfo <$> do
          -- TODO: more settings
          pure $ CmdDataInfo





