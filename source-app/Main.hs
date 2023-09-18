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
{-# LANGUAGE ApplicativeDo #-}
module Main (main) where

import MyPrelude

import Options.Applicative

--import Parser qualified as Atto
--import Parser.SubjectID qualified as Atto


import Cmd
import Run
import Run.Term
import Run.GUI
 

--------------------------------------------------------------------------------
--  main

main :: IO ()
main = do

    -- retrieve initial 'RunData' for program execution
    --    0. from default value
    --    1. from system config (file)
    --    2. from user config (file)
    --    3. from environment variables
    getRunData >>= \maybeR -> case maybeR of
        Left err  -> do
            putTextLn $ "Error while initializing application: " <> err
            putTextLn "Exiting"
            exitFailure

        Right run -> do

            -- finialize 'RunData' and retrieve command(s) to perform
            --    4. from command line
            (run, cmd) <- getOptionsAndCmd run

            -- run application in the RunData environment
            usingReaderT run $ do

                case runType run of
                    RunTypeTerm      -> term cmd
                    RunTypeGUI front -> gui front cmd

    exitSuccess




--------------------------------------------------------------------------------
-- parse command line
--

--------------------------------------------------------------------------------

-- | modify RunData and get Command
getOptionsAndCmd :: RunData -> IO (RunData, Cmd)
getOptionsAndCmd run =

    customExecParser preferences $ info (parser <**> helper) $ mconcat [ fullDesc 
        , header   $ prettyHeader $ runMetaName run
        , progDesc $ runMetaSynopsis run
        , footer   $ runMetaCopyright run ++ show (runMetaVersion run) ++ runMetaVersionInfo run
        --, failureCode 1
      ]
    where
      prettyHeader head = "* * * " <> head <> " * * *"
      preferences = prefs $ showHelpOnEmpty 

      -- parse general settings and/or subcommand
      parser = do

          -- options for RunData:
          --  - GUI with frontend?

          runtype <- (option (fmap RunTypeGUI str) $ long "gui" <> metavar "FRONTEND" <> help "Run application in GUI mode using given frontend") <|> pure RunTypeTerm


          -- parse commands and their options
          cmd <- parseCmd-- <|> pure CmdEmpty

          pure (run { runType = runtype }, cmd) 


-- | find a subcommand and parse that command's settings
parseCmd :: Parser Cmd
parseCmd = 
    subparser $ parseCmdConfig <> parseCmdID <> parseCmdPack <> parseCmdSend <> parseCmdInfo <> parseCmdGUI 


--------------------------------------------------------------------------------
--  parse commands
--a <- flag default $ "name" <> short 'n' <> help "help text"
--a <- flag' $ long "name" <> short 'n' <> help "help text"
--a <- switch $ long "name" <> short 'n' <> help "help text"
--a <- strOption $ long "name" <> short "n" <> metavar "ARG" <> help "help text"
--a <- option auto $long "name" <> short 'n' <> metavar "ARG" <> help "help text" -- str, auto, maybeReader, eitherReader 
--a <- argument auto $ metavar "ARG" <> value "default value" <> help "help text"
--a <- strArgument $ metavar "ARG" <> value "default value" <> help "help text"
--a <- attoOption $ long "name" <> short 'n' <> metavar "ARG" <> help "help text"


-- | command "send" and its options.
--   send a folder or packed subject to server. TODO: shall we ignore prepacked folder to prevent sending non-encrypted subjects?
parseCmdSend :: Mod CommandFields Cmd
parseCmdSend = 
    command "send" $ info (parser <**> helper) $ progDesc "Send folder or packed subject to server" <> briefDesc
    where
      parser = CmdSend <$> do
          recipient <- option (str) $ long "recipient" <> metavar "RECIPIENT" <> help "Receiver"
          arg <- argument str $ metavar "FOLDER|PACKED_FILE" <> help "Packed file or folder"
          pure $ CmdDataSend {
              cmdsendPath = arg
            , cmdsendRecipient = def { recipientName = recipient } -- FIXME
          }


-- | command "config" and its options.
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

-- | command "id" and its options
--   FIXME: Use SubjectID instead of String
parseCmdID :: Mod CommandFields Cmd 
parseCmdID = 
    command "id" $ info (parser <**> helper) $ progDesc "Convert SubjectID (number <-> magic sentence)" <> briefDesc
    where
      parser = CmdID <$> do
          short <- switch $ long "short" <> help "minimal output"
          args <- many $ argument str $ metavar "ID(s)"
          
          pure $ CmdDataID {
              cmdidShort = short
            , cmdidArgs = args
          }

-- | command "pack" and its options
--   TODO: metadata: notes, type, tags, etc.
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
              cmdpackRecipient = def { recipientName = recipient }
            , cmdpackPath = arg
            , cmdpackSubjectID = sid
          }

-- | command "gui" and its options
parseCmdGUI :: Mod CommandFields Cmd 
parseCmdGUI = 
    command "gui" $ info (parser <**> helper) $ progDesc "Run command in GUI" <> briefDesc 
    where
      parser = CmdGUI <$> do
          --full <- switch $ long "fullscreen" <> help "Start GUI in fullscreen"
          pure $ CmdDataGUI {
              --cmdguiFullscreen = full
          }

-- | command "info" and its options
parseCmdInfo :: Mod CommandFields Cmd 
parseCmdInfo = 
    command "info" $ info (parser <**> helper) $ progDesc "Show specific info" <> briefDesc
    where
      parser = CmdInfo <$> do
          -- TODO: more settings
          pure $ CmdDataInfo


--import qualified Data.Attoparsec.Text as A
--attoReadM :: A.Parser a -> ReadM a
--attoReadM p = eitherReader (A.parseOnly p . T.pack)
--
--atto :: AttoParse a => ReadM a
--atto = eitherReader (parseOnly parser . T.pack)
--
--attoOption :: AttoParse a => Option a




