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
module Cmd
  (
    Cmd (..),

    CmdDataConfig (..),
    CmdDataID   (..), 
    CmdDataInfo   (..), 
    CmdDataPack (..), 
    CmdDataSend (..),
    CmdDataGUI  (..), 

    Frontend,
    GetSet (..),

  ) where

import MyPrelude
import Data.Recipient
import Data.Subject

--------------------------------------------------------------------------------
--  types

type Frontend = String


--------------------------------------------------------------------------------
-- which command to perform
--

data Cmd =
    CmdEmpty                 | -- ^ no command
    CmdConfig !CmdDataConfig | -- ^ write configuration
    CmdID     !CmdDataID     | -- ^ convert SubjectID
    CmdInfo   !CmdDataInfo   | -- ^ show info 
    CmdPack   !CmdDataPack   | -- ^ pack data
    CmdSend   !CmdDataSend   | -- ^ send subject to server
    CmdGUI    !CmdDataGUI      -- ^ run in GUI mode . TODO: CmdGUI !CmdDataGUI !Cmd -- use subcommand? run like so: 'subject --verbose gui --frontend gtk --fullscreen id --short 43 332 434 444  


--------------------------------------------------------------------------------
--  command specific settings

data GetSet =
    Get Text | Set Text Text

-- | config settings
data CmdDataConfig =
    CmdDataConfig {
        cmdconfigGetSet :: Maybe GetSet
    }

-- | pack settings
data CmdDataPack =
    CmdDataPack {
        cmdpackRecipient :: Recipient
        --cmdpackID :: SubjectID
      , cmdpackSubjectID :: String
      , cmdpackPath :: FilePath
    }

-- | GUI settings
data CmdDataGUI =
    CmdDataGUI {
        -- specific GUI settings
        --cmdguiFullscreen :: Bool
    }

-- | ID settings
--   FIXME: Use SubjectID instead of String
data CmdDataID =
    CmdDataID {
        cmdidShort :: Bool
      , cmdidArgs :: [String]
    }

-- | info settings
data CmdDataInfo =
    CmdDataInfo

-- | send settings
data CmdDataSend =
    CmdDataSend {
        cmdsendRecipient :: Recipient
      , cmdsendPath :: FilePath
    }

