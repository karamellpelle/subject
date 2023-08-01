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
    CmdDataSend (..),
    CmdDataPack (..), 
    CmdDataGUI  (..), 
    CmdDataID   (..), 

  ) where

import MyPrelude

--------------------------------------------------------------------------------
-- which command to perform
--

data Cmd =
    CmdConfig !CmdDataConfig | -- ^ write configuration
    CmdSend   !CmdDataSend   | -- ^ send subject to server
    CmdPack   !CmdDataPack   | -- ^ pack data
    CmdGUI    !CmdDataGUI    | -- ^ run in GUI mode
    CmdID     !CmdDataID       -- ^ run in GUI mode


--------------------------------------------------------------------------------
--  command specific settings

data CmdDataConfig =
    CmdDataConfig

data CmdDataPack =
    CmdDataPack

data CmdDataGUI =
    CmdDataGUI

data CmdDataID =
    CmdDataID

data CmdDataSend =
    CmdDataSend
    --{
    --    cmdSendRecipient :: Recipient
    --}

