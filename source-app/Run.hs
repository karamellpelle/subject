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
module Run
  (
    RunData (..),
    RunM,
    RunType (..),
  ) where

import MyPrelude

import Data.Version
import Data.YAML



-- | our Reader monad
type RunM a = ReaderT RunData IO a

    
--------------------------------------------------------------------------------
--  global settings

-- | global data for application
data RunData = 
  RunData
  {
      --rundataCfg :: YAML,          -- ^ yaml node
      rundataType :: RunType,        -- ^ UI type

      rundataVersion :: Version,     -- ^ Version of program

      rundataVerbose :: Bool
  }


instance Default RunData where
    def = RunData {
      --rundataCfg        = YAML.Node,
      rundataType       = RunTypeTerm,
      rundataVersion    = makeVersion [0,0],
      rundataVerbose    = False
    }


--------------------------------------------------------------------------------
--  

-- | how to run application (UI)
data RunType =
    RunTypeTerm |
    RunTypeGUI
