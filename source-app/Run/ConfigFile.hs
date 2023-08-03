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
module Run.ConfigFile
  (
    ConfigFile (..),
  ) where

import MyPrelude

import Data.YAML


--------------------------------------------------------------------------------
--  ConfigFile: data that can be defined in a configuration file

-- | 
data ConfigFile = 
  ConfigFile
  {
      -- meta
      configfileVersion :: (UInt, UInt)
    , configfilePath :: FilePath

      -- settings: servers, signers, receivers, etc.
  }


instance Default ConfigFile where
    def = ConfigFileData {
        configfileVersion    = (0, 0)
      , configfileFilePath   = ""
    }

--------------------------------------------------------------------------------
--  YAML

--instance FromYAML ConfigFile where
--
--instance ToYAML ConfigFile where
    


--------------------------------------------------------------------------------
--  adding configuration, with right over left

-- | right overwrites left
instance SemiGroup ConfigFile where
    cf0 (<>) cf1 = cf0 {
          configfileVersion = configFileVersion cf1
        , configfilePath = ""
    }
    where
      

