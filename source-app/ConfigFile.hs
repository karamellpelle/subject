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
module ConfigFile
  (
    ConfigFile (..),

    readConfigFile,
  ) where

import MyPrelude

import Data.YAML


--------------------------------------------------------------------------------
--  ConfigFile: 

-- | data that can be defined in a configuration file
data ConfigFile = 
  ConfigFile
  {
      -- meta
      configfileVersion :: (UInt, UInt)
    , configfilePath :: FilePath

      -- settings: servers, signers, receivers, etc.
  }

-- Default
instance Default ConfigFile where
    def = ConfigFile {
        configfileVersion    = (0, 0)
      , configfilePath   = ""
    }

-- | right adds to (or overwrite) left
instance Semigroup ConfigFile where
    (<>) config0 config1 = config0 {
          configfileVersion = configfileVersion config1
        , configfilePath = configfilePath config1
    }
      

--------------------------------------------------------------------------------
--  read from file

--instance FromYAML ConfigFile where
--
--instance ToYAML ConfigFile where
    
readConfigFile :: MonadIO m => FilePath -> m ConfigFile
readConfigFile path = io $ do
    putTextLn $ "=> reading ConfigFile: " <> fromString path
    return def

