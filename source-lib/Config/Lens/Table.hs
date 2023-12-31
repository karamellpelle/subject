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
{-# OPTIONS_GHC -Wno-orphans #-}

module Config.Lens.Table
  (
      Field,
      FieldPath,

      Table (..), 
      lensTableFrom,
      lensName,

  ) where

import MyPrelude
import Config.Lens.Table.Internal
import Config.Lens.Internal


--------------------------------------------------------------------------------
--  there are no tables for primitive types types (exect for maybe constructors?) 

instance Table UInt
instance Table Int
instance Table String
instance Table Char
instance Typeable a => Table (Maybe a)
