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

module LensTable
  (
      RecordField,
      RecordFields,

      LensTable (..), 
      lensTableFrom,
      lensName,

      lensLookup,

  ) where

import MyPrelude
import LensTable.Internal


--------------------------------------------------------------------------------
--  types with no record fields (empty implementation)

instance LensTable UInt
instance LensTable Int
instance LensTable String
instance LensTable Char
instance Typeable a => LensTable (Maybe a)


