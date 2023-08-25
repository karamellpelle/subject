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
module Config.Lens
  (
      module Config.Lens.Table,
      module Config.Lens.Parser,
      module Config.Lens.Monad,
  ) where

import MyPrelude
--import Config.Lens.Table.Internal
import Config.Lens.Table
import Config.Lens.Parser
import Config.Lens.Monad


--------------------------------------------------------------------------------
--  types with no record fieldpath (empty implementation for scalar types)
