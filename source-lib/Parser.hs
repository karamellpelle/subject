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
module Parser
  (
    module Data.Attoparsec.Text,
    AttoParse (..)
  ) where

import MyPrelude

import Data.Attoparsec.Text


class AttoParse a where
    parser :: Parser a

--import qualified Data.Attoparsec.Text as A
--attoReadM :: A.Parser a -> ReadM a
--attoReadM p = eitherReader (A.parseOnly p . T.pack)
--
--atto :: AttoParse a => ReadM a
--atto = eitherReader (parseOnly parser . T.pack)
--
--attoOption :: AttoParse a => Option a


