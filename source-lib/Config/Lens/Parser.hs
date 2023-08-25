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
module Config.Lens.Parser
  (
      ParserFormat (..),
      parserformatDot, parserformatBracket, parserformatPointer,


  ) where

import MyPrelude
import Config.Lens.Internal
import Config.Lens.Table
import Config.Lens.Parser.Internal



--lensParse :: (Table a, Typeable b) => ParserFormat -> Text -> Either ErrorString (Lens' a b)
--lensParse parserformat str = do
--    path <- eitherParse (parseFieldPath parserformat) str
--    lensLookup path
--
--
