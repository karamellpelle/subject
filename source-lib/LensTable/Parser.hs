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
module LensTable.Parser
  (
      Style (..),
      style0, style1,

      parseFieldPath,
      parseFieldPathGet,
      parseFieldPathSet,

      lensParse,

  ) where

import MyPrelude hiding (takeWhile) -- TODO: remove takeWhile  (why is it exposed?)
import Data.Text qualified as T

import LensTable.Internal
import Parser 


lensParse :: (LensTable a, Typeable b) => Style -> Text -> Either ErrorString (Lens' a b)
lensParse style str = do
    path <- eitherParse (parseFieldPath style) str
    lensLookup path


--------------------------------------------------------------------------------
--  Style

data Style =
    Style {
        styleSeparator :: Text
      , styleAssigner :: Text
    }

instance Default Style where
    def = style0


-- | telephone > alphabet > rat = value
style0 :: Style
style0 = Style {
        styleSeparator = ">"
      , styleAssigner = "="
    }


-- | telephone->alphabet->rat := value
style1 :: Style
style1 = Style {
        styleSeparator = "->"
      , styleAssigner = ":="
    }


--------------------------------------------------------------------------------
--  parse record fieldpath
--    - valid names: (_|[a-z])(_|-|'|[a-z]|[A-Z]|[0-9]|)*

-- | parse a path of record fieldpath (allowing rest).
--   * space at beginning is allowed
--   * space between separator is allowed
parseFieldPath :: Style -> Parser FieldPath
parseFieldPath style =
    sepBy1 (skipSpace *> parseField) (skipSpace *> string (styleSeparator style)) <?> "Record fieldpath format"
    where
      parseField = liftA2 T.cons (satisfy p0) (takeWhile p1) <?> "Record field format" 
      p0 = \c -> c == '_' || isAsciiLower c -- _ or a-z
      p1 = \c -> c == '_' || isAsciiLower c || isAsciiUpper c || isDigit c || c == '\'' || c == '-' -- _ or a-z or A-Z or 0-9 or - or '


-- | parse a a path of record fieldpath (read format), for example "   version> major  // program major version"
--      * space at beginning is allowed
--      * space between separator is allowed
--      * space at end of line is allowed
--   FIXME: endOfInput and add comments (string com <|> endOfInput)
parseFieldPathGet :: Style -> Parser FieldPath
parseFieldPathGet style =
    parseFieldPath style <* skipSpace <* endOfInput <?> "Record fieldpath format (read)"


-- | parse a path of record fieldpath (write format), example " person -> address -> city := FL-30432  # my address  "
--      * space at beginning is allowed
--      * space between separator and assigner is allowed
--      * space at end of line is allowed
--   FIXME: endOfInput and add comment/end symbol (string com <|> endOfInput)
parseFieldPathSet :: Style -> Parser a -> Parser (FieldPath, a)
parseFieldPathSet style parseA =
    liftA2 (,) (parseFieldPath style <* skipSpace <* string (styleAssigner style))
               (skipSpace *> parseA) <* skipSpace <* endOfInput <?> "Record fieldpath format (set)"


