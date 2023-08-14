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
module Parser.RecordField
  (
      RecordField,
      RecordFields,

      parseRecordFields,
      parseRecordFieldsGet,
      parseRecordFieldsSet,

  ) where

import MyPrelude hiding (takeWhile)
import Data.Text qualified as T
import RecordField
import Parser 


--------------------------------------------------------------------------------
--  parse record fields


-- | parse a path of record fields (allowing rest).
--   * space at beginning is allowed
--   * space between separator is allowed
parseRecordFields :: Text -> Parser [Text]
parseRecordFields sep =
    sepBy1 (skipSpace *> parseRecordField) (skipSpace *> string sep) <?> "Record fields format"
    where
      parseRecordField = liftA2 T.cons (satisfy p0) (takeWhile p1) <?> "Record field format" 
      p0 = \c -> c == '_' || isAsciiLower c -- _ or a-z
      p1 = \c -> c == '_' || isAsciiLower c || isAsciiUpper c || isDigit c || c == '\'' || c == '-' -- _ or a-z or A-Z or 0-9 or - or '


-- | parse a a path of record fields (read format), for example "   version> major  // program major version"
--      * space at beginning is allowed
--      * space between separator is allowed
--      * space at end of line is allowed
--   FIXME: endOfInput and add comments (string com <|> endOfInput)
parseRecordFieldsGet :: Text -> Parser RecordFields
parseRecordFieldsGet sep =
    parseRecordFields sep <* skipSpace <* endOfInput <?> "Record fields format (read)"


-- | parse a path of record fields (write format), example " person -> address -> city := FL-30432  # my address  "
--      * space at beginning is allowed
--      * space between separator and assigner is allowed
--      * space at end of line is allowed
--   FIXME: endOfInput and add comment/end symbol (string com <|> endOfInput)
parseRecordFieldsSet :: Text -> Text -> Parser a -> Parser (RecordFields, a)
parseRecordFieldsSet sep ass parseA =
    liftA2 (,) (parseRecordFields sep <* skipSpace <* string ass) 
               (skipSpace *> parseA) <* skipSpace <* endOfInput <?> "Record fields format (set)"
