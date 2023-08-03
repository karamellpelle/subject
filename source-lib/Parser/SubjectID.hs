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
module Parser.SubjectID
  (

  ) where

import MyPrelude

import Data.Attoparsec.Text

import Data.Subject.SubjectID
import Parser

--------------------------------------------------------------------------------
--  parse Text into SubjectID
--   a) number
--   b) compact uppercase letters,                  example: ABCD
--   c) compact words with first letter uppercase,  example: AnxiousBearCreatesDirectly
--   d) space separated sentence,                   example: anxious bear creates directly

instance AttoParse SubjectID where
    parser = parseSubjectID

parseSubjectID :: Parser SubjectID
parseSubjectID =
    parseSubjectIx <|> parseSubjectLetters

-- | number
parseSubjectIx :: Parser SubjectID
parseSubjectIx =
    SubjectIx <$> decimal

-- | letters. FIXME: parse with bijection
parseSubjectLetters :: Parser SubjectID
parseSubjectLetters =
    parseLetters <|> parseWords <|> parseSentence <?> "valid subject name"
    where
      parseLetters = do
          c0 <- satisfy (inClass "A-P")
          c1 <- satisfy (inClass "A-P")
          c2 <- satisfy (inClass "A-P")
          c3 <- satisfy (inClass "A-P")
          pure $ SubjectLetters c0 c1 c2 c3
      parseWords = do
          c0 <- pW
          c1 <- pW
          c2 <- pW
          c3 <- pW
          pure $ SubjectLetters c0 c1 c2 c3
      parseSentence = do
          c0 <- pw 
          c1 <- pw 
          c2 <- pw 
          c3 <- pw 
          pure $ SubjectLetters c0 c1 c2 c3
      pW = pure 'A'
      --pw = <$> skipSpace *> many1 letter
      pw = pure 'a'


