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
    module Data.Char,

    parseTest',

    AttoParse (..)
  ) where

import MyPrelude

import Data.Attoparsec.Text
import Data.Char


class AttoParse a where
    parser :: Parser a

-- | like 'parseTest' but feed end of input
parseTest' :: Show a => Parser a -> Text -> IO ()
parseTest' p str =
    case feed (parse p str) "" of
        Done i r  -> putTextLn $ show r <> "    , rest of input: " <> show i
        Partial f -> print "parseTest': partial result - strange"
        fail      -> print fail

    
--instance Read a => AttoParse a where
--    parser = readEither <$> takeWhile ()


