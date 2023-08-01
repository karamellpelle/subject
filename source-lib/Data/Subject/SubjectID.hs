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
module Data.Subject.SubjectID
  (
    SubjectID (..),
    subjectIx,
    subjectMagicSentence

  ) where

import MyPrelude
import Data.Bits

--------------------------------------------------------------------------------
--  how to name a subject

data SubjectID =
    SubjectIx UInt |
    SubjectLetters Char Char Char Char

instance Default SubjectID where
    def = SubjectIx 0


-- | get Ix of SubjectID
subjectIx :: SubjectID -> UInt
subjectIx (SubjectIx ix)     = ix
subjectIx (SubjectLetters c0 c1 c2 c3) = ixFromLetters c0 c1 c2 c3


-- | get magic sentence of SubjectID 
subjectMagicSentence :: SubjectID -> Text
subjectMagicSentence (SubjectLetters x0 x1 x2 x3) = sentenceFromLetters x0 x1 x2 x3
subjectMagicSentence (SubjectIx ix)     = sentenceFromIx ix 


--------------------------------------------------------------------------------
--  sentence

sentenceFromLetters :: Char -> Char -> Char -> Char -> Text
sentenceFromLetters x0 x1 x2 x3 = do
    --str0 <- map x0
    --str0 <- map x0
    --str0 <- map x0
    --str0 <- map x0
    --pure $ case 
    "TODO: implement sentenceFromLetters"

-- | create senctenc from Ix
sentenceFromIx :: UInt -> Text
sentenceFromIx ix = 
    let (x0, x1, x2, x3) = lettersFromIx ix
    in  sentenceFromLetters x0 x1 x2 x3


--------------------------------------------------------------------------------
--  a non-trivial bijection Ix <-> Letters to create a variation 
--  of magic sentences

-- | TODO: write bijection function
biject :: UInt -> UInt
biject = id

-- | TODO: write bijection function
biject' :: UInt -> UInt
biject' = id

lettersFromIx :: UInt -> (Char, Char, Char, Char)
lettersFromIx ix =
    let ix' = biject ix
        a0 = (ix' .>>. 0)  .&. 0xff
        a1 = (ix' .>>. 8)  .&. 0xff
        a2 = (ix' .>>. 16) .&. 0xff
        a3 = (ix' .>>. 24) .&. 0xff
    in  (c a0, c a1, c a2, c a3)
    where
      c a = chr $ fromIntegral a

ixFromLetters :: Char -> Char -> Char -> Char -> UInt
ixFromLetters c0 c1 c2 c3 =
    let a0 = (a c0) .<<. 0
        a1 = (a c1) .<<. 8
        a2 = (a c2) .<<. 16
        a3 = (a c3) .<<. 24
    in biject' (a0 .|. a1 .|. a2 .|. a3)
    where
      a c = fromIntegral $ ord c


