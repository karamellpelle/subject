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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Run.Lenses
  (
    runConfigDocumentFormatL,
    runTestAL,
    runTestBL,
    testaTestBL,
    testaIdL,
    testaNameL,
    testbChar0L,
    testbChar1L,
    testbChar2L,
  ) where

import MyPrelude 
import Run.RunData

import Config.Lens

--import Relude.Extra.Lens

--------------------------------------------------------------------------------
--  tables of record field -> Lens

instance Table RunData where
    lensTableName = "Application data"
    lensTable = lensTableFrom [
            lensName "config-documentformat" runConfigDocumentFormatL
          , lensName "testA" runTestAL
          , lensName "testB" runTestBL
        ]

instance Table TestB where
    lensTable = lensTableFrom [
            lensName "char0" testbChar0L
          , lensName "char1" testbChar1L
          , lensName "char2" testbChar2L
        ]

instance Table TestA where
    lensTable = lensTableFrom [
            lensName "id" testaIdL
          , lensName "name" testaNameL
          , lensName "testB" testaTestBL
        ]

-- lenses
runConfigDocumentFormatL = runConfigDocumentFormat 
runTestAL = runTestA
runTestBL = runTestB
testaTestBL = testaTestB
testaIdL = testaId
testaNameL = testaName
testbChar0L = testbChar0
testbChar1L = testbChar1
testbChar2L = testbChar2
