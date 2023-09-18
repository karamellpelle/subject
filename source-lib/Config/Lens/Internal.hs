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
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Config.Lens.Internal where

import MyPrelude 

--import Lens.Micro
--import Lens.Micro.Mtl


--------------------------------------------------------------------------------
--  types

-- | error messages
type ErrorString = Text


-- | a record field 
type Field = Text
--data Field = 
--    FieldName { fieldName :: Text } 
    -- | FieldList
    -- | FieldListIx

-- | a path of record fieldpath
type FieldPath = [Field]


-- (a -> b) is a temporary type for Lens
type Lens' a b = (a -> b)


--class Config conf where
--    configGet :: conf -> FieldPath -> b
--    configSet set :: conf -> FieldPath -> b -> conf
--    configOpen :: (MonadError m ErrorString) => FilePath -> m ErrorString conf
--    configSave :: (MonadError m ErrorString) => FilePath -> (a -> 
--    withConfig :: (MonadError m ErrorString) => FilePath -> (conf -> m a) -> m a
--    withConfig file f = configOpen >>= f `catchError` (\err -> print err) 
