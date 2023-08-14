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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
module RecordField
  (
      RecordField,
      RecordFields,

      LookupLensFrom,
      makeLensFrom,

      findLens,

  ) where

import MyPrelude hiding (takeWhile)
--import Data.Typeable
import Type.Reflection


--------------------------------------------------------------------------------
--  RecordField

-- | a record field with valid name: (_|[a-z])(_|-|'|[a-z]|[A-Z]|[0-9]|)*
type RecordField = Text

-- | a path with record fields
type RecordFields = [RecordField]

--------------------------------------------------------------------------------
--  find lenses

--data MyLens a b =
--    MyLens (a -> b)
--    deriving (Typeable)

-- (a -> b) is a temporary type for Lens

-- | le
data LensFrom a where
    LensTo :: (Typeable a, Typeable b) => TypeRep b -> (a -> b) -> LensFrom a
    NoLens :: LensFrom a


--makeLensFrom :: Typeable a, b => MyLens a b -> LensFrom a
makeLensFrom :: forall a b . (Typeable a, Typeable b) => (a -> b) -> LensFrom a
makeLensFrom lensAB =
    LensTo (typeRep :: TypeRep b) lensAB

-- | Lens' a b -> Lens' b c -> Lens' a c
--   see implementation of 'fromDyn' of Data.Dynamic
compose :: forall a b . LensFrom a -> LensFrom b -> LensFrom a
compose (LensTo tb' lensAB) (LensTo tc lensBC)
    | Just HRefl <- tb' `eqTypeRep` (typeRep :: TypeRep b) = LensTo tc $ append lensAB lensBC    -- if codomain of 'a' is the domain of 'b', compose lenses
    | otherwise                                            = NoLens
    where
      append lensAB lensBC = lensBC . lensAB


--------------------------------------------------------------------------------
--  record field -> Lens table

class Typeable a => LookupLensFrom a where
    lookupLensFrom :: RecordField -> LensFrom a



--------------------------------------------------------------------------------
--  internal find mechanism

type ErrorString = Text


findLensFrom :: forall a . (LookupLensFrom a, Typeable a) => RecordFields -> Either ErrorString (LensFrom a)
findLensFrom ss = case ss of
    []      -> Left $ "Empty path of record fields" 
    (s:[])  -> helper s
    (s:ss)  -> case helper s of 
        Left  err         -> Left err
        Right lensfromA   -> fmap (compose lensfromA) $ findLensFrom ss
    where
      helper :: forall b . LookupLensFrom b => RecordField -> Either ErrorString (LensFrom b)
      helper s = case lookupLensFrom s of
        NoLens  -> Left $ "Record field does not exist: " <> quote s
        lenstoB -> Right lenstoB


--------------------------------------------------------------------------------
--  findLens

-- | find a lens a -> b from a given record path
findLens :: forall a b . (LookupLensFrom a, LookupLensFrom b) => RecordFields -> Either ErrorString (a -> b)
findLens ss =
    case findLensFrom ss @a of
        Left err  -> Left err
        Right lensfromA -> case lensfromA of
            NoLens          -> Left "The record field path does not exist"
            -- make sure we point to the correct type (codomain):
            LensTo tb' lensA
              | Just HRefl <- tb' `eqTypeRep` (typeRep :: TypeRep b) -> Right lensA
              | otherwise -> Left $ "Codomain type mismatch for record field path " <> show ss <> 
                                   ": Expected codomain " <> show (typeRep :: TypeRep b) <> " of " <>
                                   show (typeRep :: TypeRep a)  <> " -> " <> show (typeRep :: TypeRep tb') <> ""

--TypeRep a -> TypeRep b -> Bool

