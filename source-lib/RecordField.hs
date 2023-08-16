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
--{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
module RecordField
  (
      RecordField,
      RecordFields,

      --LensFrom,
      LookupLensFrom (..),
      lensRecordFieldTable,
      lensRecordField,

      --findLens,
      oneLens,

      -- debug
      LensFrom (..),
      showLensFrom,
      showEitherLensFrom,
      showEitherLens,
      
  ) where

import MyPrelude 
-- import Data.Typeable uses a different type 'TypeRep = Internal.SomeTypeRep', 
-- i.e. a homeogenous type, so we can't use that (or the Internal module). but
-- 'module Type.Reflection' contains the correct 'TypeRep a':
import Type.Reflection
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
--  RecordField

-- | a record field with valid name: (_|[a-z])(_|-|'|[a-z]|[A-Z]|[0-9]|)*
type RecordField = Text

-- | a path with record fields
type RecordFields = [RecordField]

-- (a -> b) is a temporary type for Lens
type Lens' a b = (a -> b)


--------------------------------------------------------------------------------
-- heterogenous lens container

-- if the real Lens' type creates problem, wrap them in a type
--data PackedLens a b =
--    PackedLens Lens'
--packLens :: Lens' -> PackedLens a b
--packLens PackedLens

-- | wrap a 'Lens' a b' into a heterogenous type from 'a'
data LensFrom a where
    --NoLens :: (LookupLensFrom a, LookupLensFrom a) => LensFrom a
    LensTo :: (Typeable a, Typeable b) => TypeRep b -> (a -> b) -> LensFrom a
    NoLens :: (Typeable a) => LensFrom a


--------------------------------------------------------------------------------
--  debug

showLensFrom :: forall a . LensFrom a -> String
showLensFrom (LensTo tb lensAB) = "LensFromTo: " ++ show (Fun (TypeRep @a) tb) <> " (real lens: " ++ (show $ typeOf lensAB) ++ ")"
showLensFrom NoLens = "NoLens"

--showLensFrom :: forall a . ItherLensFrom a -> String
showEitherLensFrom e = case e of
    Left  err   -> toString err
    Right lf    -> showLensFrom lf

showEitherLens e = case e of
    Left  err   -> toString err
    Right f    -> show (typeOf f)



--------------------------------------------------------------------------------
--  

-- | Lens' a b -> Lens' b c -> Lens' a c
--   see implementation of 'fromDyn' of Data.Dynamic
compose :: forall a b . LensFrom a -> LensFrom b -> LensFrom a
compose (LensTo tb' lensAB) (LensTo tc lensBC)
    | Just HRefl <- tb' `eqTypeRep` tb = LensTo tc $ append lensAB lensBC    -- if codomain of 'a' is the domain of 'b', compose lenses
    | otherwise                        = NoLens
    where
      tb = TypeRep @b
      append lensAB lensBC = lensBC . lensAB


--------------------------------------------------------------------------------
--  from a type 'a': map name to 'Lens' a x'

class Typeable a => LookupLensFrom a where
    lookupLensFrom :: RecordField -> LensFrom a
    lookupLensFrom = const NoLens


--------------------------------------------------------------------------------
--  create a map easily with these two handy functions (this is also the only way for the user!)

-- | lookup table
lensRecordFieldTable :: forall a. Typeable a => [(RecordField, LensFrom a)] -> (RecordField -> LensFrom a)
lensRecordFieldTable rfls = \rf -> fromMaybe (NoLens @a) $ Map.lookup rf $ fromList rfls

-- | lookup table item
lensRecordField :: forall a b. (Typeable a, Typeable b) => RecordField -> Lens' a b -> (RecordField, LensFrom a)
lensRecordField name lensAB =
    (name, LensTo (TypeRep @b) lensAB)


--------------------------------------------------------------------------------
--  internal find mechanism

type ErrorString = Text

{-
findLensFrom :: forall a . (LookupLensFrom a, Typeable a) => RecordFields -> Either ErrorString (LensFrom a)
findLensFrom ss = case ss of
    []      -> Left $ "Empty path of record fields" 
    (s:[])  -> oneLens s
    (s:ss)  -> case oneLens s of 
        Left  err         -> Left err
        Right lensfromA   -> fmap (compose lensfromA) $ findLensFrom ss
    where
      helper :: forall b . LookupLensFrom b => RecordField -> Either ErrorString (LensFrom b)
      helper s = case lookupLensFrom s of
        NoLens  -> Left $ "Record field does not exist: " <> quote s
        lenstoB -> Right lenstoB
-}

--------------------------------------------------------------------------------
--  findLens

eqT :: TypeRep a -> TypeRep b -> Bool
eqT = \ta tb -> case ta `eqTypeRep` tb of
    Just HRefl  -> True
    _           -> False
{-
-- | find a lens a -> b from a given record path
findLens :: forall a b . (LookupLensFrom a, Typeable b) => RecordFields -> Either ErrorString (a -> b)
findLens ss = case ss of
    []      -> Left $ "Empty path of record fields" 
    (s:[])  -> helper ta s
    --(s:ss)  -> case helper s of 
    --    Left  err         -> Left err
    --    Right lensfromA   -> fmap (compose lensfromA) $ findLensFrom ss
   
    where
      ta = typeRep :: TypeRep a
      tb = typeRep :: TypeRep b

      helper :: forall x . LookupLensFrom x => TypeRep x -> RecordField -> Either ErrorString (x -> b)
      helper tx s = case lookupLensFrom @x s of
          NoLens  -> Left $ "Record field does not exist: " <> quote s
          LensTo tb' lensA
            | Just HRefl <- tb' `eqTypeRep` tx -> Right lensA
            | otherwise -> Left $ "Codomain type mismatch for record field path " <> --show ss <> 
                                 ": Expected codomain " <> show tb <> " of " <> show ta  <> " -> " <> show tb' <> ""
-}

-- TODO: use 'eqT'
oneLens :: forall a b . (LookupLensFrom a, LookupLensFrom b) => RecordField -> Either ErrorString (Lens' a b)
oneLens s = case lookupLensFrom @a s of
    NoLens  -> Left $ "Record field does not exist: " <> quote s
    LensTo tb' lensAB
      | Just HRefl <- tb' `eqTypeRep` tb -> Right lensAB
      | otherwise -> Left $ "Record field type mismatch: found " <> quote s <> 
                            " :: " <> show (Fun ta tb') <> ", expected " <> show (Fun ta tb)
    where
      ta = typeRep @a
      tb = typeRep @b

