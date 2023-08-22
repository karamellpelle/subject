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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module RecordField.Internal where


import MyPrelude 

import Type.Reflection
--import Data.Typeable 
-- ^ this module uses a different type 'TypeRep = Internal.SomeTypeRep', 
-- i.e. a homeogenous type, so we can't use that (or the Internal module). but
-- 'module Type.Reflection' contains the correct 'TypeRep a':

import Data.Map.Strict qualified as Map


--------------------------------------------------------------------------------
--  TODO
--  * can the magical HasField be used in some way? 
--    https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/hasfield.html


--------------------------------------------------------------------------------
--  RecordField

-- | a record field with valid name: (_|[a-z])(_|-|'|[a-z]|[A-Z]|[0-9]|)*
type RecordField = Text

-- | a path of record fields
type RecordFields = [RecordField]



--------------------------------------------------------------------------------
-- heterogenous lens container


-- (a -> b) is a temporary type for Lens
type Lens' a b = (a -> b)


-- | wrap a 'Lens' a b' into a heterogenous type from 'a'
data LensFrom a where
    LensTo :: (LookupLensFrom a, LookupLensFrom b) => TypeRep b -> (Lens' a b) -> LensFrom a
    NoLens :: (LookupLensFrom a) => LensFrom a -- FIXME: remove string 


-- | from a type 'a': map name to 'Lens' a x' for arbitrary x
class Typeable a => LookupLensFrom a where
    lookupLensFrom :: RecordField -> LensFrom a
    lookupLensFrom = const NoLens 
    lookupLensFromName :: String
    lookupLensFromName = show (typeRep @a)

--  create a map easily with these two handy functions, this is also the only way 
--  for the user:

-- | lookup table
lensRecordFieldTable :: forall a. LookupLensFrom a => [(RecordField, LensFrom a)] -> (RecordField -> LensFrom a)
lensRecordFieldTable rfls = \rf -> fromMaybe (NoLens @a) $ Map.lookup rf $ fromList rfls

-- | lookup table item
lensRecordField :: forall a b. (LookupLensFrom a, LookupLensFrom b) => RecordField -> Lens' a b -> (RecordField, LensFrom a)
lensRecordField name lensAB =
    (name, LensTo (TypeRep @b) lensAB)



-- TODO: rename:
--class Typeable a => LensTable a where
--    lensTableName :: Text
--    lensTableName = show (TypeRep @a)
--    lensTableLookup :: RecordField -> LensFrom a
--    lensTableLookup = const NoLens
--
--instance Typeable a => LensTable a
-- lensTable lensRecordFieldTable =
-- lensName = lensRecordField


--------------------------------------------------------------------------------
-- compose two LensFrom 

-- | Lens' a b -> Lens' b c -> Lens' a c
--   if codomain of 'a' is the domain of 'b', compose lenses
--
--   see implementation of 'fromDyn' of Data.Dynamic
--
compose :: forall a b . (LookupLensFrom a, LookupLensFrom b) => LensFrom a -> LensFrom b -> LensFrom a
compose (LensTo tb' lensAB) (LensTo tc lensBC)
    | Just HRefl <- tb' `eqTypeRep` tb = LensTo tc $ append lensAB lensBC    
    | otherwise                        = NoLens
    where
      tb = TypeRep @b
      append lensAB lensBC = lensBC . lensAB
compose _ab _bc = NoLens @a 


--------------------------------------------------------------------------------
--  find lens

type ErrorString = Text


lookupLens :: forall a b . (LookupLensFrom a, Typeable b) => RecordFields -> Either ErrorString (Lens' a b)
lookupLens ss = helper ta ss
    where
      ta = typeRep @a
      tb = typeRep @b

      helper :: forall x . (LookupLensFrom x) => TypeRep x -> RecordFields -> Either ErrorString (Lens' x b)
      helper tx (s:ss)  = case lookupLensFrom @x s of
          NoLens            -> Left $ (fromString (lookupLensFromName @x)) <> " has no field " <> quote s 
          LensTo ty' lensXY -> case helper ty' ss of
              Left  err           -> Left err
              Right lensYB        -> Right $ compose lensXY lensYB
      helper tx [] = case tx `eqTypeRep` tb of
              Just HRefl          -> Right (id @b)
              Nothing             -> Left $ "Codomain mismatch: expected " <> show (Fun ta tb) <> ", found " <> show (Fun ta tx)

      compose :: Lens' u0 u1 -> Lens' u1 u2 -> Lens' u0 u2
      compose = flip (.)


-- | TODO: remove
findLensFrom :: forall a . (LookupLensFrom a) => RecordFields -> Either ErrorString (LensFrom a)
findLensFrom ss = helper (typeRep @a) ss
    where
      helper :: forall x . (LookupLensFrom x) => TypeRep x -> RecordFields -> Either ErrorString (LensFrom x)
      helper tx (s:ss)  = case lookupLensFrom @x s of
          NoLens -> Left $ show tx <> " has no field " <> quote s 
          LensTo ty lensX -> case helper ty ss of
              Left err      -> Left err
              Right lensfromY  -> Right (compose (LensTo ty lensX) (lensfromY))
      helper tx [] = Right $ LensTo tx (id @x)
