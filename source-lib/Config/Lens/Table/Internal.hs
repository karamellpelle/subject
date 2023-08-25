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
module Config.Lens.Table.Internal where


import MyPrelude 

import Data.Map.Strict qualified as Map
import Type.Reflection
-- ^ 'Type.Reflection' contains the homogenous 'TypeRep a' we need, 
--    not the heterogenous in Data.Typeable

import Config.Lens.Internal


--------------------------------------------------------------------------------
--  TODO
--  * can the magical HasField be used in some way? 
--    https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/hasfield.html


--------------------------------------------------------------------------------
-- heterogenous lens container

-- | wrap a 'Lens' a b' into a heterogenous type from 'a'
data LensFrom a where
    LensTo :: (Table a, Table b) => TypeRep b -> (Lens' a b) -> LensFrom a
    NoLens :: (Table a) => LensFrom a


-- | from a type 'a': map name to 'Lens' a x' for arbitrary x
--   instance default implementation for a type with no record fieldpath
class Typeable a => Table a where
    lensTableName :: String
    lensTableName = show (typeRep @a)
    lensTable :: Field -> LensFrom a
    lensTable = const NoLens 



--------------------------------------------------------------------------------
-- compose two LensFrom 

-- | Lens' a b -> Lens' b c -> Lens' a c
--   if codomain of 'a' is the domain of 'b', compose lenses
--
--   see implementation of 'fromDyn' of Data.Dynamic
--
--   TODO: remove?
compose :: forall a b . (Table a, Table b) => LensFrom a -> LensFrom b -> LensFrom a
compose (LensTo tb' lensAB) (LensTo tc lensBC)
    | Just HRefl <- tb' `eqTypeRep` tb = LensTo tc $ append lensAB lensBC    
    | otherwise                        = NoLens
    where
      tb = TypeRep @b
      append lensAB lensBC = lensBC . lensAB
compose _ab _bc = NoLens @a 

--------------------------------------------------------------------------------
--  user interface

-- | lookup table
lensTableFrom :: forall a. Table a => [(Field, LensFrom a)] -> (Field -> LensFrom a)
lensTableFrom rfls = \rf -> fromMaybe (NoLens @a) $ Map.lookup rf $ fromList rfls

-- | lookup table item
lensName :: forall a b. (Table a, Table b) => Field -> Lens' a b -> (Field, LensFrom a)
lensName name lensAB =
    (name, LensTo (TypeRep @b) lensAB)


-- | find a lens from type 'a' to type 'b' from given path of record fieldpath
lensLookup :: forall a b . (Table a, Typeable b) => FieldPath -> Either ErrorString (Lens' a b)
lensLookup ss = helper ta ss
    where
      ta = typeRep @a
      tb = typeRep @b

      helper :: forall x . (Table x) => TypeRep x -> FieldPath -> Either ErrorString (Lens' x b)
      helper tx (s:ss)  = case lensTable @x s of
          NoLens            -> Left $ (fromString (lensTableName @x)) <> " has no field " <> quote s 
          LensTo ty' lensXY -> case helper ty' ss of
              Left  err           -> Left err
              Right lensYB        -> Right $ compose lensXY lensYB
      helper tx [] = case tx `eqTypeRep` tb of
              Just HRefl          -> Right (id @b)
              Nothing             -> Left $ "Codomain mismatch: expected " <> show (Fun ta tb) <> ", found " <> show (Fun ta tx)

      compose :: Lens' u0 u1 -> Lens' u1 u2 -> Lens' u0 u2
      compose = flip (.)
