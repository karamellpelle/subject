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
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveAnyClass #-}

module Spec.RecordField where

--import Test.Tasty
--import Test.Tasty.SmallCheck 
--import Test.SmallCheck.Series
--
import MyPrelude

import RecordField.Internal
import Type.Reflection
import Text.Pretty.Simple (pPrint)

import GHC.Show qualified as S


data Document = Document {
    docVersion :: UInt
  , docAuthor :: Author
  , docYear :: Int
  , docPublisher :: String
}
docVersionL = docVersion; docAuthorL = docAuthor; docYearL = docYear; docPublisherL = docPublisher
deriving instance S.Show Document
instance Default Document where def = Document { docVersion = 10 , docAuthor = def , docYear = 1996 , docPublisher = "Internets Publishing inc" }

data Author = Author {
    authorName  :: Name
  , authorHomepage :: Maybe String
}
authorNameL  = authorName; authorHomepageL = authorHomepage
deriving instance S.Show Author
instance Default Author where def = Author { authorName = def,authorHomepage=Just "www.haskell.org" }

data Name = Name {
    nameFirst :: String
  , nameLast :: String
  , nameTitle :: Maybe String
}
nameFirstL = nameFirst; nameLastL = nameLast; nameTitleL = nameTitle
deriving instance S.Show Name
instance Default Name where def = Name { nameFirst = "Haskell",nameLast="Curry",nameTitle=Just "Leader" }
--instance Default Name 

instance LookupLensFrom Document where
    lookupLensFrom = lensRecordFieldTable [
        lensRecordField "version" docVersionL
      , lensRecordField "author" docAuthorL
      , lensRecordField "year" docYearL
      , lensRecordField "publisher" docPublisherL
      ]

instance LookupLensFrom Author where
    lookupLensFrom = lensRecordFieldTable [
        lensRecordField "name" authorNameL
      , lensRecordField "homepage" authorHomepageL
      ]

instance LookupLensFrom Name where
    lookupLensFrom = lensRecordFieldTable [
        lensRecordField "first" nameFirstL
      , lensRecordField "last" nameLastL
      , lensRecordField "title" nameTitleL
      ]

instance LookupLensFrom UInt

--------------------------------------------------------------------------------
--  debug


instance S.Show (LensFrom a) 
  where show = showLensFrom

-- | showLensFrom
showLensFrom :: forall a . LensFrom a -> String
showLensFrom (LensTo tb lensAB) = "LensFrom :: " ++ show (Fun (TypeRep @a) tb) <> ",  actual Lens: " ++ (show $ typeOf lensAB) 
showLensFrom NoLens = "NoLens"

--showLensFrom :: forall a . ItherLensFrom a -> String
instance {-# OVERLAPPING #-} S.Show (Either ErrorString (LensFrom a))
    where show (Left err) = toString err
          show (Right lf) = show lf

showEitherLens :: forall a b . (Typeable a, Typeable b) => Either ErrorString (Lens' a b) -> String
showEitherLens e = case e of
    Left  err  -> "Left " <> toString err
    Right f    -> "Right " <> show (typeOf f)

instance {-# OVERLAPPING #-} (Typeable a, Typeable b) => S.Show (Either ErrorString (Lens' a b))
  where show = showEitherLens

whenOneLens :: forall a b . (LookupLensFrom a, LookupLensFrom b) => RecordField -> ((Lens' a b) -> IO ()) -> IO ()
whenOneLens name f =
    case oneLens @a @b name of
        Left err -> pPrint err
        Right lensAB -> f lensAB

