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
{-# OPTIONS_GHC -Wno-unused-imports #-}
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
import RecordField

import Type.Reflection
import Text.Pretty.Simple (pPrint, pPrintString)

import GHC.Show qualified as S

doc0 :: Document
doc0 = def { docYear = 2023, docPublisher = "KPelle Pub. Inc." }

doc1 :: Document 
doc1 = def { docYear = 1999, docAuthor = author0 { authorHomepage = Just "www.duckduckgo.com" }}

author0 :: Author
author0 = def { authorName = def { nameFirst = "Larry", nameLast = "Laffer", nameTitle = Just "virgin" } }

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

instance LensTable Document where
    lensTable = lensTableFrom [
        lensName "version" docVersionL
      , lensName "author" docAuthorL
      , lensName "year" docYearL
      , lensName "publisher" docPublisherL
      ]

instance LensTable Author where
    lensTable = lensTableFrom [
        lensName "name" authorNameL
      , lensName "homepage" authorHomepageL
      ]

--instance LensTable where
--    lensTableName = "Author"
--    lensTableLookup = lensTable [ lensTableName "name" authorNameL ]

instance LensTable Name where
    --lensTableName = "Name"
    lensTable = lensTableFrom [
        lensName "first" nameFirstL
      , lensName "last" nameLastL
      , lensName "title" nameTitleL
      ]


--------------------------------------------------------------------------------
--  debug

instance S.Show (LensFrom a) 
  where show = showLensFrom

-- | showLensFrom
showLensFrom :: forall a . LensFrom a -> String
showLensFrom (LensTo tb lensAB) = "LensFrom :: " ++ show (Fun (TypeRep @a) tb) <> ",  actual Lens: " ++ (show $ typeOf lensAB) 
showLensFrom (NoLens) = "NoLens " ++ show (TypeRep @a)


--showLensFrom :: forall a . ItherLensFrom a -> String
--instance {-# OVERLAPPING #-} S.Show (Either ErrorString (LensFrom a))
--    where show (Left err) = toString err
--          show (Right lf) = show lf

showEitherLens :: forall a b . (Typeable a, Typeable b) => Either ErrorString (Lens' a b) -> String
showEitherLens e = case e of
    Left  err  -> "LEFT  : " <> toString err
    Right f    -> "RIGHT : " <> show (typeOf f)

instance {-# OVERLAPPING #-} (Typeable a, Typeable b) => S.Show (Either ErrorString (Lens' a b))
  where show = showEitherLens

applyLens :: (LensTable a, Typeable b) => RecordFields -> a -> (b -> IO ()) -> IO ()
applyLens ss a f = case lensLookup ss of
    Left err      -> print err
    Right lensAB  -> f $ lensAB a


--------------------------------------------------------------------------------
--  GHCI

-- applyLens ["author", "name", "title"] doc0 (pPrint @IO @(Maybe String))

