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
module Data.Recipient
  (
    Recipient (..),
    
  ) where

import MyPrelude

type Fingerprint = String

-- FIXME: TMP
data Recipient = Recipient {
      recipientName :: String
    , recipientFingerprint :: Fingerprint
    , recipientPriority :: UInt
 }

deriving instance Show Recipient

instance Default Recipient where
    def = Recipient "<NoRecipient>" "<NoFingerprint>" 0 
