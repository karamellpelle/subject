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
module MyPrelude
    (
    module Relude,    
    module Data.Default,

    UInt,
    fI,
    succ',
    io,
) where

import Relude
import Data.Default


--------------------------------------------------------------------------------
--  

-- | using Int's is wrong, it's overkill like using complex numbers.
--   since "sets" is a natural logical concept, we need a data structure for
--   that, namely UInt. or should we use Peano numbers? 
--   the natural numbers \mathbf{N} is the structure "sets of finite size"
--   the integer numbers \mathbf{Z} is the structure "steps in direction"
type UInt = Word    

--------------------------------------------------------------------------------
--  

io :: MonadIO m => IO a -> m a 
io = liftIO

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

-- | we don't want 'succ' from Prelude since it do bounds checking
succ' :: Num a => a -> a
succ' = \a -> a + 1
