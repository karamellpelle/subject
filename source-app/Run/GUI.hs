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
module Run.GUI
  (
    gui,
    
    guiDefaultFrontend,
  ) where

import MyPrelude

import Run
import Cmd


gui :: Frontend -> Cmd -> RunM ()
gui frontend cmd = do

    case frontend of
        "GTK3"     -> putTextLn "Running GUI with GTK3 as frontend"
        "monomer"  -> putTextLn "Running GUI with monomer as frontend"
        _          -> putTextLn $ quote (toText frontend) <> " is not a supported frontend"

    pure ()
    --win <- new Gtk.Window [ #title := "Hi there" ]

guiDefaultFrontend :: Frontend
guiDefaultFrontend = "monomer"



