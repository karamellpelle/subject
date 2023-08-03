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
module Run.Term
  (
    term,

  ) where

import MyPrelude
import Run
import Cmd

term :: Cmd -> RunM ()
term cmd = do
    putTextLn ""
    putTextLn " * * Running Command in Terminal * * "
    putTextLn ""
    putText   "=> "
    case cmd of 
        CmdConfig  d  -> eatConfig d
        CmdID      d  -> eatID d
        CmdPack    d  -> eatPack d
        CmdSend    d  -> eatSend d
        CmdInfo    d  -> eatInfo d
        CmdGUI     d  -> eatGUI d
        CmdEmpty    -> eatEmpty
        _           -> putTextLn "Terminal does not know how to handle given command"


--------------------------------------------------------------------------------
--  data handlers

-- config
eatConfig :: CmdDataConfig -> RunM ()
eatConfig cmd = do
    putTextLn "User config:"
    case cmdconfigGetSet cmd of
        Just (Get var)        -> putTextLn $ " => " <> var <> ": (unknown)"
        Just (Set var value)  -> putTextLn $ " => " <> var <> " set to " <> value
        Nothing               -> putTextLn $ "(no action)"
        

-- id
eatID :: CmdDataID -> RunM ()
eatID cmd = do
    putTextLn $ "Converting ID (id <-> magic sentence) from "
    forM_ (cmdidArgs cmd) $ putTextLn . fromString  

-- pack
eatPack :: CmdDataPack -> RunM ()
eatPack cmd = do
    putTextLn $ "Packing folder '" <> fromString (cmdpackPath cmd) <> "' to '" <> fromString (cmdpackRecipient cmd) <> "' with SubjectID=" <> fromString (cmdpackSubjectID cmd) 

-- send
eatSend :: CmdDataSend -> RunM ()
eatSend cmd = do
    putTextLn $ "sending folder '" <> fromString (cmdsendPath cmd) <> "' to '" <> fromString (cmdsendRecipient cmd) <> "'"

-- info
eatInfo :: CmdDataInfo -> RunM ()
eatInfo cmd = do
    putTextLn "List info (not implemented)"

-- gui
eatGUI :: CmdDataGUI -> RunM ()
eatGUI cmd = do
    putTextLn $ "Terminal does not handle GUI commands." 
    putTextLn $ "TODO: print gui arguments anyway" 
    putTextLn $ "Hint: Run application with '--gui FRONTEND CMD' (or without CMD)"

eatEmpty :: RunM ()
eatEmpty = do
    putTextLn "Nothing to do - nice :)"
