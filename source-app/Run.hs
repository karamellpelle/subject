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
{-# LANGUAGE TemplateHaskell #-}
module Run
  (
    RunData (..),
    RunM,
    RunType (..),

    getRunData,
  ) where

import MyPrelude

import Data.Version
import Data.YAML
import qualified Data.Text as T
import System.Directory
import System.FilePath

import Development.GitRev
import Paths_subject

import Parser as Atto



-- | our Reader monad
type RunM a = ReaderT RunData IO a

    
--------------------------------------------------------------------------------
--  global settings

-- | global data for application
data RunData = 
  RunData
  {
      -- meta information
      runVersion :: Version             -- ^ Version of program
    , runVersionInfo :: String          -- ^ tags, like Git commit
    , runConfigPath :: FilePath         -- ^ user configuration file path
    , runConfigVersion :: (UInt, UInt)  -- ^ file format of configuration file

      -- run settings
    , runVerbose :: Bool          -- ^ be verbose?
    , runType :: RunType          -- ^ type of UI for program

  }


instance Default RunData where
    def = RunData {
        runVersion    = makeVersion [0,0]
      , runVersionInfo = $(gitBranch) ++ " @ " ++ $(gitHash)
      , runConfigPath = "" 
      , runConfigVersion = (0, 0)

      , runVerbose    = False
      , runType       = RunTypeTerm
    }


--------------------------------------------------------------------------------
--  

-- | how to run application (UI) aka frontend
--   FIXME: rename into RunFrontend?
data RunType =
    RunTypeTerm |
    RunTypeGUI String


--------------------------------------------------------------------------------
--  Paths_
--  * https://cabal.readthedocs.io/en/3.10/cabal-package.html#accessing-data-files-from-package-code



--------------------------------------------------------------------------------
--  create data for program execution

type StateR = StateT RunData IO

type ErrorString = Text

type ExceptStateR = ExceptT ErrorString StateR


-- | create initial 'RunData' for program execution
--    0. from default value
--    1. from system config (file)
--    2. from user config (file)
--    3. from environment variables
getRunData :: IO (Either ErrorString RunData)
getRunData = evaluatingStateT def $ 
    runExceptT $ do

        applyConfigFile =<< getGlobalFileName "config.yaml"
        applyConfigFile =<< getLocalFileName "config.yaml"

        -- TODO: environment variables

        -- TODO: other settings

        -- return RunData (or error)
        get

  where
    getGlobalFileName path = io $ getDataFileName ("subject" </> path)

    getLocalFileName path = do
        dir <- io $ getXdgDirectory XdgConfig "subject" 
        return $ dir </> path


-- | read config file and apply on 'RunData'.
--   will terminate program if necessary data couldn't be read
applyConfigFile :: FilePath -> ExceptStateR ()
applyConfigFile path = do
    return ()
    --required yaml "version" $ \v -> modify $ over 
    --optional yaml "receiver > fingerprint" $ (modify $ over (receiverL . fingerprintL))




--------------------------------------------------------------------------------
--  retrieve values in YAML based on a field path


-- | retrieve a required value from configuration file
getRequired :: FromYAML a => Text -> (a -> StateR b) -> ExceptStateR b
getRequired yaml fpath =
    undefined

-- | retrieve an optional value from configuration file
getOptional :: FromYAML a => Text -> (a -> StateR b) -> ExceptStateR b
getOptional = undefined

fieldPath :: Text -> Either ErrorString [Text]
fieldPath v =
    first toText $ parseOnly parseRecordFieldGet v



--------------------------------------------------------------------------------
--  parse record fields

-- parse record field path 
parseRecordFieldGet :: Atto.Parser [Text]
parseRecordFieldGet =
    parseRecordFields ">" <* skipSpace <* endOfInput <?> "valid record recordfield path (get)"

-- parse record recordfield path with set value 
parseRecordFieldSet :: Atto.Parser a -> Atto.Parser ([Text], a)
parseRecordFieldSet parser =
    liftA2 (,) (parseRecordFields ">" <* skipSpace <* string assigner) (skipSpace *> parser) <* skipSpace <* endOfInput <?> "valid record recordfield path (set)"
    where
      assigner = "="

-- parse a path of record recordfield names 
parseRecordFields :: Text -> Atto.Parser [Text]
parseRecordFields separator =
    sepBy1 (skipSpace *> parseRecordField) (skipSpace *> string separator) 
    where
      separator = ">" 
      parseRecordField = liftA2 T.cons (satisfy p0) (Atto.takeWhile p1) <?> "valid record recordfield name" -- FIXME: prevent GHCi from including takeWhile!
      p0 = \c -> c == '_' || isAsciiLower c -- _ or a-z
      p1 = \c -> c == '_' || isAsciiLower c || isAsciiUpper c || isDigit c || c == '\'' -- _ or a-z or A-Z or 0-9 or '



{-
--------------------------------------------------------------------------------
--  LensFrom

data LensFrom a =
    LensFrom (forall b. Lens a b)

mapLensFromRunData = mkMap [
      ("version", runVersionL)
    , ("xx", runXxL)
  ]


--findLens :: forall b . [Text] -> Either ErroString (Lens a b)
findLens :: [Text] -> Either ErroString (Lens a b)
findLens (mword, mwords) =
    case map[mword] of
        Nothing   -> Left $ "'" <> mword <> "' is not a valid keyword of" <> showType a
        Just lens -> Right lens <*> findLens mwords  -- todo apply lens
findLens [] =
    []

-- | retrieve a required value from configuration file
required :: Monad m, FromYAML b => Text -> (a -> Lens a b -> b -> StateT a m ()) -> ExceptStateR b -- or StateR b? or a?
required yaml path handleLensAB =
    mwords <- parsePath pathSeparator path
    lens <- findLens
    --b <- hoistEither (modify $ \a -> over a lens))
    hoistEither $ modify $ \a -> over a lens) $ \b ->
        case findYAML yaml mwords of
            Left err  -> Left $ "no such config record: '" <> path <> "'" <> err
            Right b'  -> handleLensAB a 

    hoistEither $ modify $ \a -> handle \lens a -> \a -> over a lens) $ \b ->
        handleLensAB a b lens

getRunData = do
    required yaml "recipient > fingerprint " $ \lens b -> do
        modify $ over lens $ f b
        --modify $ over lens $ f b append

        return a

parsePath :: Text -> Text -> Either ErrorString [Text]
parsePath sep path =
    case runParser parser of 
        Left err  -> Left $ ""
        right     -> right
    where
      parser

separatorPath :: Text
separatorPath = ">"
--hoist either
-- TODO: use Relude::guarded: Constr <$> guarded valueOK value

-}
