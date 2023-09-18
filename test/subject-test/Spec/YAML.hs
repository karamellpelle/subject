module Spec.YAML where

import MyPrelude
import Relude.Extra.Map
import Data.YAML
import Data.Recipient
import System.Directory
import Text.Pretty.Simple (pPrint, pPrintString)
import Config.Lens.Internal

-- document format: YAML
type DocYAML = Doc (Node Pos)


makeRecipient :: Text -> Text -> UInt -> Recipient
makeRecipient n f p =
    Recipient (toString n) (toString f) p

instance FromYAML Recipient where
    parseYAML = withMap "Recipient" $ \m ->
        makeRecipient <$> m .: "name"
                      <*> m .: "fingerprint"
                      <*> m .:? "priority" .!= 0

docString :: LByteString
docString = encodeUtf8 @Text @LByteString $ unlines [
  "",
  "",
  "",
  "",
  ""
  ]

docStr :: LByteString
docStr = "- name: Erik Weisz\n  age: 52\n  magic: True\n- name: Mina Crandon\n  age: 53"

data Person = Person
    { name  :: Text
    , age   :: Int
    , magic :: Bool
    } deriving Show

instance FromYAML Person where
   parseYAML = withMap "Person" $ \m -> Person
       <$> m .: "name"
       <*> m .: "age"
       <*> m .:? "magic" .!= False

decodeDoc :: LByteString -> Either (Pos, String) [[Person]]
decodeDoc = decodeYAMLStr
--decodeYAMLStr :: FromYAML a => LByteString -> Either Text [a]
decodeYAMLStr :: FromYAML a => LByteString -> Either (Pos, String) [[a]]
--decodeYAMLStr :: FromYAML a => LByteString -> Either Text [a]
decodeYAMLStr bs =
  decode bs
  --case decode bs of
  --    --Left (pos, str) -> Left (pos , "./tmp/test.yaml" ++ prettyPosWithSource pos  bs " error" ++ str)
  --    --Right a -> Right a
  --    Left (pos, str) -> do
  --        Left $ toText $ "./tmp/test.yaml" ++ prettyPosWithSource pos  bs " error" ++ str
  --    Right doxs      -> 
  --        Ri(ght (doxs)

--decodeYAML :: IO [DocYAML]
decodeYAML :: IO (Either Text [[Recipient]])
--decodeYAML :: IO (Either (Pos, String) [[Recipient]])
decodeYAML = do
    readFileLBS "tmp/test.yaml" >>= \bs -> 
      --case decodeNode bs of
      case decode bs of
          Left (pos, str) -> do
              --return $ one $ Doc $ Scalar pos $ SStr $ toText $ "./tmp/test.yaml" ++ prettyPosWithSource pos  bs " error" ++ str
              return $ Left $ toText $ "./tmp/test.yaml" ++ prettyPosWithSource pos  bs " error" ++ str
          Right doxs      -> 
              return $ Right (doxs)

decodeDocYAML :: IO (Either Text DocYAML)
decodeDocYAML = do
    readFileLBS "tmp/test.yaml" >>= \bs -> 
      case decodeNode bs of
          Left (pos, str) -> do
              --return $ one $ Doc $ Scalar pos $ SStr $ toText $ "./tmp/test.yaml" ++ prettyPosWithSource pos  bs " error" ++ str
              return $ Left $ toText $ "./tmp/test.yaml" ++ prettyPosWithSource pos  bs " error" ++ str
          Right []        -> do
              return $ Left $ "No document found in file"
          Right (d:ds)    -> do
              return $ Right d

peekDoc :: FromYAML a => DocYAML -> FieldPath -> Either Text a
peekDoc (Doc node) fs = 
    helper node fs
    where
      --helper :: Node Pos -> FieldPath -> Either Text a
      helper node fs = case fs of
          []        -> case parseEither (parseYAML node) of
              Left (pos, str) -> Left $ toText $ "peekDoc parse error: " ++ str
              Right a         -> Right a

          (f:fs)   -> case look node f of 
              Right node' -> helper node' fs
              Left err    -> Left err

look :: Node Pos -> Text -> Either Text (Node Pos)
look node key = 
    case node of 
        Mapping _ _ map -> 
            case lookup (Scalar fakePos (SStr key)) map of
                Just node'  -> Right node'
                Nothing     -> Left $ "mapping not found: " <> quote key

        _               -> Left $ "Node is not a mapping"

fakePos :: Pos
fakePos = Pos { posByteOffset = -1 , posCharOffset = -1  , posLine = 1 , posColumn = 0 }
--
--peekNode = do
--    Doc node <- lift decodeDocYAML
--    peek
