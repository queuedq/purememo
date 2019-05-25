module Purememo.Data.Memo where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Purememo.Data.SerializableDateTime (SerializableDateTime)
import Type.Row (type (+))


newtype MemoId = MemoId String

derive instance newtypeMemoId :: Newtype MemoId _
derive instance genericMemoId :: Generic MemoId _
derive instance eqMemoId :: Eq MemoId
derive instance ordMemoId :: Ord MemoId

derive newtype instance encodeJsonMemoId :: EncodeJson MemoId
derive newtype instance decodeJsonMemoId :: DecodeJson MemoId

instance showMemoId :: Show MemoId where
  show = genericShow


type MemoRep row =
  ( title :: String
  , body :: String
  , tagList :: Array String
  | row
  )

type MemoMetadataRep row =
  ( id :: MemoId
  , createdAt :: SerializableDateTime
  , pinned :: Boolean
  | row
  )

type Memo = { | MemoRep () }
type MemoWithMetadata = { | MemoRep + MemoMetadataRep () }


-- decodeMemo :: Json -> Either String MemoWithMetadata
-- decodeMemo json = do
--   obj <- decodeJson json
--   title <- obj .: "title"
--   body <- obj .: "body"
--   tagList <- obj .: "tagList"

--   id <- obj .: "id"
--   createdAt <- obj .: "createdAt"
--   pinned <- obj .: "pinned"

--   pure $ { title, body, tagList, id, createdAt, pinned }
