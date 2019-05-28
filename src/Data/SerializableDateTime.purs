module Purememo.Data.SerializableDateTime where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.DateTime (DateTime)
import Data.Either (Either, note)
import Data.Newtype (class Newtype, unwrap)
import Data.PreciseDateTime (fromDateTime, fromRFC3339String, toDateTimeLossy, toRFC3339String)
import Data.RFC3339String (RFC3339String(..))


newtype SerializableDateTime = SerializableDateTime DateTime

derive instance newtypeSerializableDateTime :: Newtype SerializableDateTime _

instance decodeJsonSerializableDateTime :: DecodeJson SerializableDateTime where
  decodeJson = deserialize <=< decodeJson

instance encodeJsonSerializableDateTime :: EncodeJson SerializableDateTime where
  encodeJson = encodeJson <<< serialize


deserialize :: String -> Either String SerializableDateTime
deserialize =
  map SerializableDateTime
    <<< map toDateTimeLossy
    <<< note "Could not parse RFC3339 string"
    <<< fromRFC3339String
    <<< RFC3339String

serialize :: SerializableDateTime -> String
serialize =
  unwrap
    <<< toRFC3339String
    <<< fromDateTime
    <<< unwrap
