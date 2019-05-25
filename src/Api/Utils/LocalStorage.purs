-- | A handy wrapper for the local storage API.
module Purememo.Api.Utils.LocalStorage
  ( Item
  , getAllKeys
  , getItem
  , getItems
  , getAllItems
  , filterItemsByKey
  , filterItemsByValue
  , setItem
  , removeItem
  ) where

import Prelude

import Data.Array (catMaybes, filter)
import Data.Enum (enumFromTo)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage as S


type Item = { key :: String, value :: String }


getAllKeys :: Effect (Array String)
getAllKeys = do
  storage <- window >>= localStorage
  len <- S.length storage
  let indices = enumFromTo 0 (len - 1)
  catMaybes <$> traverse (flip S.key storage) indices

getItem :: String -> Effect (Maybe Item)
getItem key = do
  storage <- window >>= localStorage
  value <- S.getItem key storage
  pure $ map { key, value: _ } value

getItems :: Array String -> Effect (Array Item)
getItems keys = catMaybes <$> traverse getItem keys

getAllItems :: Effect (Array Item)
getAllItems = getAllKeys >>= getItems

filterItemsByKey :: (String -> Boolean) -> Effect (Array Item)
filterItemsByKey f = filter f <$> getAllKeys >>= getItems

filterItemsByValue :: (String -> Boolean) -> Effect (Array Item)
filterItemsByValue f = filter (f <<< _.value) <$> getAllItems

setItem :: String -> String -> Effect Unit
setItem key value = window >>= localStorage >>= S.setItem key value

removeItem :: String -> Effect Unit
removeItem item = window >>= localStorage >>= S.removeItem item
