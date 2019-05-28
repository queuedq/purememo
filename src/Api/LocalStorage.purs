-- | Memo CRUD APIs that use local storage as its database.
module Purememo.Api.LocalStorage
  ( getMemo
  , getMemos
  , createMemo
  , updateMemo
  , deleteMemo
  , pinMemo
  , unpinMemo
  ) where

import Prelude

import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Argonaut (decodeJson, encodeJson, jsonParser, stringify)
import Data.Array (filter, mapMaybe)
import Data.Bifunctor (lmap)
import Data.Either (Either, hush, note)
import Data.Newtype (unwrap)
import Data.Set (fromFoldable, intersection, isEmpty)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now (nowDateTime)
import Purememo.Api.Utils.LocalStorage (Item, getAllItems, getItem, removeItem, setItem)
import Purememo.Capability.Now (class Now)
import Purememo.Capability.Resource.Memo (MemoQuery(..), RequestFailure(..))
import Purememo.Data.Memo (MemoId(..), MemoWithMetadata, Memo)
import Purememo.Data.SerializableDateTime (SerializableDateTime(..), serialize)
import Record (merge)


-- Utility functions

itemToMemo :: Item -> Either RequestFailure MemoWithMetadata
itemToMemo { value } = lmap ParseError $ decodeJson =<< jsonParser value

getAllMemos :: Effect (Array MemoWithMetadata)
getAllMemos = mapMaybe (hush <<< itemToMemo) <$> getAllItems

addMetadata :: forall m. MonadEffect m => Now m => Memo -> m MemoWithMetadata
addMetadata memo = liftEffect $ do
  createdAt <- SerializableDateTime <$> nowDateTime
  let id = MemoId $ serialize createdAt -- TODO: Concat random string at the end
  pure $ merge { id, createdAt, pinned: false } memo

updateMemoWithMetadata :: forall m. MonadEffect m =>
  MemoId -> MemoWithMetadata -> m (Either RequestFailure MemoWithMetadata)
updateMemoWithMetadata id memo = runExceptT do
  stored <- ExceptT $ getMemo id
  let updated = merge memo stored
  liftEffect $ setItem (unwrap updated.id) (stringify $ encodeJson updated)
  pure updated

-- Exported functions

-- | Gets a memo from the memo id.
getMemo :: forall m. MonadEffect m => MemoId -> m (Either RequestFailure MemoWithMetadata)
getMemo id = liftEffect do
  item <- getItem $ unwrap id
  let notFoundError = NotFoundError ("Could not find a memo with id \"" <> unwrap id <> "\".")
  pure $ itemToMemo =<< note notFoundError item

-- | Gets memos that match the query. Ignores invalid memos.
getMemos :: forall m. MonadEffect m => MemoQuery -> m (Array MemoWithMetadata)
getMemos params = liftEffect $ case params of
  All ->
    getAllMemos
  Tags t ->
    let
      tagSet = fromFoldable t
      hasTags :: MemoWithMetadata -> Boolean
      hasTags memo = not $ isEmpty $ intersection tagSet $ fromFoldable memo.tagList
    in filter hasTags <$> getAllMemos

-- | Creates a memo with metadata added.
createMemo :: forall m. MonadEffect m => Now m => Memo -> m MemoWithMetadata
createMemo memo = do
  created <- addMetadata memo
  liftEffect $ setItem (unwrap created.id) (stringify $ encodeJson created)
  pure created

-- | Updates the contents of a memo.
updateMemo :: forall m. MonadEffect m => MemoId -> Memo -> m (Either RequestFailure MemoWithMetadata)
updateMemo id memo = runExceptT do
  stored <- ExceptT $ getMemo id
  let updated = merge memo stored
  liftEffect $ setItem (unwrap updated.id) (stringify $ encodeJson updated)
  pure updated

-- | Deletes a memo.
deleteMemo :: forall m. MonadEffect m => MemoId -> m Unit
deleteMemo id = liftEffect $ removeItem (unwrap id)

-- | Pins a memo.
pinMemo :: forall m. MonadEffect m => MemoId -> m (Either RequestFailure MemoWithMetadata)
pinMemo id = runExceptT do
  memo <- ExceptT $ getMemo id
  ExceptT $ updateMemoWithMetadata id (memo { pinned = true })

-- | Unpins a memo.
unpinMemo :: forall m. MonadEffect m => MemoId -> m (Either RequestFailure MemoWithMetadata)
unpinMemo id = runExceptT do
  memo <- ExceptT $ getMemo id
  ExceptT $ updateMemoWithMetadata id (memo { pinned = false })
