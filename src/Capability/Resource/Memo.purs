module Purememo.Capability.Resource.Memo where

import Prelude

import Data.Either (Either)
import Halogen (HalogenM, lift)
import Purememo.Data.Memo (MemoId, Memo, MemoWithMetadata)


data MemoQuery
  = All
  | Tags (Array String)

data RequestFailure
  = ParseError String
  | NotFoundError String


class Monad m <= ManageMemo m where
  getMemo :: MemoId -> m (Either RequestFailure MemoWithMetadata)
  getMemos :: MemoQuery -> m (Array MemoWithMetadata)
  createMemo :: Memo -> m MemoWithMetadata
  updateMemo :: MemoId -> Memo -> m (Either RequestFailure MemoWithMetadata)
  deleteMemo :: MemoId -> m Unit
  pinMemo :: MemoId -> m (Either RequestFailure MemoWithMetadata)
  unpinMemo :: MemoId -> m (Either RequestFailure MemoWithMetadata)

instance manageMemoHalogenM :: ManageMemo m => ManageMemo (HalogenM s f g p o m) where
  getMemo = lift <<< getMemo
  getMemos = lift <<< getMemos
  createMemo = lift <<< createMemo
  updateMemo s = lift <<< updateMemo s
  deleteMemo = lift <<< deleteMemo
  pinMemo = lift <<< pinMemo
  unpinMemo = lift <<< unpinMemo
