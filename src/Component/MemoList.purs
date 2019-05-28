module Purememo.Component.MemoList where

import Prelude

import Data.Array (length)
import Halogen.HTML as HH
import Network.RemoteData (RemoteData(..))
import Purememo.Data.Memo (MemoWithMetadata)
import Purememo.Data.SerializableDateTime (serialize)

memoList
  :: forall p i
   . RemoteData String (Array MemoWithMetadata)
  -> HH.HTML p (i Unit)
memoList = case _ of
  NotAsked ->
    text "Memos not yet loaded"
  Loading ->
    text "Loading..."
  Failure error ->
    text ("Error loading memos: " <> error)
  Success memos | length memos == 0 ->
    text "You have no memos yet. Add a memo!"
  Success memos ->
    HH.ul_
      (memoItem <$> memos)

  where
    text string =
      HH.div_
        [ HH.text string ]

memoItem :: forall p i. MemoWithMetadata -> HH.HTML p (i Unit)
memoItem memo =
  HH.li_
    [ HH.h2_
        [ HH.text memo.title ]
    , HH.time_
        [ HH.text (serialize memo.createdAt) ]
    , HH.p_
        [ HH.text memo.body ]
    , HH.div_
        [ HH.text "Tags:" ]
    , HH.ul_
        (tagItem <$> memo.tagList)
    ]

  where
    tagItem tag =
      HH.li_
        [ HH.text tag ]
