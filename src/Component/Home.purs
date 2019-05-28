module Purememo.Component.Home where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Network.RemoteData (RemoteData(..))
import Purememo.Capability.Resource.Memo (class ManageMemo, MemoQuery(..), createMemo, getMemos)
import Purememo.Component.MemoList (memoList)
import Purememo.Component.NewMemoForm (newMemoForm)
import Purememo.Data.Memo (MemoWithMetadata, Memo)


type State =
  { memos :: RemoteData String (Array MemoWithMetadata)
  }

data Query a
  = Initialize a
  | CreateMemo a


home :: forall m. MonadAff m => ManageMemo m =>
  H.Component HH.HTML Query Unit Void m
home =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }
  where

  initialState :: State
  initialState = { memos: NotAsked }

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Initialize a -> do
      state <- H.get
      memos <- getMemos All
      let nextState = state { memos = Success memos }
      H.put nextState
      pure a

    CreateMemo a -> do
      let
        memo :: Memo
        memo =
          { title: "Dummy title"
          , body: "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
          , tagList: ["lorem", "ipsum", "dolor", "sit", "amet"]
          }
      _ <- createMemo memo
      allMemos <- getMemos All
      H.modify_ $ _ { memos = Success allMemos }
      pure a

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Purememo" ]
      , memoList state.memos
      , newMemoForm CreateMemo
      ]

