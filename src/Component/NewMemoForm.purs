module Purememo.Component.NewMemoForm where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

newMemoForm :: forall p i. H.Action i -> HH.HTML p (i Unit)
newMemoForm createMemoQuery =
  HH.button
    [ HE.onClick $ HE.input_ createMemoQuery ]
    [ HH.text "New memo" ]
