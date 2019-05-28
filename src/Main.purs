module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Purememo.AppM (Env, LogLevel(..), runAppM)
import Purememo.Component.Home as Home


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody

  let logLevel = Dev

  let
    environment :: Env
    environment = { logLevel }

  -- -- Playing around with local storage API
  -- allMemos <- H.liftEffect getAllItems
  -- H.liftEffect $ log (show allMemos)

  -- let
  --   startsWithF :: String -> Boolean
  --   startsWithF s = case charAt 0 s of
  --     Nothing -> false
  --     Just c -> toLower c == 'f'

  -- filtered <- H.liftEffect $ filterItemsByKey startsWithF
  -- H.liftEffect $ log (show filtered)

  -- filteredByValue <- H.liftEffect $ filterItemsByValue startsWithF
  -- H.liftEffect $ log (show filteredByValue)

  let
    rootComponent :: H.Component HH.HTML Home.Query Unit Void Aff
    rootComponent = H.hoist (runAppM environment) Home.home

  _ <- runUI rootComponent unit body

  pure unit
