module Main where

import Prelude (Unit, bind, id, pure, show, (<>), ($))

import Halogen (ComponentDSL, ComponentHTML, HalogenEffects, Natural, Component, action, component, fromAff, gets, modify, runUI)

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Functor (($>))

import Halogen.HTML.Events.Handler as EH
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import Halogen.Util (awaitBody, runHalogenAff)

newtype User = User { name :: String, surname :: String, age :: Int }

type State = { name :: String, surname :: String, age :: String }

initialState :: State
initialState = { name : "", surname : "", age : "" }

type UIEff eff = Aff (console :: CONSOLE | eff)

data Query a = FormSubmit a
             | UpdateAge String a
             | UpdateName String a
             | UpdateSurname String a

textInput :: String -> String -> State -> (State -> String) -> (String -> Unit -> Query Unit) -> ComponentHTML Query
textInput id text state selector query =
  H.div [ P.class_ $ H.className "form-group" ]
        [ H.label [ P.for id
                  , P.class_ $ H.className "col-md-2 control-label"
                  ]
                  [ H.text text ]
        , H.div [ P.class_ $ H.className "col-md-6" ]
                [ H.input [ P.inputType P.InputText
                          , P.class_ $ H.className "form-control"
                          , P.id_ id
                          , P.value $ selector state
                          , E.onValueChange $ E.input query
                          ]
                ]
        ]

ui :: forall eff. Component State Query (UIEff eff)
ui = component { render, eval }
  where

  render :: State -> ComponentHTML Query
  render state =
    H.div_ [ H.div [ P.class_ $ H.className "navbar navbar-default" ]
                   [ H.div [ P.class_ $ H.className "container-fluid" ]
                           [ H.div [ P.class_ $ H.className "navbar-header" ]
                                   [ H.a [ P.class_ $ H.className "navbar-brand"
                                         , P.href "#"
                                         ]
                                         [ H.text "Purescript" ]
                                   ]
                           ]
                    ]
           , H.div [ P.class_ $ H.className "container-fluid" ]
                   [ H.div [ P.class_ $ H.className "row" ]
                           [ H.form [ P.class_ $ H.className "col-md-8 form-horizontal" ]
                                    [ textInput "name" "Name" state _.name UpdateName
                                    , textInput "surname" "Surname" state _.surname UpdateSurname
                                    , textInput "age" "Age" state _.age UpdateAge
                                    , H.div [ P.class_ $ H.className "form-group" ]
                                            [ H.div [ P.class_ $ H.className "col-md-offset-2 col-md-6" ]
                                                    [ H.button [ P.buttonType P.ButtonSubmit
                                                               , P.class_ $ H.className "btn btn-default"
                                                               , E.onClick $ (\_ -> EH.preventDefault $> action FormSubmit)
                                                               ]
                                                              [ H.text "Submit" ]
                                                    ]
                                            ]
                                    ]
                           ]
                   ]
           ]

  eval :: Natural Query (ComponentDSL State Query (UIEff eff))
  eval (FormSubmit next) = do
    state <- gets id
    fromAff $ log $ "State: " <> (show state.name) <> " :: " <> (show state.surname) <> " :: " <> (show state.age)
    pure next
  eval (UpdateAge age next) = do
    modify (_ { age = age })
    pure next
  eval (UpdateName name next) = do
    modify (_ { name = name })
    pure next
  eval (UpdateSurname surname next) = do
    modify (_ { surname = surname })
    pure next

-- | Run the app
main :: Eff (HalogenEffects (console :: CONSOLE)) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui initialState body
