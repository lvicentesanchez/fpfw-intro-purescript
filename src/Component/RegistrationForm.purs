module Component.RegistrationForm
  ( RegistrationEff()
  , RegistrationQuery
  , RegistrationState()
  , initialRegistrationState
  , registrationForm
  ) where

import Prelude (Unit, bind, map, pure, show, (++), ($))

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Array (null)
import Data.Functor (($>))
import Data.Validation (runV)

import Halogen (ComponentDSL, ComponentHTML, Natural, Component, action, component, fromAff, get, modify)
import Halogen.HTML.Events.Handler as EH
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import Model.User (User(..), fromRegistration)

type RegistrationState = { name :: String, surname :: String, age :: String, errors :: Array String }

initialRegistrationState :: RegistrationState
initialRegistrationState = { name : "", surname : "", age : "", errors : [] }

type RegistrationEff eff = Aff (console :: CONSOLE | eff)

data RegistrationQuery a = SubmitRegistrationForm a
                         | UpdateAge String a
                         | UpdateName String a
                         | UpdateSurname String a

errorPanel :: Array String -> ComponentHTML RegistrationQuery
errorPanel err =
  if null err
  then H.div_ []
  else H.div [ P.class_ (H.className "alert alert-danger") ]
             [ H.ul_ (map (\str -> H.li_ [ H.text str ]) err) ]

navbar :: ComponentHTML RegistrationQuery
navbar =
  H.div [ P.class_ (H.className "navbar navbar-default") ]
        [ H.div [ P.class_ (H.className "container-fluid") ]
                [ H.div [ P.class_ (H.className "navbar-header") ]
                        [ H.a [ P.class_ (H.className "navbar-brand")
                              , P.href "#"
                              ]
                              [ H.text "Purescript" ]
                        ]
                ]
         ]

submit
  :: String
  -> (Unit -> RegistrationQuery Unit)
  -> ComponentHTML RegistrationQuery
submit caption query =
  H.div [ P.class_ $ H.className "form-group" ]
        [ H.div [ P.class_ (H.className "col-md-offset-2 col-md-6") ]
                [ H.button [ P.buttonType P.ButtonSubmit
                           , P.class_ (H.className "btn btn-default")
                           , E.onClick (\_ -> EH.preventDefault $> action query)
                           ]
                           [ H.text caption ]
                ]
        ]

textInput
  :: String
  -> String
  -> RegistrationState
  -> (RegistrationState -> String)
  -> (String -> Unit -> RegistrationQuery Unit)
  -> ComponentHTML RegistrationQuery
textInput id label state getter query =
  H.div [ P.class_ (H.className "form-group") ]
        [ H.label [ P.for id
                  , P.class_ (H.className "col-md-2 control-label")
                  ]
                  [ H.text label ]
        , H.div [ P.class_ (H.className "col-md-6") ]
                [ H.input [ P.inputType P.InputText
                          , P.class_ (H.className "form-control")
                          , P.id_ id
                          , P.value (getter state)
                          , E.onValueChange (E.input query)
                          ]
                ]
        ]

registrationForm :: forall eff. Component RegistrationState RegistrationQuery (RegistrationEff eff)
registrationForm = component { render, eval }
  where

  render :: RegistrationState -> ComponentHTML RegistrationQuery
  render state =
    H.div_ [ navbar
           , H.div [ P.class_ $ H.className "container-fluid" ]
                   [ H.div [ P.class_ $ H.className "row" ]
                           [ H.form [ P.class_ $ H.className "col-md-8 form-horizontal" ]
                                    [ textInput "name" "Name" state _.name UpdateName
                                    , textInput "surname" "Surname" state _.surname UpdateSurname
                                    , textInput "age" "Age" state _.age UpdateAge
                                    , submit "Register" SubmitRegistrationForm
                                    , H.div [ P.class_ $ H.className "form-group" ]
                                            [ H.div [ P.class_ (H.className "col-md-offset-2 col-md-6") ]
                                                    [ errorPanel state.errors ]
                                            ]
                                    ]
                           ]

                   ]
           ]

  eval :: Natural RegistrationQuery (ComponentDSL RegistrationState RegistrationQuery (RegistrationEff eff))
  eval (SubmitRegistrationForm next) = do
    state <- get
    runV error valid (fromRegistration state.name state.surname state.age)
    pure next
    where
    error err = modify (_ { errors = err })
    valid (User u) = do
      error []
      fromAff $ log ("Submit :: User(" ++ u.name ++ ", " ++ u.surname ++ ", " ++ (show u.age) ++ ")")

  eval (UpdateAge age next) = do
    modify (_ { age = age })
    pure next

  eval (UpdateName name next) = do
    modify (_ { name = name })
    pure next

  eval (UpdateSurname surname next) = do
    modify (_ { surname = surname })
    pure next
