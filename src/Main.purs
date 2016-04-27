module Main where

import Prelude (Unit, bind)

import Component.RegistrationForm

import Halogen (HalogenEffects, Component, runUI)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Halogen.Util (awaitBody, runHalogenAff)

ui :: forall eff. Component RegistrationState RegistrationQuery (RegistrationEff eff)
ui = registrationForm

main :: Eff (HalogenEffects (console :: CONSOLE)) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui initialRegistrationState body
