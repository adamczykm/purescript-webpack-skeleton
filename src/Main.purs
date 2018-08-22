module Main where

import Prelude

import Effect (Effect)
import Example1 (simpleTester1)
import FluxComponent (rootComponent)
import React.Basic (Component)


main :: Effect (Component {})
main = rootComponent identity simpleTester1
