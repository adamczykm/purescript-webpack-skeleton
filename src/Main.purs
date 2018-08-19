module Main where

import Prelude

import Container as Container
import Effect (Effect)
import React.Basic (Component)


main :: Effect (Component {})
main = pure $ Container.component
