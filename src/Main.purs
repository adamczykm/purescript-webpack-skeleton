module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Example1 (simpleTester)
import FluxComponent (rootComponent)
import React.Basic (Component)


main :: Effect (Component {})
main = rootComponent identity (simpleTester "testerParent" (Just (simpleTester "testerChild" Nothing)))
