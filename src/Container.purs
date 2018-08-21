module Container where

import Data.Semiring ((+))
import Data.Show (show)
import Effect.Console (log)
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.Events as Events
import ToggleButton as ToggleButton

component :: React.Component {}
component = React.component { displayName: "container", initialState, receiveProps, render}
  where
    receiveProps _ = do
      log "container rec props"

    incState {i} = {i: i+1}
    initialState = {i: 0}
    render {state, setState} =
      R.div
        { children:
            [ R.text (show state.i)
            , R.button { onClick: Events.handler_ do
                            setState incState }
            , React.element ToggleButton.component { label: "A" }
            , React.element ToggleButton.component { label: "B" }
            ]
}
