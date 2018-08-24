module Example1 where

import FluxComponent

import Control.Applicative (pure)
import Control.Apply ((*>))
import Control.Bind (bind)
import Control.Monad (class Monad)
import Data.Eq ((/=))
import Data.Function (const, ($))
import Data.Maybe (Maybe(..))
import Data.Semiring ((+))
import Data.Show (show)
import Data.Unit (unit)
import Effect.Console as EC
import React.Basic (empty)
import React.Basic.DOM (button, div, text) as R
import React.Basic.Events as Events

simpleTester ∷ ∀ h e. Monad e => String -> Maybe (FluxComponent h e {}) -> FluxComponent h e {}
simpleTester name mChild = do
  child <- case mChild of
    Just c  -> createFluxElement c {}
    Nothing -> pure empty
  fluxComponent
    { displayName: name
    , initialState: {d:0}
    , subscribedStoreStateChange: \old new → old.i /= new.i
    , receiveProps: const $ pure unit
    , render: render child }
  where
    render child {setState, state, storeState, dispatch} = R.div
        { children:
            [ R.text (show storeState.i)
            , R.text (show state.d)
            , R.button { children: [R.text "Store"], onClick: Events.handler_ $ EC.log "dispatch" *> dispatch Increment }
            , R.button { children: [R.text "Store & State"], onClick: Events.handler_ $ EC.log "dispatch" *> setState (\ {d} -> {d: d+1}) *> dispatch Increment }
            , R.button { children: [R.text "State"], onClick: Events.handler_ $ setState (\ {d} -> {d: d+1}) }
            , child
            ] }
