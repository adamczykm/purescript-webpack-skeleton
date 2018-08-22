module Example1 where

import FluxComponent

import Control.Applicative (pure)
import Control.Apply ((*>))
import Control.Bind (bind)
import Control.Monad (class Monad)
import Data.Eq ((/=))
import Data.Function (($))
import Data.Semiring ((+))
import Data.Show (show)
import Effect.Console (log)
import Effect.Console as EC
import Effect.Unsafe (unsafePerformEffect)
import React.Basic (Component)
import React.Basic.DOM as R
import React.Basic.Events as Events

---------- unsafelog
dbgLog :: forall t43. String -> t43 -> t43
dbgLog str a = unsafePerformEffect (EC.log str *> pure a)

simpleTester1 ∷ ∀ e. Monad e ⇒ AppMonad e (Component {})
simpleTester1 = do
  test2 <- createFluxElement simpleTester2 {}
  fluxComponent
    { displayName: "simpleTester1"
    , initialState: {d:0}
    , subscribedStoreStateChange: \old new → old.i /= new.i
    , receiveProps: \_ -> liftAction $ EC.log "recv props"
    , render: render test2 }
  where
    render test2 {setState, state, storeState, dispatch} = dbgLog "render" $ do
      pure $ R.div
        { children:
            [ R.text (show storeState.i)
            , R.text (show state.d)
            , R.button { children: [R.text "Store"], onClick: Events.handler_ $ log "dispatch" *> dispatch Increment }
            , R.button { children: [R.text "Store & State"], onClick: Events.handler_ $ log "dispatch" *> setState (\ {d} -> {d: d+1}) *> dispatch Increment }
            , R.button { children: [R.text "State"], onClick: Events.handler_ $ setState (\ {d} -> {d: d+1}) }
            , test2
            ] }


simpleTester2 ∷ ∀ e. Monad e ⇒ AppMonad e (Component {})
simpleTester2 = fluxComponent
  { displayName: "simpleTester2"
  , initialState: {d:0}
  , subscribedStoreStateChange: \old new → old.i /= new.i
  , receiveProps: \_ -> liftAction $ EC.log "recv props"
  , render }
  where
    render {setState, state, storeState, dispatch} = dbgLog "render" $ pure $
      R.div
        { children:
            [ R.text (show storeState.i)
            , R.text (show state.d)
            , R.button { children: [R.text "Store"], onClick: Events.handler_ $ log "dispatch" *> dispatch Increment }
            , R.button { children: [R.text "Store & State"], onClick: Events.handler_ $ log "dispatch" *> setState (\ {d} -> {d: d+1}) *> dispatch Increment }
            , R.button { children: [R.text "State"], onClick: Events.handler_ $ setState (\ {d} -> {d: d+1}) }
            ] }
