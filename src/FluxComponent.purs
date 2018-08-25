module FluxComponent where

import Control.Applicative (class Applicative, pure, when)
import Control.Apply (class Apply, (*>))
import Control.Bind (class Bind, bind, discard, (=<<), (>>=))
import Control.Category ((<<<))
import Control.Monad (class Monad)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Data.Array (snoc)
import Data.Function (flip, ($))
import Data.Functor (class Functor, map)
import Data.Maybe (Maybe(..), maybe)
import Data.NaturalTransformation (type (~>))
import Data.Semiring ((+))
import Data.Show (show)
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence, traverse_)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Console as EC
import Effect.Ref (Ref, modify, modify_, new, read)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic (Component, ComponentInstance, JSX, component, element, fragment)
import Record (insert)


foreign import forceUpdate ∷ ComponentInstance → Effect Unit

-- The application monad stack
newtype AppMonad h e a = AppMonad (ReaderT (AppRecord h) e a)

derive newtype instance functorAppMonad ∷ Functor e ⇒ Functor (AppMonad h e)
derive newtype instance applyAppMonad ∷ Apply e ⇒ Apply (AppMonad h e)
derive newtype instance applicativeAppMonad ∷ Applicative e ⇒ Applicative (AppMonad h e)
derive newtype instance bindAppMonad ∷ Bind e ⇒ Bind (AppMonad h e)
derive newtype instance monadAppMonad ∷ Monad e ⇒ Monad (AppMonad h e)


liftAction ∷ ∀ h e. Monad e ⇒ e ~> AppMonad h e
liftAction = AppMonad <<< lift

-- to be hidden
unApp ∷ ∀ h e a
  . AppMonad h e a
  → ReaderT (AppRecord h) e a
unApp (AppMonad rt) = rt

type StoreListener = (StoreState → StoreState → Maybe ComponentInstance)

-- Read-only global state of the application

type AppRecord h =
  { storeState ∷ Ref StoreState
  , storeChangeSubscriptions ∷ Ref (Array StoreListener)
  , updateStoreState
    ∷ Ref (Array StoreListener)
    → Ref StoreState
    → Action
    → Effect Unit
  , subscribeToStateChange ∷ Ref (Array StoreListener) → StoreListener → Effect Unit
}

type StoreState = {i ∷ Int}

type SetState state = ({ | state } -> { | state }) -> Effect Unit

type SetStateThen state = ({ | state } -> { | state }) -> ({ | state } -> Effect Unit) -> Effect Unit

-------
data SubRecordSpec a = SubRecordSpec

data Action = Increment

type FluxComponent h e p = AppMonad h e (Component p)

rootComponent ∷ ∀ e. e ~> Effect → (∀ h. FluxComponent h e {}) → Effect (Component {})
rootComponent runE (AppMonad cmp) = initialAppRecord >>= runE <<< runReaderT cmp
  where

    tempSentryContext = {user: 0}


    initialStoreState ∷ StoreState
    initialStoreState = {i: 0}

    reducer ∷ Action → StoreState → StoreState
    reducer Increment {i} = {i: i+1}


    -------- things below are unsafe and should be hidden

    initialAppRecord ∷ ∀ h. Effect (AppRecord h)
    initialAppRecord = do
      ss ← new initialStoreState
      subs ← new []
      pure {
        storeState: ss,
        storeChangeSubscriptions: subs,
        updateStoreState,
        subscribeToStateChange}

    subscribeToStateChange ∷ Ref (Array StoreListener) → StoreListener → Effect Unit
    subscribeToStateChange subsRef listener = modify_ (flip snoc listener) subsRef

    updateStoreState
      ∷ Ref (Array StoreListener)
      → Ref StoreState
      → Action
      → Effect Unit
    updateStoreState listenersRef ssRef action = do
      ssOld ← read ssRef
      ssNew ← modify (reducer action) ssRef
      EC.log "Store new state: " *> read ssRef >>= EC.log <<< show
      traverse_ (\listener → (maybe (pure unit) forceUpdate) (listener ssOld ssNew)) =<< read listenersRef


fluxComponent ∷ ∀ h props state e
  . Monad e
  ⇒ { displayName ∷ String
    , initialState ∷ { | state }
    , subscribedStoreStateChange ∷ StoreState → StoreState → Boolean
    , receiveProps ∷
        { isFirstMount ∷ Boolean
        , props ∷ { | props }
        , state ∷ { | state }
        , setState ∷ SetState state
        , setStateThen ∷ SetStateThen state
        , instance_ ∷ ComponentInstance
        , storeState ∷ StoreState
        , dispatch ∷ Action → Effect Unit
        }
        → Effect Unit
     , render ∷
        { props ∷ { | props }
        , state ∷ { | state }
        , setState ∷ SetState state
        , setStateThen ∷ SetStateThen state
        , instance_ ∷ ComponentInstance
        , storeState ∷ StoreState
        , dispatch ∷ Action → Effect Unit
        } → JSX
  } → AppMonad h e (Component { | props })

fluxComponent spec = AppMonad $ do

  -- unpack apprecord
  appR@{ storeState
       , storeChangeSubscriptions
       , updateStoreState
       , subscribeToStateChange } ← ask

  ------------- prepare auxilliary functions

      -- accept "forceUpdate" function and call it when intended part of storestate changes
  let subscribeForStoreStateChange inst = subscribeToStateChange storeChangeSubscriptions (\old new → if spec.subscribedStoreStateChange old new then Just inst else Nothing)
      -- dispatch and action to perform store update, (TODO: maybe cause render loop, how to unloop)
      dispatchF     = updateStoreState storeChangeSubscriptions storeState
      -- read storestate ref
      getStoreState = read storeState

  ------------- wrap react basic component creation
      receiveProps origArgs = do
        when (origArgs.isFirstMount) $ subscribeForStoreStateChange origArgs.instance_
        ss ← read storeState
        let newArgs = insert (SProxy ∷ SProxy "storeState") ss
                       (insert (SProxy ∷ SProxy "dispatch") dispatchF origArgs)
        spec.receiveProps newArgs

      render origArgs = unsafePerformEffect do
        ss ← read storeState
        let newArgs = insert (SProxy ∷ SProxy "storeState") ss
                       (insert (SProxy ∷ SProxy "dispatch") dispatchF origArgs)
        pure $ spec.render newArgs

  ------------- build react component
  pure $ component
    { displayName: spec.displayName
    , initialState: spec.initialState
    , receiveProps
    , render }

createFluxElement ∷ ∀ h props e
  . Monad e
  ⇒ AppMonad h e (Component { | props })
  → {| props }
  → AppMonad h e JSX
createFluxElement mc p = mc >>= \c → pure (element c p)

fragmentFlux ∷ ∀ h e. Applicative e ⇒ Array (AppMonad h e JSX) → AppMonad h e JSX
fragmentFlux = map fragment <<< sequence
