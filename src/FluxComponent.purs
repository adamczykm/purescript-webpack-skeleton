module FluxComponent where

import Control.Applicative (class Applicative, apply, pure, when)
import Control.Apply (class Apply, (*>))
import Control.Bind (class Bind, bind, discard, (=<<), (>>=))
import Control.Category ((<<<))
import Control.Monad (class Monad)
import Control.Monad.Reader (Reader, ReaderT, ask, lift, runReaderT)
import Data.Array (length, snoc)
import Data.Function (const, flip, ($))
import Data.Functor (class Functor, map)
import Data.Identity (Identity)
import Data.NaturalTransformation (type (~>))
import Data.Newtype (unwrap)
import Data.Semiring ((+))
import Data.Show (show)
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence, traverse_)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Console as EC
import Effect.Ref (Ref, modify, modify_, new, read)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic (Component, ComponentInstance, JSX, component, element, fragment)
import Record (insert)


--- temp
type LogRecord = {msg ∷ String}

-- The application monad stack
newtype AppMonad e a = AppMonad (ReaderT AppRecord e a)

liftAction ∷ ∀ e. Monad e ⇒ e ~> AppMonad e
liftAction = AppMonad <<< lift

-- to be hidden
unApp ∷ ∀ e a
  . AppMonad e a
  → ReaderT AppRecord e a
unApp (AppMonad rt) = rt

instance functorAppMonad ∷ Functor e ⇒ Functor (AppMonad e) where
  map f (AppMonad rt) = AppMonad (map f rt)

instance applyAppMonad ∷ Apply e ⇒ Apply (AppMonad e) where
  apply (AppMonad f) (AppMonad rt) = AppMonad (apply f rt)

instance applicativeAppMonad ∷ Applicative e ⇒ Applicative (AppMonad e) where
  pure = AppMonad <<< pure

instance bindAppMonad ∷ Bind e ⇒ Bind (AppMonad e) where
  bind (AppMonad rt) f = AppMonad (rt >>= unApp <<< f)

instance monadAppMonad ∷ Monad e ⇒ Monad (AppMonad e)

-- The application config context
type AppReader a = Reader AppRecord a

-- Read-only global state of the application
type AppRecord = {
  storeState ∷ Ref StoreState,
  storeChangeSubscriptions ∷ Ref (Array (StoreState → StoreState → Effect Unit)),
  updateStoreState
    ∷ Ref (Array (StoreState → StoreState → Effect Unit))
    → Ref StoreState
    → Action
    → Effect Unit,
  subscribeToStateChange ∷ Ref (Array (StoreState → StoreState → Effect Unit)) → (StoreState → StoreState → Boolean) → Effect Unit → Effect Unit
}

type StoreState = {i ∷ Int}

type SetState state = ({ | state } -> { | state }) -> Effect Unit

type SetStateThen state = ({ | state } -> { | state }) -> ({ | state } -> Effect Unit) -> Effect Unit

-------
data SubRecordSpec a = SubRecordSpec

data Action = Increment


rootComponent ∷ ∀ e. e ~> Effect →  AppMonad e (Component {}) → Effect (Component {})
rootComponent runE (AppMonad cmp) = initialAppRecord >>= runE <<< runReaderT cmp
  where

    initialStoreState ∷ StoreState
    initialStoreState = {i: 0}

    reducer ∷ Action → StoreState → StoreState
    reducer Increment {i} = {i: i+1}

    -------- things below are unsafe and should be hidden

    initialAppRecord ∷ Effect AppRecord
    initialAppRecord = do
      ss ← new initialStoreState
      subs ← new []
      pure {
        storeState: ss,
        storeChangeSubscriptions: subs,
        updateStoreState,
        subscribeToStateChange
      }

    subscribeToStateChange ∷ Ref (Array (StoreState → StoreState → Effect Unit)) → (StoreState → StoreState → Boolean) → Effect Unit → Effect Unit
    subscribeToStateChange subsRef storeDiff forceUpdate = do
      let forceUpdateOnSubscribedChange old new = when (storeDiff old new) forceUpdate
      modify_ (flip snoc forceUpdateOnSubscribedChange) subsRef


    updateStoreState
      ∷ Ref (Array (StoreState → StoreState → Effect Unit))
      → Ref StoreState
      → Action
      → Effect Unit
    updateStoreState listenersRef ssRef action = do
      ssOld ← read ssRef
      ssNew ← modify (reducer action) ssRef
      EC.log "Store new state: " *> read ssRef >>= EC.log <<< show
      traverse_ (\listen → listen ssOld ssNew) =<< read listenersRef



fluxComponent ∷ ∀ props state e
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
        , log ∷ LogRecord → Effect Unit
        }
        → AppMonad Effect Unit
     , render ∷
        { props ∷ { | props }
        , state ∷ { | state }
        , setState ∷ SetState state
        , setStateThen ∷ SetStateThen state
        , instance_ ∷ ComponentInstance
        , storeState ∷ StoreState
        , dispatch ∷ Action → Effect Unit
        , log ∷ LogRecord → Effect Unit
        } → AppMonad Identity JSX
  } → AppMonad e (Component { | props })

fluxComponent spec = AppMonad $ do

  -- unpack apprecord
  appR@{ storeState
  , storeChangeSubscriptions
  , updateStoreState
  , subscribeToStateChange} ← ask

  ------------- prepare auxilliary functions

      -- accept "forceUpdate" function and call it when intended part of storestate changes
  let subscribeForStoreStateChange = subscribeToStateChange storeChangeSubscriptions spec.subscribedStoreStateChange
      -- dispatch and action to perform store update, (TODO: maybe cause render loop, how to unloop)
      dispatchF     = updateStoreState storeChangeSubscriptions storeState
      -- read storestate ref
      getStoreState = read storeState
      -- [TODO] to be replaced by logging utils
      log {msg}     = EC.log msg

  ------------- wrap react basic component creation
      receiveProps origArgs = AppMonad $ do
        lift $ when (origArgs.isFirstMount) $ subscribeForStoreStateChange (EC.log "force update" *> origArgs.setState (const origArgs.state))
        ss ← lift $ read storeState
        let newArgs = insert (SProxy ∷ SProxy "storeState") ss
                       (insert (SProxy ∷ SProxy "dispatch") dispatchF
                        (insert (SProxy ∷ SProxy "log") log origArgs))
        unApp $ spec.receiveProps newArgs

      render origArgs = unsafePerformEffect do
        ss ← read storeState
        let newArgs = insert (SProxy ∷ SProxy "storeState") ss
                       (insert (SProxy ∷ SProxy "dispatch") dispatchF
                        (insert (SProxy ∷ SProxy "log") log origArgs))
        pure $ spec.render newArgs

  ------------- build react component
  pure $ component
    { displayName: spec.displayName
    , initialState: spec.initialState
    , receiveProps: flip runReaderT appR <<< unApp <<< receiveProps
    , render: unwrap <<< flip runReaderT appR <<< unApp <<< render }

createFluxElement ∷ ∀ props e
  . Monad e
  ⇒ AppMonad e (Component { | props })
  → {| props }
  → AppMonad e JSX
createFluxElement mc p = mc >>= \c → pure (element c p)

fragmentFlux ∷ ∀ e. Applicative e ⇒ Array (AppMonad e JSX) → AppMonad e JSX
fragmentFlux = map fragment <<< sequence
