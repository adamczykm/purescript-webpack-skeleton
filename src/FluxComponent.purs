module FluxComponent where

--- temp

-- The application monad stack
import Control.Applicative (class Applicative, apply, pure, when)
import Control.Apply (class Apply)
import Control.Bind (class Bind, bind, discard, (>>=))
import Control.Category ((<<<))
import Control.Monad.Reader (class MonadReader, Reader, ReaderT(..), ask)
import Data.Array (snoc)
import Data.Function (const, flip, ($))
import Data.Functor (class Functor, map)
import Data.Semiring ((+))
import Data.Symbol (SProxy(..))
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Console as EC
import Effect.Ref (Ref, modify, modify_, new, read)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic (Component, ComponentInstance, JSX, component, element)
import Record (insert)
import Web.DOM.Document (createElement)

--- temp
type LogRecord = {msg ∷ String}

-- The application monad stack
newtype AppMonad a = AppMonad (ReaderT AppRecord Effect a)

instance functorAppMonad ∷ Functor AppMonad where
  map f (AppMonad rt) = AppMonad (map f rt)

instance applyAppMonad ∷ Apply AppMonad where
  apply (AppMonad f) (AppMonad rt) = AppMonad (apply f rt)

instance applicativeAppMonad ∷ Applicative AppMonad where
  pure = AppMonad <<< pure

instance bindAppMonad ∷ Bind AppMonad where
  bind (AppMonad rt) f = let fi a = let (AppMonad rt') = f a
                                    in rt'
                         in AppMonad (rt >>= fi)

-- The application config context
type AppReader a = Reader AppRecord a

-- Read-only global state of the application
type AppRecord = {
  storeState ∷ Ref StoreState,
  storeChangeSubscriptions ∷ Ref (Array (StoreState → StoreState → Effect Unit)),
  updateStoreState ∷ Ref StoreState → Action → Effect Unit,
  subscribeToStateChange ∷ Ref (Array (StoreState → StoreState → Effect Unit)) → (StoreState → StoreState → Boolean) → Effect Unit → Effect Unit
}

type StoreState = {i ∷ Int}

type SetState state = ({ | state } -> { | state }) -> Effect Unit

type SetStateThen state = ({ | state } -> { | state }) -> ({ | state } -> Effect Unit) -> Effect Unit

-------
data SubRecordSpec a = SubRecordSpec

data Action = Increment


rootComponent ∷ Effect Unit --(Component {})
rootComponent = pure unit -- ?asd2
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


    updateStoreState ∷ Ref StoreState
      → Action
      → Effect Unit
    updateStoreState ssRef action =
      modify_ (reducer action) ssRef


fluxComponent ∷ ∀ props state
  . { displayName ∷ String
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
        → Effect Unit
     , render ∷
        { props ∷ { | props }
        , state ∷ { | state }
        , setState ∷ SetState state
        , setStateThen ∷ SetStateThen state
        , instance_ ∷ ComponentInstance
        , storeState ∷ StoreState
        , log ∷ LogRecord → Effect Unit
        } → JSX
  } → AppMonad (Component { | props })
fluxComponent spec = AppMonad $ do

  -- unpack apprecord
  { storeState
  , storeChangeSubscriptions
  , updateStoreState
  , subscribeToStateChange} ← ask

  ------------- prepare auxilliary functions
      -- accept "forceUpdate" function and call it when intended part of storestate changes
  let subscribeForStoreStateChange = subscribeToStateChange storeChangeSubscriptions spec.subscribedStoreStateChange
      -- dispatch and action to perform store update, (TODO: maybe cause render loop, how to unloop)
      dispatchF     = updateStoreState storeState
      -- read storestate ref
      getStoreState = read storeState
      -- [TODO] to be replaced by logging utils
      log {msg}     = EC.log msg

  ------- wrap react basic component creation
      receiveProps origArgs = do
        when (origArgs.isFirstMount) $ subscribeForStoreStateChange (origArgs.setState (const origArgs.state))
        ss ← read storeState
        let newArgs = insert (SProxy ∷ SProxy "storeState") ss
                       (insert (SProxy ∷ SProxy "dispatch") dispatchF
                        (insert (SProxy ∷ SProxy "log") log origArgs))
        spec.receiveProps newArgs

      render origArgs = unsafePerformEffect do
        ss ← read storeState
        let newArgs = insert (SProxy ∷ SProxy "storeState") ss
                       (insert (SProxy ∷ SProxy "log") log origArgs)
        pure $ spec.render newArgs

  pure $ component
   { displayName: spec.displayName
   , initialState: spec.initialState
   , receiveProps
   , render
   }

createFluxElement ∷ ∀ props
  . AppMonad (Component { | props })
  → {| props }
  → AppMonad JSX
createFluxElement mc p = mc >>= \c → pure (element c p)

