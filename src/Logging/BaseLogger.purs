module Logging.BaseLogger where


import Control.Applicative (class Applicative)
import Control.Bind (when)
import Control.Logger (Logger(..))
import Data.Function (($))
import Data.HeytingAlgebra ((&&))
import Data.Ord (class Ord, (<))
import Data.Symbol (SProxy(..))
import Data.Unit (Unit)
import Prim.Row as Row
import Record (delete, insert)


type BaseLogger ctx m l r = ctx → l → r → m Unit


fromBaseLogger :: ∀ r m ctx l
  . Row.Lacks "level" r
  ⇒ Row.Lacks "context" r
  ⇒ BaseLogger ctx m l {|r}
  → Logger m { context :: ctx , level :: l | r}
fromBaseLogger lgr = Logger $ \r → do
  lgr r.context r.level (delete (SProxy ∷ SProxy "level")
                          (delete (SProxy ∷ SProxy "context") r))


toBaseLogger ∷ ∀ m l ctx r
  . Row.Lacks "level" r
  ⇒ Row.Lacks "context" r
  ⇒ Logger m { level ∷ l , context ∷ ctx | r}
  → BaseLogger ctx m l {|r}
toBaseLogger (Logger log) = \ctx l r → log
  (insert (SProxy ∷ SProxy "level") l
    (insert (SProxy ∷ SProxy "context") ctx r))


buildLogger ∷ ∀ ctx r m l cfgr
  . Applicative m
  ⇒ Ord l
  ⇒ { level ∷ l , filter ∷ r → Boolean | cfgr}
  → (ctx → l → r → m Unit)
  → BaseLogger ctx m l r
buildLogger cfg log = \ctx l r → do
  when (cfg.level < l && cfg.filter r) $
    log ctx l r
