module Build.Build where
import Build.Task
import Build.Store 
import Control.Monad.State

type Build c i k v = Tasks c k v -> k -> Store i k v -> Store i k v


busy :: Eq k => Build Applicative () k v
busy tasks key store = execState (fetch key) store
  where
    fetch :: k -> State (Store () k v) v
    fetch k = case tasks k of
                Nothing -> gets (getValue k)
                Just task -> do v

