{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

module Main (main) where
import Control.Monad.RWS (MonadState)
import Control.Monad.State 
import Control.Monad.Writer
import Data.Hashable (Hashable)
import Data.Functor.Const
import Data.Functor.Identity
import qualified Data.Map.Strict as M

data Store i k v = Stored i (k -> v) 

initialise :: i -> (k -> v) -> Store i k v
initialise = Stored

getInfo :: Store i k v -> i
getInfo (Stored i fn) = i

putInfo :: i -> Store i k v -> Store i k v
putInfo i (Stored old fn) = Stored i fn

getValue :: k -> Store i k v -> v
getValue k (Stored i fn) = fn k

putValue :: Eq k => k -> v -> Store i k v -> Store i k v
putValue k v (Stored i fn) = Stored i (\nk -> if nk == k then v else fn nk)


data Hash v

hash :: Hashable v => v -> Hash v
hash = undefined

getHash :: Hashable v => k -> Store i k v -> Hash v
getHash = undefined

newtype Task c k v = Task { run :: forall f. c f => (k -> f v) -> f v }
type Tasks c k v = k -> Maybe (Task c k v)

type Build c i k v = Tasks c k v -> k -> Store i k v -> Store i k v

type Scheduler c i ir k v = Rebuilder c ir k v -> Build c i k v
type Rebuilder c   ir k v = k -> v -> Task c k v -> Task (MonadState ir) k v


busy :: Eq k => Build Applicative () k v
busy tasks key store = execState (fetch key) store
  where
    fetch k = case tasks k of
                Nothing -> gets (getValue k)
                Just task -> do v <- run task fetch; modify (putValue k v); return v


sprsh1 :: Tasks Applicative String Integer
sprsh1 "B1" = Just $ Task $ \fetch -> ((+) <$> fetch "A1" <*> fetch "A2")
sprsh1 "B2" = Just $ Task $ \fetch -> (*2) <$> fetch "B1"
sprsh1 _ = Nothing

store = initialise () (\key -> if key == "A1" then 10 else 20)
result = busy sprsh1 "B2" store

compute :: Task Monad k v -> Store i k v -> v
compute task store = undefined

dependencies :: Task Applicative k v -> [k]
dependencies task = getConst $ run task (\k -> Const [k])

main = putStrLn "Hello, World!"
