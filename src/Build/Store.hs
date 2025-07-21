{-# LANGUAGE RankNTypes #-}

module Build.Store where

data Store i k v = Store {info :: i, values :: k -> v}

getInfo :: Store i k v -> i
getInfo = info

getValue :: k -> Store i k v -> v
getValue key s = values s key

putInfo :: i -> Store i k v -> Store i k v
putInfo i s = let v = values s in Store {info = i, values = v}

initialise :: i -> (k -> v) -> Store i k v
initialise = Store
