{-# LANGUAGE RankNTypes #-}

module Build.Task where

newtype Task c k v = Task { run :: forall f. c f => (k -> f v) -> f v }
type Tasks c k v = k -> Maybe (Task c k v)

sprsh1 :: Tasks Applicative String Integer
sprsh1 "B1" = Just $ Task $ \fetch -> (+) <$> fetch "A1"  <*> fetch "A2"
