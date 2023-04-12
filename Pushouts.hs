{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

module Pushouts  where

import Data.Proxy

class Pair tag x a b | tag -> x, tag -> a, tag -> b where
  projectL :: Proxy tag -> x -> a
  projectR :: Proxy tag -> x -> b

-- Law: injectL t . projectL t === injectR t . projectR t
class Pair tag x a b => Pushout tag x a b where
  type PushoutTo tag x a b
  injectL :: Proxy tag -> a -> PushoutTo tag x a b
  injectR :: Proxy tag -> b -> PushoutTo tag x a b

data Wedge a b = Nowhere | Here a | There b
  deriving Show

data Boring a b

instance Pair (Boring a b) () (Maybe a) (Maybe b) where
  projectL :: Proxy (Boring a b) -> () -> Maybe a
  projectL _ () = Nothing
  projectR :: Proxy (Boring a b) -> () -> Maybe b
  projectR _ () = Nothing

instance Pushout (Boring a b) () (Maybe a) (Maybe b) where
  type PushoutTo (Boring a b) () (Maybe a) (Maybe b) = Wedge a b
  injectL :: Proxy (Boring a b) -> Maybe a -> Wedge a b
  injectL _ Nothing = Nowhere
  injectL _ (Just a) = Here a
  injectR :: Proxy (Boring a b) -> Maybe b -> Wedge a b
  injectR _ Nothing = Nowhere
  injectR _ (Just b) = There b


ex1, ex2, ex3, ex4 :: Wedge Int String
ex1 = injectR (Proxy :: Proxy (Boring Int String)) Nothing
ex2 = injectR (Proxy :: Proxy (Boring Int String)) (Just "hello world")
ex3 = injectL (Proxy :: Proxy (Boring Int String)) Nothing
ex4 = injectL (Proxy :: Proxy (Boring Int String)) (Just 13)
