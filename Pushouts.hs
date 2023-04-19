{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

module Pushouts  where

import Data.Proxy
import Data.Ratio
import Data.Scientific

class Pair tag x a b | tag -> x, tag -> a, tag -> b where
  projectL :: Proxy tag -> x -> a
  projectR :: Proxy tag -> x -> b

-- Law: injectL t . projectL t === injectR t . projectR t
class Pair tag x a b => Pushout tag x a b where
  type PushoutTo tag x a b
  injectL :: Proxy tag -> a -> PushoutTo tag x a b
  injectR :: Proxy tag -> b -> PushoutTo tag x a b



data Boring a b

-- This is the pushout of `Maybe a <- () -> Maybe b`
data Wedge a b = Nowhere | Here a | There b
  deriving Show

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


data X

instance Pair X Integer Scientific Rational where
  projectL :: Proxy X -> Integer -> Scientific
  projectL _ = fromInteger
  projectR :: Proxy X -> Integer -> Rational
  projectR _ = fromInteger

data P = I Integer | NIReal Scientific | NIRational Rational
  deriving (Show)

instance Pushout X Integer Scientific Rational where
  type PushoutTo X Integer Scientific Rational = P
  injectL :: Proxy X -> Scientific -> P
  injectL _ real = if real == fromInteger (floor real) then I (floor real) else NIReal real
  injectR :: Proxy X -> Rational -> P
  injectR _ rational = if rational == fromInteger (floor rational) then I (floor rational) else NIRational rational

ex5, ex6, ex7, ex8 :: P
ex5 = injectL (Proxy :: Proxy X) 15.0
ex6 = injectR (Proxy :: Proxy X) (15 % 1)
ex7 = injectL (Proxy :: Proxy X) 15.1
ex8 = injectR (Proxy :: Proxy X) (15 % 14)

