module Declarations where

class Cls1 a where fn1 :: a -> a

class Cls2 a where fn2 :: a -> a

data A = A deriving Show
instance Cls1 A where fn1 x = x

data B = B deriving Show
instance Cls1 B where fn1 x = x
instance Cls2 B where fn2 x = x