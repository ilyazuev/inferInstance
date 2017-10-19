Library for solving of problem described in the article: "Fully-functional heterogeneous lists - HaskellWiki"

https://wiki.haskell.org/Fully-functional_heterogeneous_lists

Consider we have 2 classes and 2 types.
And we will define heterogeneous list for different types.
With "InferInstance" library we can infer to which class belongs the instance of the corresponding type.
And also we can call functions defined in the corresponding class.
See example:


```haskell
-- Declarations.hs
module Declarations where

class Cls1 a where fn1 :: a -> a

class Cls2 a where fn2 :: a -> a

data A = A deriving Show
instance Cls1 A where fn1 x = x

data B = B deriving Show
instance Cls1 B where fn1 x = x
instance Cls2 B where fn2 x = x
```


```haskell
-- Example.hs
{-# LANGUAGE TemplateHaskell #-}
import Data.InferInstance
import Declarations

instance InferInstanceBase A
instance InferInstanceBase B

testData :: [WrappedInstance InferInstanceBase]
testData = [WrappedInstance A, WrappedInstance B]

main = do
    print [w|w<-testData, isCls1 w]
    print [w|w<-testData, isCls2 w]
    print [show$ fn2 a|w<-testData, WrappedInstance a<-[asCls2 w]]
    where
        isCls1 w = $(wrappedInstanceOf ''Cls1) w
        isCls2 w = $(wrappedInstanceOf ''Cls2) w
        asCls2 w = $(asWrappedInstanceOf ''Cls2) w

-- output:
--[A,B]
--[B]
--["B"]
```
