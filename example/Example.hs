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
