{-# LANGUAGE TemplateHaskell #-}
module TestInferInstance where

import Data.InferInstance (instanceOf, wrappedInstanceOf, WrappedInstance(..), InferInstanceBase)
import Base.Clickable
import Base.Serializable
import Circle
import Rectangle
import Triangle

printClickableAndSerializable :: [WrappedInstance InferInstanceBase] -> IO()
printClickableAndSerializable testData = do
    putStrLn "\nclickable objects: "
    print$ [w|w<-testData, isClsClickable w]
    putStrLn "\nserializable objects: "
    print$ [w|w@(WrappedInstance a)<-testData, isSerializable a]
    where
        isSerializable a = $(instanceOf ''ClsSerializable) a

isClsClickable w = $(wrappedInstanceOf ''ClsClickable) w
