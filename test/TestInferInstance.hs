{-# LANGUAGE TemplateHaskell #-}
module TestInferInstance where

import Data.InferInstance (instanceOf, instanceWrapOf, WrappedInstance(..), InferInstanceBase)
import Base.Clickable
import Base.Serializable
import Circle
import Rectangle
import Triangle

printClickableAndSerializable :: [WrappedInstance InferInstanceBase] -> IO()
printClickableAndSerializable testData = do
    putStrLn "\nclickable objects: "
    print$ [w|w<-testData, $(instanceWrapOf ''ClsClickable) w]
    putStrLn "\nserializable objects: "
    print$ [w|w@(WrappedInstance a)<-testData, isSerializable a]

isSerializable a = $(instanceOf ''ClsSerializable) a
