{-# LANGUAGE TemplateHaskell #-}
module TestUsingInstance where

import Data.InferInstance (asInstanceOf, asWrappedInstanceOf, WrappedInstance(..), InferInstanceBase)
import Base.Renderable
import Base.Clickable
import Base.Serializable
import Circle
import Rectangle
import Triangle

callClickableAndRenderable :: [WrappedInstance InferInstanceBase] -> IO()
callClickableAndRenderable testData = do
    putStrLn "\ncall serialize function: "
    print$ [serialize c|WrappedInstance c<-[asSerializable a|WrappedInstance a<-testData]]
    putStrLn "\ncall click function: "
    print$ [click c|WrappedInstance c<-[asClickable w|w<-testData]]
    print$ [click c|w<-testData, WrappedInstance c<-[asClickable w]]
    putStrLn "\ncall render and click functions: "
    print$ [render r ++ " rendered; " ++ click c|
            w<-testData, WrappedInstance c<-[asClickable w], WrappedInstance r<-[asRenderable w]]
    where
        asSerializable w = $(asInstanceOf ''ClsSerializable) w
        asClickable w = $(asWrappedInstanceOf ''ClsClickable) w

asRenderable w = $(asWrappedInstanceOf ''ClsRenderable) w