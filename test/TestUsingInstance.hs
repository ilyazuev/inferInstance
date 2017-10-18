{-# LANGUAGE TemplateHaskell #-}
module TestUsingInstance where

import Data.InferInstance (asInstanceOf, WrappedInstance(..), InferInstanceBase)
import Base.Renderable
import Base.Clickable
import Rectangle
import Triangle

callClickableAndRenderable :: [WrappedInstance InferInstanceBase] -> IO()
callClickableAndRenderable testData = do
    putStrLn "\ncall click function: "
    print$ [click c|WrappedInstance c<-[asClickable a|WrappedInstance a<-testData]]
    print$ [click c|WrappedInstance a<-testData, WrappedInstance c<-[asClickable a]]
    putStrLn "\ncall render and click functions: "
    print$ [render r ++ " rendered; " ++ click c|
            WrappedInstance a<-testData, WrappedInstance c<-[asClickable a], WrappedInstance r<-[asRenderable a]]
    where
        asClickable a = $(asInstanceOf ''ClsClickable) a
        asRenderable a = $(asInstanceOf ''ClsRenderable) a