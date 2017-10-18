module Main where

import Triangle
import Base.Renderable
import TestInferInstance (printClickableAndSerializable)
import TestUsingInstance (callClickableAndRenderable)
import TestData
import Data.InferInstance (WrappedInstance(..))

main = do
    putStrLn "all objects: "
    print$ [w|w<-testData]
    printClickableAndSerializable testData
    callClickableAndRenderable testData
    callClickableAndRenderable testData2
    where
        testData2 = ( WrappedInstance$ Triangle  "trngl_3" (RenderableBase "(7, 7)") ) : testData
