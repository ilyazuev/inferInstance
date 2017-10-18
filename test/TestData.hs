module TestData(testData) where

import Base.Renderable
import Base.Clickable
import Base.Serializable
import Circle
import Rectangle
import Triangle
import Data.InferInstance (WrappedInstance(..), InferInstanceBase)

testData :: [WrappedInstance InferInstanceBase]
testData = [
    WrappedInstance$ Circle    "crcl_1"  (RenderableBase "(1, 1)") (SerializableBase "Crcl1"),
    WrappedInstance$ Circle    "crcl_2"  (RenderableBase "(2, 2)") (SerializableBase "Crcl2"),
    WrappedInstance$ Rectangle "rect_1"  (RenderableBase "(3, 3)"),
    WrappedInstance$ Rectangle "rect_2"  (RenderableBase "(4, 4)"),
    WrappedInstance$ Triangle  "trngl_1" (RenderableBase "(5, 5)"),
    WrappedInstance$ Triangle  "trngl_2" (RenderableBase "(6, 6)")
    ]