{-# LANGUAGE RecordWildCards #-}
module Circle where

import Data.InferInstance (InferInstanceBase)
import Base.Renderable
import Base.Serializable

data Circle = Circle {
    name :: String,
    renderableBase :: RenderableBase,
    serializableBase :: SerializableBase
    } deriving Show

instance InferInstanceBase Circle

instance ClsRenderable Circle where
    render Circle{..} = "Circle " ++ name ++ " " ++ render renderableBase

instance ClsSerializable Circle where
    serialize Circle{..} = "Circle " ++ name ++ " " ++ serialize serializableBase
