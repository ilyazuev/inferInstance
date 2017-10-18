{-# LANGUAGE RecordWildCards #-}
module Triangle where

import Data.InferInstance (InferInstanceBase)
import Base.Renderable
import Base.Clickable

data Triangle = Triangle {
    name :: String,
    renderableBase :: RenderableBase
    } deriving Show

instance InferInstanceBase Triangle

instance ClsRenderable Triangle where
    render Triangle{..} = "Triangle " ++ name ++ " " ++ render renderableBase

instance ClsClickable Triangle where
    click Triangle{..} = "Triangle " ++ name ++ " " ++ coords renderableBase ++ " clicked"
