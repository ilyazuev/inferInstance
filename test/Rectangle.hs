{-# LANGUAGE RecordWildCards #-}
module Rectangle where

import Data.InferInstance (InferInstanceBase)
import Base.Renderable
import Base.Clickable

data Rectangle = Rectangle {
    name :: String,
    renderableBase :: RenderableBase
    } deriving Show

instance InferInstanceBase Rectangle

instance ClsRenderable Rectangle where
    render Rectangle{..} = "Rectangle " ++ name ++ " " ++ render renderableBase

instance ClsClickable Rectangle where
    click Rectangle{..} = "Rectangle " ++ name ++ " " ++ coords renderableBase ++ " clicked"
