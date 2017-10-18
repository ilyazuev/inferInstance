module Base.Renderable where

class ClsRenderable a where
    render::a->String

data RenderableBase = RenderableBase {coords::String} deriving Show

instance ClsRenderable RenderableBase where
    render a = coords a
