module Renderable where
import Data.Maybe
import Graphics.Gloss (Picture,blank)
class Renderable a where
    render :: a -> Float -> Picture

instance Renderable a => Renderable (Maybe a) where
    render Nothing f = blank
    render (Just x) f = render x f
