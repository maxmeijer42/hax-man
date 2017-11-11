module Renderable where
import Data.Maybe
import Graphics.Gloss (Picture,blank)
class Renderable a where
    render :: a -> Picture

instance Renderable a => Renderable (Maybe a) where
    render Nothing = blank
    render (Just x) = render x