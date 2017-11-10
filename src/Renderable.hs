module Renderable where
    import Graphics.Gloss (Picture)
    class Renderable a where
        render :: a -> Picture