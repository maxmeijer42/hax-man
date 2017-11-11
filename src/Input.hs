module Input where
import Hexagon
import Graphics.Gloss.Interface.IO.Game
import Game
import Data.Set (toList)
import Data.Maybe

toDirection :: Key -> Maybe Direction
toDirection (Char 'e') = Just NorthEast
toDirection (Char 'd') = Just East
toDirection (Char 'x') = Just SouthEast
toDirection (Char 'z') = Just SouthWest
toDirection (Char 'a') = Just West
toDirection (Char 'w') = Just NorthWest
toDirection _ = Nothing

activeDirections :: Game -> [Direction]
activeDirections = mapMaybe toDirection . toList . keysPressed