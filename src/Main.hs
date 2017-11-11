module Main where
import Game
import Player hiding (Event)
import Hexagon
import Renderable
import Input
import Data.Set (insert, delete)
import Graphics.Gloss.Interface.IO.Game
main :: IO ()
main = playIO (InWindow "Haxman" (400, 400) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initial          -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function

input :: Event -> Game -> IO Game
input (EventKey k Down _ _) g = return g {keysPressed = insert k (keysPressed g)}
input (EventKey k Up _ _)   g = return g {keysPressed = delete k (keysPressed g)}
input _ g = return g

view :: Game -> IO Picture
view = return . render

step :: Float -> Game -> IO Game
step f g = return g { player = movePlayer (player g) f $ headOrZero $ combineDirections $ activeDirections g }
    where
        headOrZero :: [Direction] -> ScaledDirection
        headOrZero [] = ScaledDirection 0 NorthEast
        headOrZero (x:xs) = ScaledDirection 1 x