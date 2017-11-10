module Main where
import Game
import Player hiding (Event)
import Hexagon
import Renderable
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
input e = return

view :: Game -> IO Picture
view = return . render

step :: Float -> Game -> IO Game
step f = return
