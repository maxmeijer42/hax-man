module Main where
import Game
import Player hiding (Event)
import Hexagon
import Renderable
import Input
import System.Random
import Control.Arrow ((>>>))
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
input (EventKey (SpecialKey KeySpace) Down _ _) g = return g {paused = not (paused g)}
input (EventKey k   Down _ _) g = return g {keysPressed = insert k (keysPressed g)}
input (EventKey k   Up   _ _) g = return g {keysPressed = delete k (keysPressed g)}
input _ g = return g

view :: Game -> IO Picture
view = return . render

step :: Float -> Game -> IO Game
step f game = if paused game then return game else spawnEnemies getStdGen purePart
    where
        purePart = finishEatingDots >>> startEatingDots >>> finishBonus
                        $ g { player = movedPlayer }

        g :: Game
        g = game { gameTime = gameTime game + f}

        movedPlayer :: Player
        movedPlayer = movePlayer (player g) f direction

        direction :: ScaledDirection
        -- Take one of the directions or noDirection if there are none
        direction = head $ directions ++ [noDirection]

        directions :: [ScaledDirection]
        directions = map (ScaledDirection 1) (filter allowed chosenDirections)

        allowed :: Direction -> Bool
        allowed = canMove g (posDirFromPlayer (player g))

        -- The directions that are chosen follow from the combination of
        -- keys pressed by the user.
        chosenDirections :: [Direction]
        chosenDirections = combineDirections $ activeDirections g
