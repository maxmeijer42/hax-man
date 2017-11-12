module Main where
import Game
import Player hiding (Event)
import Hexagon
import Renderable
import Input
import System.Random
import System.Environment
import Data.Aeson
import Debug.Trace
import qualified Data.ByteString.Lazy as B (writeFile,readFile)
import Control.Arrow ((>>>))
import Data.Set (insert, delete)
import Graphics.Gloss.Interface.IO.Game
import Data.List
import Data.Maybe

main :: IO ()
main = do   
    args <- getArgs
    if null args 
        then startGame initial
        else do
            content <- B.readFile (head args)
            startGame (loadLevel content)

startGame game = playIO (InWindow "Haxman" (400, 400) (0, 0)) -- Or FullScreen
                    black            -- Background color
                    10               -- Frames per second
                    game             -- Initial state
                    view             -- View function
                    input            -- Event function
                    step             -- Step function

input :: Event -> Game -> IO Game
input (EventKey (Char 's') Down _ _) g = do
    B.writeFile "level.json" $ encode (level g)
    return g
input (EventKey (SpecialKey KeySpace) Down _ _) g = return g {paused = not (paused g)}
input (EventKey k   Down _ _) g = return g {keysPressed = Data.Set.insert k (keysPressed g)}
input (EventKey k   Up   _ _) g = return g {keysPressed = Data.Set.delete k (keysPressed g)}
input _ g = return g

view :: Game -> IO Picture
view g = return (g `render` gameTime g)

step :: Float -> Game -> IO Game
step f game = if paused game then return game else spawnEnemies getStdGen purePart
    where
        purePart = finishEatingDots >>> startEatingDots >>> finishBonus
                        $ g { player = movedPlayer, enemies = movedEnemies }

        g :: Game
        g = game { gameTime = gameTime game + f}

        movedPlayer :: Player
        movedPlayer = movePlayer (player g) f dir

        movedEnemies :: [Enemy]
        movedEnemies = map bestMoveEnemy (enemies g)
        
        bestMoveEnemy e = moveEnemy e f optimalDirection
            where
                possibleDirections = directionsEnemy e
                lengthOfSteps = map getLengthOfDirection possibleDirections
                getLengthOfDirection dir = distance (point (fromPosition (Hexagon.translate (position (posDirFromEnemy e)) (unscaled dir)))) (point (posDirFromPlayer(player g)))
                optimalDirection = possibleDirections !! fromJust (elemIndex (minimum lengthOfSteps) lengthOfSteps)

        dir :: ScaledDirection
        -- Take one of the directions or noDirection if there are none
        dir = head $ directions ++ [(noSpeed . unscaled . direction . posDirFromPlayer . player) g]

        directions :: [ScaledDirection]
        directions = map (ScaledDirection 1) (filter allowed chosenDirections)

        directionsEnemy :: Enemy -> [ScaledDirection]
        directionsEnemy e = map (ScaledDirection 1) (filter (allowedEnemy e) [NorthEast, East, SouthEast, SouthWest, West, NorthWest])

        allowed :: Direction -> Bool
        allowed = canMove g (posDirFromPlayer (player g))

        allowedEnemy :: Enemy -> Direction -> Bool
        allowedEnemy e = canMove g (posDirFromEnemy e)

        -- The directions that are chosen follow from the combination of
        -- keys pressed by the user.
        chosenDirections :: [Direction]
        chosenDirections = combineDirections $ activeDirections g

