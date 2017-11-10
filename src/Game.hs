module Game where
import Player
import Hexagon
import Renderable
import Data.Set (Set,empty)
import Control.Arrow ((&&&),(>>>))
import Graphics.Gloss.Data.Color (green, red, greyN)
import Graphics.Gloss.Interface.IO.Game (Key)
import Graphics.Gloss.Data.Picture (Picture(..),lineLoop,Path,Point)
newtype Level = Level [[Cell]] deriving Show

data Game = Game {
    player :: Player,
    level :: Level,
    enemies :: [Enemy],
    keysPressed :: Set Key
}

data Cell = Cell {
    cellPosition :: Position,
    cellContent :: CellContent
} deriving Show
data CellContent = Path (Maybe Dot) (Maybe SuperDot) | Wall deriving Show

levelFromCellContents :: [[CellContent]] -> Level
levelFromCellContents ccss = Level $ zipWith' Cell positionGrid ccss
    where positionGrid = 
            [
                [Position x y | x <- [0..length (ccss!!y)-1]]
                | y <- [0..length ccss-1]
            ]
          zipWith' :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
          zipWith' f = zipWith (zipWith f)

surroundWithWalls :: [[CellContent]] -> [[CellContent]]
surroundWithWalls ccss = [top] ++ map surround ccss ++ [bottom]
    where nWalls n = replicate n Wall
          top = nWalls (1 + (length . head) ccss) 
          surround x = [Wall] ++ x ++ [Wall]
          bottom = nWalls (1 + (length . last) ccss + (length ccss-1) `mod` 2)

class Initial a where
    initial :: a

instance Initial Player where
    initial = Player{
        score = 0,
        bonus = Nothing,
        posDirFromPlayer = PosDir pos dir (center pos),
        eatStatus = Nothing,
        fightStatusFromPlayer = Nothing
    }
        where pos = Position 1 1
              dir = ScaledDirection 1 NorthEast

instance Initial Level where
        initial = levelFromCellContents $ surroundWithWalls $ replicate 5 $ replicate 5 $ Path Nothing Nothing

instance Initial Game where
    initial = Game initial initial [] empty

instance Renderable CellContent where
    render Wall = Color green $ Circle 1
    render (Path _ _) = Color red $ Circle 1

hexagonPath :: Path
-- every vertex is the middle of the centers of 3 hexagons
-- so for each 2 adjacent neighbouring hexagons, calculate the centroid
hexagonPath = zipWith hexagonPoint (shift otherCenters) otherCenters
    where
        shift :: [a] -> [a]
        shift (x:xs) = xs ++ [x]
        hexagonPoint :: Point -> Point -> Point
        hexagonPoint x y = middle [center (Position 0 0),x,y] - center (Position 0 0)
        otherCenters :: [Point]
        otherCenters = map (center . translate (Position 0 0)) [NorthEast .. NorthWest]

instance Renderable Cell where
    render (Cell pos content) = uncurry Translate (center pos) pic
        where outline = Color (greyN 0.5) $ lineLoop hexagonPath
              pic = Pictures [outline, render content]

instance Renderable Level where
    render (Level css) = Pictures $ map render (concat css)

instance Renderable Game where
    render g = Translate (negate 100) 100 . Scale 20 20 $ render'
        where
            render' = Pictures [render $ level g, render $ player g]
