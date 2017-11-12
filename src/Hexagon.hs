module Hexagon where
import Control.Monad (join)
import Control.Arrow ((***),(>>>),(&&&))
import Data.List (elemIndex, sortBy)
import Data.Maybe (fromJust)
import Data.Ratio ((%),denominator)
import System.Random
import Graphics.Gloss (Point)
import Graphics.Gloss.Data.Vector (Vector, magV, mulSV, normalizeV, dotV)

-- Represents a position in the grid
data Position = Position {x :: Int, y :: Int} deriving (Show, Eq)

data Direction = NorthEast | East | SouthEast | SouthWest | West | NorthWest
    deriving (Enum, Eq, Show, Ord, Bounded)

data ScaledDirection = ScaledDirection {
    scale :: Rational,
    unscaled :: Direction
}
noDirection = ScaledDirection 0 NorthEast

data PosDir = PosDir{
    position :: Position,
    direction :: ScaledDirection,
    point :: Point
}

fromPosition :: Position -> PosDir
fromPosition p = PosDir p noDirection (center p)

-- A period of time
type Period = Float

class Vectorizable a where
    toVector :: a -> Vector

instance Vectorizable Direction where
    toVector NorthEast = normalizeV (1,2)
    toVector East      = (1,0)
    toVector SouthEast = normalizeV (1,-2)
    toVector SouthWest = normalizeV (-1,-2)
    toVector West      = (-1,0)
    toVector NorthWest = normalizeV (-1,2)

instance Vectorizable ScaledDirection where
    toVector (ScaledDirection s d) = fromRational s `mulSV` toVector d

-- Calculates the distance between two points
distance :: Point -> Point -> Float
distance a b = magV (a-b)

-- Calculates the center of a hexagonal cell on the grid
center :: Position -> Point
center (Position x y) = (fromIntegral (2*x + ((y+1) `mod` 2)), negate (fromIntegral y * sqrt 3.0))

translate :: Position -> Direction -> Position
translate (Position x y) = combinedWith . offsets
  where 
    combinedWith :: (Int, Int) -> Position
    combinedWith (a,b) = Position (x+a) (y+b)
    offsets :: Direction -> (Int, Int)
    -- because we are storing the hexagonal grid in a 2 dimensional array
    -- we need to check the parity of the y coordinate
    offsets NorthEast = ((y+1) `mod` 2, -1)
    offsets East      = (1, 0)
    offsets SouthEast = ((y+1) `mod` 2, 1)
    offsets SouthWest = (-(y `mod` 2), 1)
    offsets West      = (-1,0)
    offsets NorthWest = (-(y `mod` 2), -1)

-- The position of the cell we're heading towards
nextPosition :: PosDir -> Position
nextPosition (PosDir pos (ScaledDirection 0 _) _) = pos
nextPosition (PosDir pos d _)                     = pos `translate` unscaled d

middle :: [Point] -> Point
middle xs = join (***) (/(fromIntegral $ length xs)) $ sum xs

timeToNextPosition :: PosDir -> Period
timeToNextPosition pd@(PosDir pos d point) = dist / fromRational (scale d)
    where dist = distance point (center (nextPosition pd))

move :: PosDir -> Period -> ScaledDirection -> PosDir
move pd@(PosDir pos d point) t nextD
  | t<=0 || stopped = pd

  -- If we can reach a turning point / cell center:
  -- Go to the cell center
  -- Turn around into the new direction, if needed
  -- Spend the time that is left moving in the new direction
  | reachingCellCenter = move (PosDir nextPos nextD nextPoint) timeStarted nextD

  -- Otherwise move on in the same direction as before
  | otherwise = PosDir pos d pointAhead
  
  where 
    stopped = scale d == 0 && scale nextD == 0

    pointAhead = point + mulSV (t * fromRational (scale d)) (normalizeV (nextPoint - point))
    nextPos = nextPosition pd
    nextPoint = center nextPos
    oldPoint = center pos

    closerToOldPointThan :: Point -> Point -> Bool
    a `closerToOldPointThan` b = distance oldPoint a - distance oldPoint b < 0.001

    reachingCellCenter = scale d == 0 || nextPoint `closerToOldPointThan` pointAhead

    timeStarted | scale d == 0 = t
                | otherwise = t - timeToNextPosition pd

combineDirections :: [Direction] -> [Direction]
combineDirections d | length d <= 1 = d
                    | magV avgVector < 0.1 = []
                    | fst (head dotProducts) - fst (dotProducts!!1) > 0.01 = [snd $ head dotProducts]
                    | otherwise = map snd $ take 2 dotProducts
    where
        avgVector = middle $ map toVector d
        dotProduct :: Direction -> Float
        dotProduct = dotV (normalizeV avgVector) . toVector
        dotProducts :: [(Float, Direction)]
        dotProducts = sortBy (flip compare) $ map (dotProduct &&& id) [NorthEast .. ]

isNextTo :: Position -> Position -> Bool
isNextTo a b = any (\d -> a `translate` d == b) [NorthEast .. ] 
