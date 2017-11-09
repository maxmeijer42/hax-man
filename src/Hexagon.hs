module Hexagon where
import Graphics.Gloss (Point)
import Graphics.Gloss.Data.Vector (Vector, magV, mulSV, normalizeV)

-- Represents a position in the grid
data Position = Position {x :: Int, y :: Int}

data Direction = NorthEast | East | SouthEast | SouthWest | West | NorthWest

data ScaledDirection = ScaledDirection {
    scale :: Float,
    unscaled :: Direction
}

data PosDir = PosDir{
    position :: Position,
    direction :: ScaledDirection,
    point :: Point
}

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
    toVector (ScaledDirection s d) = s `mulSV` toVector d

-- Calculates the distance between two points
distance :: Point -> Point -> Float
distance a b = magV (a-b)

-- Calculates the center of a hexagonal cell on the grid
center :: Position -> Point
center (Position x y) = (fromIntegral (2*x + ((y+1) `mod` 2)), fromIntegral y * sqrt 3)

translate :: Position -> Direction -> Position
translate (Position x y) = combinedWith . offsets
  where 
    combinedWith :: (Int, Int) -> Position
    combinedWith (a,b) = Position (x+a) (y+b)
    negate (a,b) = (-a,-b)
    offsets :: Direction -> (Int, Int)
    -- because we are storing the hexagonal grid in a 2 dimensional array
    -- we need to check the parity of the y coordinate
    offsets NorthEast = ((y+1) `mod` 2, -1)
    offsets East      = (1, 0)
    offsets SouthEast = ((y+1) `mod` 2, 1)
    offsets SouthWest = negate $ offsets NorthEast
    offsets West      = negate $ offsets East
    offsets NorthWest = negate $ offsets SouthEast

-- The position of the cell we're heading towards
nextPosition :: PosDir -> Position
nextPosition (PosDir pos (ScaledDirection 0 _) _) = pos
nextPosition (PosDir pos d _)                     = pos `translate` unscaled d

move :: PosDir -> Period -> ScaledDirection -> PosDir
move pd@(PosDir pos d point) t nextD
  | stopped = pd

  -- If we can reach a turning point / cell center:
  -- Go to the cell center
  -- Turn around into the new direction, if needed
  -- Spend the time that is left moving in the new direction
  | reachingCellCenter = move (PosDir nextPos nextD nextPoint) timeLeft nextD

  -- Otherwise move on in the same direction as before
  | otherwise = PosDir pos d pointAhead
  
  where 
    stopped = scale d == 0 && scale nextD == 0

    pointAhead = point + toVector d
    nextPos = nextPosition pd
    nextPoint = center nextPos

    closerToPointThan :: Point -> Point -> Bool
    a `closerToPointThan` b = distance point a < distance point b

    reachingCellCenter = stopped || pointAhead `closerToPointThan` nextPoint

    -- The time it will take to reach the center of the next cell
    timeLeft = t - (distance point nextPoint / scale d)
