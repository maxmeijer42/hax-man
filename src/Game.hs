module Game where
import Player
import Hexagon
newtype Level = Level [[Cell]]

data Game = Game Player Level [Enemy]

data Cell = Cell {
    cellPosition :: Position,
    cellContent :: CellContent
}
data CellContent = Path (Maybe Dot) (Maybe SuperDot) | Wall

levelFromCellContents :: [[CellContent]] -> Level
levelFromCellContents ccss = Level $ zipWith' Cell positionGrid ccss
    where positionGrid = 
            [
                [Position x y | x <- [0..length$ccss!!(y-1)]] 
                | y <- [0..length ccss-1]
            ]
          zipWith' :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
          zipWith' f = zipWith (zipWith f)

surroundWithWalls :: [[CellContent]] -> [[CellContent]]
surroundWithWalls ccss = [top] ++ map surround ccss ++ [bottom]
    where nWalls n = replicate n Wall
          top = nWalls (1 + (length . head) ccss) 
          surround x = [Wall] ++ x ++ [Wall]
          bottom = nWalls (1 + (length . tail) ccss)

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
    initial = Game initial initial []
