module Game where
import Player
import Hexagon
import Renderable
import System.Random
import Data.Maybe (isJust)
import Data.Set (Set,empty)
import Control.Arrow ((&&&),(>>>))
import Graphics.Gloss.Data.Color (green, red, greyN, white)
import Graphics.Gloss.Interface.IO.Game (Key)
import Graphics.Gloss.Data.Picture (Picture(..),lineLoop,Path,Point)
newtype Level = Level [[Cell]] deriving Show
getCell :: Level -> Position -> Cell
getCell (Level css) (Position x y) = (css!!y)!!x
setCell :: Level -> Position -> CellContent -> Level
setCell (Level css) p cc = Level $ map (map apply) css
    where
        apply :: Cell -> Cell
        apply cell | cellPosition cell == p = cell {cellContent = cc}
                   | otherwise = cell

data Game = Game {
    player :: Player,
    level :: Level,
    enemies :: [Enemy],
    keysPressed :: Set Key,
    gameTime :: Float,
    paused :: Bool
}

data Cell = Cell {
    cellPosition :: Position,
    cellContent :: CellContent
} deriving Show
data CellContent = Path {
    pathDot :: Maybe Dot,
    pathPowerPellet :: Maybe PowerPellet
} | Wall deriving (Show,Eq)

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
              dir = noSpeed East

instance Initial Level where
        initial = levelFromCellContents $ surroundWithWalls $ replicate 5 $ replicate 5 $ Path (Just $ Dot Nothing) Nothing

instance Initial Game where
    initial = Game initial initial [] empty 0 False

instance Renderable CellContent where
    render Wall = Color green $ Circle 1
    render (Path d s) = Pictures [render d, render s]

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
            pause | paused g = [Scale 0.02 0.02 $ Translate 0 (negate 200) $ Color white $ Text "PAUSED"]
                  | otherwise = []
            render' = Pictures $ pause ++ [render $ level g, render $ player g] ++ map render (enemies g)

canMove :: Game -> PosDir -> Direction -> Bool
canMove Game{level=l} pd d = cellContent (getCell l (translate (nextPosition pd) d)) /= Wall

startEatingDots :: Game -> Game
startEatingDots g = when (hasNewDot content || hasNewPowerPellet content) updateGame g where
    hasNewDot :: CellContent -> Bool
    hasNewDot (Path (Just (Dot Nothing)) _) = True
    hasNewDot _ = False
    hasNewPowerPellet :: CellContent -> Bool
    hasNewPowerPellet (Path _ (Just (PowerPellet Nothing))) = True
    hasNewPowerPellet _ = False
    eatPosition :: Game -> Position
    eatPosition = nextPosition . posDirFromPlayer . player
    content :: CellContent
    content = cellContent (getCell (level g) (eatPosition g))
    updateGame :: Game -> Game
    updateGame g = g {
        player = (player g){eatStatus = eatEvent},
        level = setCell (level g) (eatPosition g) (updateContent content)
    } where
        eatEvent = Just $ Event (gameTime g) ()
        updateContent :: CellContent -> CellContent
        updateContent = 
            when (hasNewDot content) 
                (\c->c{pathDot = Just $ Dot eatEvent}) 
            >>> when (hasNewPowerPellet content) 
                (\c->c{pathPowerPellet = Just $ PowerPellet eatEvent})

when :: Bool -> (a->a) -> a -> a
when False = const id
when True  = ($)

finishEatingDots :: Game -> Game
finishEatingDots g@Game{player = p, level = l} = g
        {
            level = setCell l eatPosition (Path Nothing Nothing),
            player = 
                when hasPowerPellet giveBonus >>>
                when hasDot incrementScore
                $ p
        }
    where
        giveBonus :: Player -> Player
        giveBonus p = p{bonus = Just $ Event (gameTime g) ()}

        incrementScore :: Player -> Player
        incrementScore p = p{score = score p + 1}

        hasDot = isJust $ pathDot content
        hasPowerPellet = isJust $ pathPowerPellet content
        content = cellContent (getCell (level g) eatPosition)
        eatPosition = (position . posDirFromPlayer . player) g

bonusLength :: Period
bonusLength = 5.0

finishBonus :: Game -> Game
finishBonus g@Game{gameTime = t, player = Player{bonus=(Just Event{timeStarted = b})}} = 
    if b+bonusLength<t 
        then g{player=(player g){bonus=Nothing}} 
        else g
finishBonus g = g

isAvailable :: Game -> Position -> Bool
isAvailable Game{player=p,level=l,enemies=es} pos = 
    not (posDirFromPlayer p `isClose` pos) && 
    cellContent (getCell l pos) /= Wall && 
    not (any (\e->posDirFromEnemy e `isClose` pos) es)
        where
            isClose :: PosDir -> Position -> Bool
            isClose pd pos = (pos `isNextTo` nextPosition pd) || (pos `isNextTo` position pd)

availablePositions :: Game -> [Position]
availablePositions game@Game{level=Level css}= filter (isAvailable game) (map cellPosition $ concat css)

randomElement :: RandomGen g => [a] -> g -> a
randomElement possibilities g = possibilities!!fst (randomR (0,length possibilities-1) g)

spawnEnemy :: RandomGen g => Game -> g -> Game
spawnEnemy game@Game{player=p,enemies = e} gen 
    | null (availablePositions game) = game
    | otherwise = game{enemies = Enemy (fromPosition pos) Nothing : e}
    where pos = randomElement (availablePositions game) gen
    
spawnEnemies :: RandomGen g => IO g -> Game -> IO Game
spawnEnemies ioGen game = spawnEnemy game <$> ioGen