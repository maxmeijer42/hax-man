module Game where
import Player
import Hexagon
newtype Level = Level [[Cell]]

data Game = Game Player Level [Enemy]

data Cell = Path (Maybe Dot) (Maybe SuperDot) | Wall

initialGame :: Game
initialGame = Game Player{
    score = 0,
    bonus = Nothing,
    posDirFromPlayer = PosDir pos (ScaledDirection 1 NorthEast) (center pos),
    eatStatus = Nothing,
    fightStatusFromPlayer = Nothing
} (Level $ replicate 2 $ replicate 2 $ Path Nothing Nothing) []
    where pos = Position 1 1
