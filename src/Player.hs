module Player where
import Hexagon
data Bonus = Bonus

data Player = Player {
    score :: Int,
    bonus :: Maybe Bonus,
    posDirFromPlayer :: PosDir,
    eatStatus :: Maybe (Event Food),
    fightStatusFromPlayer :: Maybe (Event FightType)
}

data Enemy = Enemy {
    posDirFromEnemy :: PosDir,
    fightStatusFromEnemy :: Maybe (Event FightType)
}

data Dot = Dot deriving Show
data SuperDot = SuperDot deriving Show
data Food = DotFood Dot | SuperDotFood SuperDot

data Event a = Event {
    timeLeft :: Period,
    info :: a
}
data FightType = Winning | Losing