module Player where
import Hexagon
data Bonus = Bonus

data Player = Player {
    score :: Int,
    bonus :: Maybe Bonus,
    posDirFromPlayer :: PosDir,
    eatStatus :: Maybe (Event Dot),
    fightStatusFromPlayer :: Maybe (Event FightType)
}

data Enemy = Enemy {
    posDirFromEnemy :: PosDir,
    fightStatusFromEnemy :: Maybe (Event FightType)
}

data Dot = SuperDot (Maybe (Event ())) | NormalDot (Maybe (Event ()))

data Event a = Event {
    timeLeft :: Period,
    info :: a
}
data FightType = Winning | Losing