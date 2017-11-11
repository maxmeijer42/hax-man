module Player where
import Hexagon
import Renderable
import Graphics.Gloss.Data.Color (blue)
import Graphics.Gloss.Data.Picture (Picture(..))
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

data Dot = Dot deriving (Show,Eq)
data SuperDot = SuperDot deriving (Show,Eq)
data Food = DotFood Dot | SuperDotFood SuperDot

data Event a = Event {
    timeLeft :: Period,
    info :: a
}
data FightType = Winning | Losing

instance Renderable Player where
    render p = uncurry Translate (point $ posDirFromPlayer p) $ Color blue $ Circle 0.8

movePlayer :: Player -> Float -> ScaledDirection -> Player
movePlayer p t d = p {posDirFromPlayer = move (posDirFromPlayer p) t d}
