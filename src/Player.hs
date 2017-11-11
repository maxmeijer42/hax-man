module Player where
import Hexagon
import Renderable
import Graphics.Gloss.Data.Color (blue, red, white)
import Graphics.Gloss.Data.Picture (Picture(..), circleSolid)
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
data PowerPellet = SuperDot deriving (Show,Eq)
data Food = DotFood Dot | PowerPelletFood PowerPellet

data Event a = Event {
    timeLeft :: Period,
    info :: a
}
data FightType = Winning | Losing

instance Renderable Player where
    render p = Pictures [renderPlayer, renderScore]
        where 
            renderPlayer = uncurry Translate (point $ posDirFromPlayer p) $ Color blue $ Circle 0.8
            renderScore =  Scale 0.01 0.01 $ Translate 0 200 $ Color white $ Text ("Score: " ++ show (score p))

instance Renderable Dot where
    render d = Color red $ Circle 0.3

instance Renderable PowerPellet where
    render d = Color red $ circleSolid 0.4 

movePlayer :: Player -> Float -> ScaledDirection -> Player
movePlayer p t d = p {posDirFromPlayer = move (posDirFromPlayer p) t d}
