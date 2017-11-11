module Player where
import Hexagon
import Renderable
import Graphics.Gloss.Data.Color (blue, red, white)
import Graphics.Gloss.Data.Picture (Picture(..), circleSolid)

data Player = Player {
    score :: Int,
    bonus :: Maybe (Event ()),
    posDirFromPlayer :: PosDir,
    eatStatus :: Maybe (Event ()),
    fightStatusFromPlayer :: Maybe (Event FightType)
}

data Enemy = Enemy {
    posDirFromEnemy :: PosDir,
    fightStatusFromEnemy :: Maybe (Event FightType)
}

newtype Dot = Dot {dotEatEvent :: Maybe (Event ())} deriving (Show,Eq)
newtype PowerPellet = PowerPellet {powerPelletEatEvent :: Maybe (Event ())} deriving (Show,Eq)

data Event a = Event {
    timeStarted :: Period,
    info :: a
} deriving (Show, Eq)
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
