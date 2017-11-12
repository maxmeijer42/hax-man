module Player where
import Hexagon
import Renderable
import Graphics.Gloss.Data.Color (blue, red, white, green)
import Graphics.Gloss.Data.Picture (Picture(..), circleSolid, arcSolid)

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
            renderPlayer = toPicture (posDirFromPlayer p) $ Color blue $ arcSolid 40 (negate 40) 0.8
            renderScore =  Scale 0.01 0.01 $ Translate 0 200 $ Color white $ Text ("Score: " ++ show (score p))

instance Renderable Dot where
    render d = Color green $ Circle 0.3

instance Renderable PowerPellet where
    render d = Color green $ circleSolid 0.4 

instance Renderable Enemy where
    render e  = uncurry Translate (point $ posDirFromEnemy e) $ Color red $ Circle 0.8

movePlayer :: Player -> Float -> ScaledDirection -> Player
movePlayer p t d = p {posDirFromPlayer = move (posDirFromPlayer p) t d}
