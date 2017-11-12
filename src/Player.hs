{-# LANGUAGE DeriveGeneric #-}
module Player where
import Hexagon
import Renderable
import Data.Aeson
import Data.Maybe (fromJust, isJust, isNothing)
import GHC.Generics (Generic)
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

newtype Dot = Dot {dotEatEvent :: Maybe (Event ())} deriving (Show,Eq, Generic)
newtype PowerPellet = PowerPellet {powerPelletEatEvent :: Maybe (Event ())} deriving (Show,Eq, Generic)

data Event a = Event {
    timeStarted :: Period,
    info :: a
} deriving (Show, Eq,Generic)
data FightType = PlayerWinning | PlayerLosing deriving (Eq)

instance Renderable Player where
    render p f = Pictures [renderPlayer, renderScore]
        where 
            mouthAngle :: Float
            mouthAngle | isJust (eatStatus p) && eatTimeOffset > 0 && eatTimeOffset < 0.5 = 2*(0.5-eatTimeOffset)*40
                       | isJust (eatStatus p) && eatTimeOffset > 0.5 = 0.1
                       | otherwise = 40
            eatTimeOffset = f-(timeStarted.fromJust.eatStatus) p-1
            renderPlayer = toPicture (posDirFromPlayer p) $ Color blue $ arcSolid mouthAngle (negate mouthAngle) 0.8
            renderScore =  Scale 0.01 0.01 $ Translate 0 200 $ Color white $ Text ("Score: " ++ show (score p))

instance Renderable Dot where
    render d _ = Color green $ Circle 0.3

instance Renderable PowerPellet where
    render d _ = Color green $ circleSolid 0.4 

instance Renderable Enemy where
    render e t = uncurry Translate (point $ posDirFromEnemy e) $ Scale 0.6 0.6 $ Rotate rotation $ Color red $ Polygon hexagonPath where
        rotation :: Float
        rotation | isJust fightStatus && (info.fromJust) fightStatus == PlayerLosing = 30 + (t - timeStarted (fromJust fightStatus)) * 500
                 | otherwise = 30
            where
                fightStatus = fightStatusFromEnemy e

instance ToJSON Dot where
    toJSON _ = object []
instance FromJSON Dot where
    parseJSON x = return $ Dot Nothing
instance ToJSON PowerPellet where
    toJSON _ = object []
instance FromJSON PowerPellet where
    parseJSON x = return $ PowerPellet Nothing

movePlayer :: Player -> Float -> ScaledDirection -> Player
movePlayer p t d = p {posDirFromPlayer = move (posDirFromPlayer p) t d}

moveEnemy :: Enemy -> Float -> ScaledDirection -> Enemy 
moveEnemy e t d = e {posDirFromEnemy = move (posDirFromEnemy e) t d}

hitsPlayer :: Enemy -> Player -> Bool
hitsPlayer e p = isNothing (fightStatusFromEnemy e) && distance ((point.posDirFromPlayer) p) ((point.posDirFromEnemy) e) < 0.8

startFighting :: Player -> Float -> Player
startFighting p@Player{bonus=b} t = p{fightStatusFromPlayer = Just $ Event t i} where
    i | isJust b = PlayerWinning
      | otherwise = PlayerLosing

fightPlayer :: Enemy -> Player -> Enemy
fightPlayer enemy player | hitsPlayer enemy player = enemy{fightStatusFromEnemy = fightStatusFromPlayer player}
                         | otherwise = enemy