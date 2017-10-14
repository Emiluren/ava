{-# LANGUAGE DeriveGeneric #-}
module Level
    ( EnemyType(..)
    , ObjectType(..)
    , LevelData(..)
    , testLevel
    ) where

import Data.Aeson (FromJSON(..), ToJSON(..), defaultOptions, toEncoding, genericToEncoding)
import Foreign.C.Types (CDouble)
import GHC.Generics

import qualified ChipmunkTypes as H

data EnemyType = Mummy | Zombie deriving (Eq, Show, Generic)
instance ToJSON EnemyType where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON EnemyType

data ObjectType = Ball | Box deriving (Eq, Show, Generic)
instance ToJSON ObjectType where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ObjectType

data LevelData = LevelData
    { playerStartPosition :: H.Vector
    , wallEdges :: [(H.Vector, H.Vector)]
    , initEnemies :: [(EnemyType, H.Vector)]
    , extraObjects :: [(ObjectType, H.Vector)]
    } deriving (Generic, Show, Eq)

instance ToJSON LevelData where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON LevelData

makeHVector :: (CDouble, CDouble) -> H.Vector
makeHVector = uncurry H.Vector

shelfStart, shelfEnd :: Num a => (a, a)
shelfStart = (100, 200)
shelfEnd = (300, 320)

startV, endV :: H.Vector
(startV, endV) = (makeHVector shelfStart, makeHVector shelfEnd)

testLevel :: LevelData
testLevel =
    let tl = H.Vector 0 0
        bl = H.Vector 0 400
        tr = H.Vector 400 0
        br = H.Vector 400 400

        corners = [tl, H.Vector (-200) (-10), H.Vector (-200) 360, bl, H.Vector 150 380, br, tr]
        edges = zip corners $ tail corners ++ [head corners]
    in LevelData
       { wallEdges = (startV, endV) : edges
       , playerStartPosition = H.Vector 240 100
       , initEnemies = [(Mummy, H.Vector 110 200)]
       , extraObjects = [(Ball, H.Vector 200 20)]
       }
