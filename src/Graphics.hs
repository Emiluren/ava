module Graphics where

import Data.Word (Word8)
import Foreign.C.Types (CDouble)
import Foreign.Ptr (Ptr)
import Linear (V2(..), V4(..))
import qualified SFML.Graphics as SFML

import qualified ChipmunkTypes as H
import qualified SpriterTypes as Spriter

type Color = V4 Word8
type Position = V2 CDouble
type Angle = CDouble
type Alpha = CDouble

white :: Color
white = V4 255 255 255 255

blue :: Color
blue = V4 0 0 255 255

data Direction = DLeft | DRight deriving (Eq, Show)

data Renderable
    = AnimatedSprite (Ptr Spriter.CEntityInstance) String (V2 CDouble) Direction
    | StaticSprite SFML.Sprite Position Angle Alpha
    | Line Color Position Position
    | Shape (Ptr H.Shape) H.ShapeType
