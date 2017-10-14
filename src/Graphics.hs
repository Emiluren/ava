module Graphics where

import Data.Word (Word8)
import Foreign.C.Types (CDouble)
import Foreign.Ptr (Ptr)
import Linear (V2(..), V4(..))
import qualified SDL

import qualified ChipmunkTypes as H
import qualified SpriterTypes as Spriter

data Direction = DLeft | DRight deriving (Eq, Show)

data Renderable
    = AnimatedSprite (Ptr Spriter.CEntityInstance) String (V2 CDouble) Direction
    | StaticSprite SDL.Texture (V2 CDouble) CDouble
    | Line (V4 Word8) (V2 CDouble) (V2 CDouble)
    | Shape (Ptr H.Shape) H.ShapeType
