{-# LANGUAGE DeriveGeneric #-}
module ChipmunkTypes
    ( CpFloat
    , CpCollisionType
    , CpGroup
    , CpBitmask
    , Vector(..)
    , SegmentQueryInfo(..)
    , ShapeFilter(..)
    , Space
    , Body
    , Shape
    , Arbiter
    , ShapeType(..)

    , BeginHandler
    , PreSolveHandler
    , PostSolveHandler
    , SeparateHandler
    , BodyArbiterIterator
    , BeginHandlerFun
    , PreSolveHandlerFun
    , PostSolveHandlerFun
    , SeparateHandlerFun
    , BodyArbiterIteratorFun
    , Handler(..)

    , scale
    , len
    , toV2
    , fromV2
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.Vector.Storable as V
import Foreign.C.Types (CDouble, CUInt)
import Foreign.Storable
import Foreign.Ptr (Ptr, FunPtr)
import GHC.Generics
import Linear (V2(..))

import Instances ()

type CpFloat = CDouble
type CpCollisionType = CUInt
type CpGroup = CUInt
type CpBitmask = CUInt

data Vector = Vector { cpVecX :: CpFloat, cpVecY :: CpFloat } deriving (Eq, Show, Generic)
instance Num Vector where
    (Vector x1 y1) + (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)
    (Vector x1 y1) * (Vector x2 y2) = Vector (x1 * x2) (y1 * y2)
    abs (Vector x y) = Vector (abs x) (abs y)
    signum (Vector x y) = Vector (signum x) (signum y)
    fromInteger x = Vector (fromInteger x) (fromInteger x)
    negate (Vector x y) = Vector (-x) (-y)

toV2 :: Vector -> V2 CDouble
toV2 (Vector x y) = V2 x y

fromV2 :: V2 CDouble -> Vector
fromV2 (V2 x y) = Vector x y

len :: Vector -> CpFloat
len (Vector dx dy) = sqrt $ dx*dx + dy*dy

scale :: Vector -> CpFloat -> Vector
scale (Vector x y) s = Vector (x * s) (y * s)

instance Storable Vector where
    sizeOf _ = 2 * sizeOf (undefined :: CpFloat)
    alignment _ = 8
    peek ptr = Vector
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr 8
    poke ptr (Vector x y) =
        pokeByteOff ptr 0 x
        >> pokeByteOff ptr 8 y

instance Aeson.ToJSON Vector where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON Vector

data SegmentQueryInfo = SegmentQueryInfo
    { segQueryInfoShape :: Ptr Shape
    , segQueryInfoPoint :: Vector
    , segQueryInfoNormal :: Vector
    , segQueryInfoAlpha :: CpFloat
    } deriving Show

pointOffset, normalOffset, alphaOffset :: Int
pointOffset = sizeOf (undefined :: Ptr Shape)
normalOffset = pointOffset + sizeOf (undefined :: Vector)
alphaOffset = normalOffset + sizeOf (undefined :: Vector)

instance Storable SegmentQueryInfo where
    sizeOf _ = sizeOf (undefined :: Ptr Shape) +
               2 * sizeOf (undefined :: Vector) +
               sizeOf (undefined :: CpFloat)
    alignment _ = alignment (undefined :: Vector)
    peek ptr = SegmentQueryInfo
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr pointOffset
        <*> peekByteOff ptr normalOffset
        <*> peekByteOff ptr alphaOffset
    poke ptr (SegmentQueryInfo shape point normal alpha) =
        pokeByteOff ptr 0 shape
        >> pokeByteOff ptr pointOffset point
        >> pokeByteOff ptr normalOffset normal
        >> pokeByteOff ptr alphaOffset alpha

data ShapeFilter = ShapeFilter
    -- Two objects with the same non-zero group value do not collide.
    -- This is generally used to group objects in a composite object together to disable self collisions.
    { shapeFilterGroup :: CpGroup
    -- A bitmask of user definable categories that this object belongs to.
    -- The category/mask combinations of both objects in a collision must agree for a collision to occur.
    , shapeFilterCategories :: CpBitmask
    -- A bitmask of user definable category types that this object object collides with.
    -- The category/mask combinations of both objects in a collision must agree for a collision to occur.
    , shapeFilterMask :: CpBitmask
    } deriving Show

filterCategoriesOffset, filterMaskOffset :: Int
filterCategoriesOffset = sizeOf (undefined :: CpGroup)
filterMaskOffset = filterMaskOffset + sizeOf (undefined :: CpBitmask)

-- TODO: program crashes with message <<loop>> when this is used
instance Storable ShapeFilter where
    sizeOf _ = sizeOf (undefined :: CpGroup) + 2 * sizeOf (undefined :: CpBitmask)
    alignment _ = alignment (undefined :: CpBitmask)
    peek ptr = ShapeFilter
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr filterCategoriesOffset
        <*> peekByteOff ptr filterMaskOffset
    poke ptr (ShapeFilter group cat mask) =
        pokeByteOff ptr 0 group
        >> pokeByteOff ptr filterCategoriesOffset cat
        >> pokeByteOff ptr filterMaskOffset mask

data Space
data Body
data Shape
data Arbiter

data ShapeType
    = LineSegment Vector Vector CpFloat
    | Circle CpFloat Vector
    | Polygon (V.Vector Vector) CpFloat

type BeginHandler = Ptr Arbiter -> IO Bool
type PreSolveHandler = Ptr Arbiter -> IO Bool
type PostSolveHandler = Ptr Arbiter -> IO ()
type SeparateHandler = Ptr Arbiter -> IO ()
type BodyArbiterIterator = Ptr Body -> Ptr Arbiter -> IO ()

type BeginHandlerFun = Ptr Arbiter -> Ptr Space -> Ptr () -> IO Bool
type PreSolveHandlerFun = Ptr Arbiter -> Ptr Space -> Ptr () -> IO Bool
type PostSolveHandlerFun = Ptr Arbiter -> Ptr Space -> Ptr () -> IO ()
type SeparateHandlerFun = Ptr Arbiter -> Ptr Space -> Ptr () -> IO ()
type BodyArbiterIteratorFun = Ptr Body -> Ptr Arbiter -> Ptr () -> IO ()

data Handler = Handler
    { beginHandler :: FunPtr BeginHandlerFun
    , preSolveHandler :: FunPtr PreSolveHandlerFun
    , postSolveHandler :: FunPtr PostSolveHandlerFun
    , separateHandler :: FunPtr SeparateHandlerFun
    }
