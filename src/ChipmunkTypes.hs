{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module ChipmunkTypes where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector.Storable as V
import Foreign.C.Types (CDouble, CUInt)
import Foreign.Storable
import Foreign.Ptr (FunPtr)
import qualified Language.C.Inline as C
import Language.C.Inline.Context (ctxTypesTable)
import Language.C.Types (TypeSpecifier(..))
import qualified Language.Haskell.TH as TH

type CpFloat = CDouble
type CpCollisionType = CUInt

data Vector = Vector { cpVecX :: CpFloat, cpVecY :: CpFloat }
instance Num Vector where
    (Vector x1 y1) + (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)
    (Vector x1 y1) * (Vector x2 y2) = Vector (x1 * x2) (y1 * y2)
    abs (Vector x y) = Vector (abs x) (abs y)
    signum (Vector x y) = Vector (signum x) (signum y)
    fromInteger x = Vector (fromInteger x) (fromInteger x)
    negate (Vector x y) = Vector (-x) (-y)

instance Storable Vector where
    sizeOf _ = 2 * sizeOf (undefined :: CDouble)
    alignment _ = 8
    peek ptr = Vector
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr 8
    poke ptr (Vector x y) =
        pokeByteOff ptr 0 x
        >> pokeByteOff ptr 8 y

scale :: Vector -> CpFloat -> Vector
scale (Vector x y) s = Vector (x * s) (y * s)

data Space
data Body
data Shape

data ShapeType
    = LineSegment Vector Vector CpFloat
    | Circle CpFloat Vector
    | Polygon (V.Vector Vector) CpFloat

type BeginHandler = IO Bool
type PreSolveHandler = IO Bool
type PostSolveHandler = IO ()
type SeparateHandler = IO ()

data Handler = Handler
    { beginHandler :: FunPtr BeginHandler
    , preSolveHandler :: FunPtr PreSolveHandler
    , postSolveHandler :: FunPtr PostSolveHandler
    , separateHandler :: FunPtr SeparateHandler
    }

cpCtx :: C.Context
cpCtx = mempty
    { ctxTypesTable = chipmunkTypeTable
    }

chipmunkTypeTable :: Map TypeSpecifier TH.TypeQ
chipmunkTypeTable = Map.fromList
    [ (TypeName "cpSpace", [t| Space |])
    , (TypeName "cpBody", [t| Body |])
    , (TypeName "cpShape", [t| Shape |])
    , (TypeName "cpVect", [t| Vector |])
    ]
