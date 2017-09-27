{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module ChipmunkTypes where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector.Storable as V
import Foreign.C.Types (CDouble, CUInt)
import Foreign.Storable
import Foreign.Ptr (Ptr, FunPtr)
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

skipTwo :: (a -> b) -> (a -> c -> d -> b)
skipTwo f = (\x _ _ -> f x)

skipOne :: (a -> b -> c) -> (a -> b -> d -> c)
skipOne f = (\x y _ -> f x y)

makeBeginHandler :: BeginHandler -> IO (FunPtr BeginHandlerFun)
makeBeginHandler = $(C.mkFunPtr [t| BeginHandlerFun |]) . skipTwo

makePreSolveHandler :: PreSolveHandler -> IO (FunPtr PreSolveHandlerFun)
makePreSolveHandler = $(C.mkFunPtr [t| PreSolveHandlerFun |]) . skipTwo

makePostSolveHandler :: PostSolveHandler -> IO (FunPtr PostSolveHandlerFun)
makePostSolveHandler = $(C.mkFunPtr [t| PostSolveHandlerFun |]) . skipTwo

makeSeparateHandler :: SeparateHandler -> IO (FunPtr SeparateHandlerFun)
makeSeparateHandler = $(C.mkFunPtr [t| SeparateHandlerFun |]) . skipTwo

makeArbiterIterator :: BodyArbiterIterator -> IO (FunPtr BodyArbiterIteratorFun)
makeArbiterIterator = $(C.mkFunPtr [t| BodyArbiterIteratorFun |]) . skipOne

data Handler = Handler
    { beginHandler :: FunPtr BeginHandlerFun
    , preSolveHandler :: FunPtr PreSolveHandlerFun
    , postSolveHandler :: FunPtr PostSolveHandlerFun
    , separateHandler :: FunPtr SeparateHandlerFun
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
    , (TypeName "cpBool", [t| Bool |])
    , (TypeName "cpArbiter", [t| Arbiter |])
    ]
