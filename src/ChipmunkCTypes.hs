{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, DeriveGeneric #-}
module ChipmunkCTypes
    ( makeBeginHandler

    , makePreSolveHandler
    , makePostSolveHandler
    , makeSeparateHandler
    , makeArbiterIterator

    , cpCtx
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Foreign.Ptr (FunPtr)
import qualified Language.C.Inline as C
import Language.C.Inline.Context (ctxTypesTable)
import Language.C.Types (TypeSpecifier(..))
import qualified Language.Haskell.TH as TH

import ChipmunkTypes

skipTwo :: (a -> b) -> (a -> c -> d -> b)
skipTwo f x _ _ = f x

skipOne :: (a -> b -> c) -> (a -> b -> d -> c)
skipOne f x y _ = f x y

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
    , (TypeName "cpShapeFilter", [t| ShapeFilter |])
    , (TypeName "cpBool", [t| Bool |])
    , (TypeName "cpArbiter", [t| Arbiter |])
    , (TypeName "cpSegmentQueryInfo", [t| SegmentQueryInfo |])
    , (TypeName "cpPointQueryInfo", [t| PointQueryInfo |])
    ]
