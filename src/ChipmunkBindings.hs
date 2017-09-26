{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module ChipmunkBindings where

import Control.Monad (replicateM)
import Data.Monoid ((<>))
import Data.StateVar (StateVar, makeStateVar)
import Foreign.Marshal.Alloc (alloca, free, malloc)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.Storable (peek, poke)
import qualified Language.C.Inline as C
import System.IO.Unsafe (unsafePerformIO)

import ChipmunkTypes

C.context $ C.baseCtx <> C.vecCtx <> C.funCtx <> cpCtx
C.include "<chipmunk/chipmunk.h>"

zero :: Vector
zero = Vector 0 0

rotate :: Vector -> Vector -> Vector
rotate vec1 vec2 = unsafePerformIO $ rot
    where
        rot = do
            [v1, v2, v3] <- replicateM 3 malloc
            poke v1 vec1
            poke v2 vec2
            [C.exp| void { *$(cpVect* v3) = cpvrotate(*$(cpVect* v1), *$(cpVect* v2)) } |]
            res <- peek v3
            mapM_ free [v1, v2, v3]
            return res

fromAngle :: CpFloat -> Vector
fromAngle a = unsafePerformIO $ alloca fromA
    where
        fromA vec = do
            [C.exp| void { *$(cpVect* vec) = cpvforangle($(double a)) } |]
            peek vec

skipTwo :: (a -> b) -> (a -> c -> d -> b)
skipTwo f = (\x _ _ -> f x)

makeBeginHandler :: BeginHandler -> IO (FunPtr BeginHandlerFun)
makeBeginHandler = $(C.mkFunPtr [t| BeginHandlerFun |]) . skipTwo

makePreSolveHandler :: PreSolveHandler -> IO (FunPtr PreSolveHandlerFun)
makePreSolveHandler = $(C.mkFunPtr [t| PreSolveHandlerFun |]) . skipTwo

makePostSolveHandler :: PostSolveHandler -> IO (FunPtr PostSolveHandlerFun)
makePostSolveHandler = $(C.mkFunPtr [t| PostSolveHandlerFun |]) . skipTwo

makeSeparateHandler :: SeparateHandler -> IO (FunPtr SeparateHandlerFun)
makeSeparateHandler = $(C.mkFunPtr [t| SeparateHandlerFun |]) . skipTwo

newSpace :: IO (Ptr Space)
newSpace = [C.exp| cpSpace* { cpSpaceNew() } |]

freeSpace :: Ptr Space -> IO ()
freeSpace space = [C.exp| void { cpSpaceFree($(cpSpace* space)) } |]

step :: Ptr Space -> CpFloat -> IO ()
step space dt = [C.exp| void { cpSpaceStep($(cpSpace* space), $(double dt)) } |]

addCollisionHandler :: Ptr Space -> CpCollisionType -> CpCollisionType -> Handler -> IO ()
addCollisionHandler space colType1 colType2 (Handler begh preh posth seph) =
    [C.block| void {
                cpCollisionBeginFunc beginFunc = $(cpBool (*begh)(cpArbiter*, cpSpace*, void*));
                cpCollisionPreSolveFunc preSolveFunc = $(cpBool (*preh)(cpArbiter*, cpSpace*, void*));
                cpCollisionPostSolveFunc postSolveFunc = $(void (*posth)(cpArbiter*, cpSpace*, void*));
                cpCollisionSeparateFunc separateFunc = $(void (*seph)(cpArbiter*, cpSpace*, void*));

                cpCollisionHandler* handler = cpSpaceAddCollisionHandler($(cpSpace* space),
                                                                         $(unsigned int colType1),
                                                                         $(unsigned int colType2));

                if (beginFunc != NULL) handler->beginFunc = beginFunc;
                if (preSolveFunc != NULL) handler->preSolveFunc = preSolveFunc;
                if (postSolveFunc != NULL) handler->postSolveFunc = postSolveFunc;
                if (separateFunc != NULL) handler->separateFunc = separateFunc;
            }
        |]

gravity :: Ptr Space -> StateVar Vector
gravity space = makeStateVar getter setter
    where
        getter =
            let getVec vec = do
                    [C.exp| void { *$(cpVect* vec) = cpSpaceGetGravity($(cpSpace* space)) } |]
                    peek vec
            in alloca getVec
        setter v =
            let setVec vec = do
                    poke vec v
                    [C.exp| void { cpSpaceSetGravity($(cpSpace* space), *$(cpVect* vec)) } |]
            in alloca setVec

newShape :: Ptr Body -> ShapeType -> IO (Ptr Shape)
newShape body (LineSegment a b radius) =
    let makeShape v1 v2 = do
            poke v1 a
            poke v2 b
            [C.exp| cpShape* { cpSegmentShapeNew($(cpBody* body),
                                                 *$(cpVect* v1),
                                                 *$(cpVect* v2),
                                                 $(double radius)) } |]
    in alloca $ alloca . makeShape
newShape body (Circle radius offset) =
    let makeShape vec = do
            poke vec offset
            [C.exp| cpShape* { cpCircleShapeNew($(cpBody* body),
                                                $(double radius),
                                                *$(cpVect* vec)) } |]
    in alloca makeShape
newShape body (Polygon vect radius) =
    [C.exp| cpShape* { cpPolyShapeNewRaw($(cpBody* body),
                                         $vec-len:vect,
                                         $vec-ptr:(cpVect* vect),
                                         $(double radius)) } |]

friction :: Ptr Shape -> StateVar CpFloat
friction shape = makeStateVar getter setter
    where
        getter = [C.exp| double { cpShapeGetFriction($(cpShape* shape))} |]
        setter v = [C.exp| void { cpShapeSetFriction($(cpShape* shape), $(double v))} |]

elasticity :: Ptr Shape -> StateVar CpFloat
elasticity shape = makeStateVar getter setter
    where
        getter = [C.exp| double { cpShapeGetElasticity($(cpShape* shape))} |]
        setter v = [C.exp| void { cpShapeSetElasticity($(cpShape* shape), $(double v))} |]

collisionType :: Ptr Shape -> StateVar CpCollisionType
collisionType shape = makeStateVar getter setter
    where
        getter = [C.exp| unsigned int { cpShapeGetCollisionType($(cpShape* shape)) } |]
        setter v = [C.exp| void { cpShapeSetCollisionType($(cpShape* shape), $(unsigned int v)) } |]

surfaceVel :: Ptr Shape -> StateVar Vector
surfaceVel shape = makeStateVar getter setter
    where
        getter =
            let getVec vec = do
                    [C.exp| void { *$(cpVect* vec) = cpShapeGetSurfaceVelocity($(cpShape* shape)) } |]
                    peek vec
            in alloca getVec
        setter v =
            let setVec vec = do
                    poke vec v
                    [C.exp| void { cpShapeSetSurfaceVelocity($(cpShape* shape), *$(cpVect* vec)) } |]
            in alloca setVec

shapeBody :: Ptr Shape -> StateVar (Ptr Body)
shapeBody shape = makeStateVar getter setter
    where
        getter = [C.exp| cpBody* { cpShapeGetBody($(cpShape* shape)) } |]
        setter b = [C.exp| void { cpShapeSetBody($(cpShape* shape), $(cpBody* b)) } |]

newBody :: CpFloat -> CpFloat -> IO (Ptr Body)
newBody m i = [C.exp| cpBody* { cpBodyNew($(double m), $(double i)) } |]

freeBody :: Ptr Body -> IO ()
freeBody body = [C.exp| void { cpBodyFree($(cpBody* body)) } |]

position :: Ptr Body -> StateVar Vector
position body = makeStateVar getter setter
    where
        getter =
            let getVec vec = do
                    [C.exp| void { *$(cpVect* vec) = cpBodyGetPosition($(cpBody* body)) } |]
                    peek vec
            in alloca getVec
        setter v =
            let setVec vec = do
                    poke vec v
                    [C.exp| void { cpBodySetPosition($(cpBody* body), *$(cpVect* vec)) } |]
            in alloca setVec

force :: Ptr Body -> StateVar Vector
force body = makeStateVar getter setter
    where
        getter =
            let getVec vec = do
                    [C.exp| void { *$(cpVect* vec) = cpBodyGetForce($(cpBody* body)) } |]
                    peek vec
            in alloca getVec
        setter v =
            let setVec vec = do
                    poke vec v
                    [C.exp| void { cpBodySetForce($(cpBody* body), *$(cpVect* vec)) } |]
            in alloca setVec

angle :: Ptr Body -> StateVar CpFloat
angle body = makeStateVar getter setter
    where
        getter = [C.exp| double { cpBodyGetAngle($(cpBody* body))} |]
        setter v = [C.exp| void { cpBodySetAngle($(cpBody* body), $(double v))} |]

spaceGetStaticBody :: Ptr Space -> IO (Ptr Body)
spaceGetStaticBody space = [C.exp| cpBody* { cpSpaceGetStaticBody($(cpSpace* space)) } |]

spaceAddShape :: Ptr Space -> Ptr Shape -> IO ()
spaceAddShape space shape = [C.exp| void { cpSpaceAddShape($(cpSpace* space), $(cpShape* shape))} |]

spaceAddBody :: Ptr Space -> Ptr Body -> IO ()
spaceAddBody space body = [C.exp| void { cpSpaceAddBody($(cpSpace* space), $(cpBody* body)) } |]

applyImpulse :: Ptr Body -> Vector -> Vector -> IO ()
applyImpulse body impulse point =
    let makeShape imp p = do
            poke imp impulse
            poke p point
            [C.exp| void { cpBodyApplyImpulseAtLocalPoint($(cpBody* body),
                                                          *$(cpVect* imp),
                                                          *$(cpVect* p)) } |]
    in alloca $ alloca . makeShape

momentForCircle :: CpFloat -> (CpFloat, CpFloat) -> Vector -> CpFloat
momentForCircle m (r1, r2) offset = unsafePerformIO $ alloca mfc
    where mfc vec = do
              poke vec offset
              [C.exp| double { cpMomentForCircle($(double m),
                                                 $(double r1),
                                                 $(double r2),
                                                 *$(cpVect* vec)) } |]
