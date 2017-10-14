{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module ChipmunkBindings
    ( module ChipmunkBindings
    , module ChipmunkTypes
    , module ChipmunkCTypes
    ) where

import Control.Monad (replicateM)
import Data.IORef (newIORef, modifyIORef', readIORef)
import Data.Monoid ((<>))
import Data.StateVar (StateVar, makeStateVar)
import Foreign.Marshal.Alloc (alloca, free, malloc)
import Foreign.Ptr (Ptr, FunPtr, freeHaskellFunPtr)
import Foreign.Storable (peek, poke, Storable)
import qualified Language.C.Inline as C
import System.IO.Unsafe (unsafePerformIO)

import ChipmunkTypes
import ChipmunkCTypes

C.context $ C.baseCtx <> C.vecCtx <> C.funCtx <> cpCtx
C.include "<chipmunk/chipmunk.h>"

zero :: Vector
zero = Vector 0 0

noGroup :: CpGroup
{-# NOINLINE noGroup #-}
noGroup = unsafePerformIO [C.exp| unsigned int { CP_NO_GROUP } |]

allCategories :: CpBitmask
{-# NOINLINE allCategories #-}
allCategories = unsafePerformIO [C.exp| unsigned int { CP_ALL_CATEGORIES } |]

storableGetterSetter :: Storable a => (Ptr a -> IO ()) -> (Ptr a -> IO ()) -> StateVar a
storableGetterSetter getAction setAction = makeStateVar getter setter where
    getter = alloca (\ptr -> getAction ptr >> peek ptr)
    setter x = alloca (\ptr -> poke ptr x >> setAction ptr)

rotate :: Vector -> Vector -> Vector
rotate vec1 vec2 = unsafePerformIO rot where
    rot = do
        [v1, v2, v3] <- replicateM 3 malloc
        poke v1 vec1
        poke v2 vec2
        [C.exp| void { *$(cpVect* v3) = cpvrotate(*$(cpVect* v1), *$(cpVect* v2)) } |]
        res <- peek v3
        mapM_ free [v1, v2, v3]
        return res

fromAngle :: CpFloat -> Vector
fromAngle a = unsafePerformIO $ alloca fromA where
    fromA vec = do
        [C.exp| void { *$(cpVect* vec) = cpvforangle($(double a)) } |]
        peek vec

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
    } |]

gravity :: Ptr Space -> StateVar Vector
gravity space = storableGetterSetter getter setter where
    getter vec = [C.exp| void { *$(cpVect* vec) = cpSpaceGetGravity($(cpSpace* space)) } |]
    setter vec = [C.exp| void { cpSpaceSetGravity($(cpSpace* space), *$(cpVect* vec)) } |]

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
friction shape = makeStateVar getter setter where
    getter = [C.exp| double { cpShapeGetFriction($(cpShape* shape))} |]
    setter v = [C.exp| void { cpShapeSetFriction($(cpShape* shape), $(double v))} |]

elasticity :: Ptr Shape -> StateVar CpFloat
elasticity shape = makeStateVar getter setter where
    getter = [C.exp| double { cpShapeGetElasticity($(cpShape* shape))} |]
    setter v = [C.exp| void { cpShapeSetElasticity($(cpShape* shape), $(double v))} |]

collisionType :: Ptr Shape -> StateVar CpCollisionType
collisionType shape = makeStateVar getter setter where
    getter = [C.exp| unsigned int { cpShapeGetCollisionType($(cpShape* shape)) } |]
    setter v = [C.exp| void { cpShapeSetCollisionType($(cpShape* shape), $(unsigned int v)) } |]

surfaceVel :: Ptr Shape -> StateVar Vector
surfaceVel shape = storableGetterSetter getter setter where
    getter vec = [C.exp| void { *$(cpVect* vec) = cpShapeGetSurfaceVelocity($(cpShape* shape)) } |]
    setter vec = [C.exp| void { cpShapeSetSurfaceVelocity($(cpShape* shape), *$(cpVect* vec)) } |]

shapeBody :: Ptr Shape -> StateVar (Ptr Body)
shapeBody shape = makeStateVar getter setter where
    getter = [C.exp| cpBody* { cpShapeGetBody($(cpShape* shape)) } |]
    setter b = [C.exp| void { cpShapeSetBody($(cpShape* shape), $(cpBody* b)) } |]


-- TODO: could be made much simpler with storable instance but that crashes with <<loop>>
shapeFilter :: Ptr Shape -> StateVar ShapeFilter
shapeFilter shape = makeStateVar getter setter where
    getter = do
        groupPtr <- malloc
        categoriesPtr <- malloc
        maskPtr <- malloc
        [C.block| void {
            cpShapeFilter filter = cpShapeGetFilter($(cpShape* shape));
            *$(unsigned int* groupPtr) = filter.group;
            *$(unsigned int* categoriesPtr) = filter.group;
            *$(unsigned int* maskPtr) = filter.group;
        } |]
        group <- peek groupPtr
        categories <- peek categoriesPtr
        mask <- peek maskPtr
        free groupPtr
        free categoriesPtr
        free maskPtr
        return $ ShapeFilter group categories mask
    setter (ShapeFilter group categories mask) =
        [C.block| void {
            cpShapeFilter filter;
            filter.group = $(unsigned int group);
            filter.categories = $(unsigned int categories);
            filter.mask = $(unsigned int mask);
            cpShapeSetFilter($(cpShape* shape), filter);
        } |]


newBody :: CpFloat -> CpFloat -> IO (Ptr Body)
newBody m i = [C.exp| cpBody* { cpBodyNew($(double m), $(double i)) } |]

freeBody :: Ptr Body -> IO ()
freeBody body = [C.exp| void { cpBodyFree($(cpBody* body)) } |]

position :: Ptr Body -> StateVar Vector
position body = storableGetterSetter getter setter where
    getter vec = [C.exp| void { *$(cpVect* vec) = cpBodyGetPosition($(cpBody* body)) } |]
    setter vec = [C.exp| void { cpBodySetPosition($(cpBody* body), *$(cpVect* vec)) } |]

velocity :: Ptr Body -> StateVar Vector
velocity body = storableGetterSetter getter setter where
    getter vec = [C.exp| void { *$(cpVect* vec) = cpBodyGetVelocity($(cpBody* body)) } |]
    setter vec = [C.exp| void { cpBodySetVelocity($(cpBody* body), *$(cpVect* vec)) } |]

force :: Ptr Body -> StateVar Vector
force body = storableGetterSetter getter setter where
    getter vec = [C.exp| void { *$(cpVect* vec) = cpBodyGetForce($(cpBody* body)) } |]
    setter vec = [C.exp| void { cpBodySetForce($(cpBody* body), *$(cpVect* vec)) } |]

angle :: Ptr Body -> StateVar CpFloat
angle body = makeStateVar getter setter where
    getter = [C.exp| double { cpBodyGetAngle($(cpBody* body))} |]
    setter v = [C.exp| void { cpBodySetAngle($(cpBody* body), $(double v))} |]

bodyEachArbiter :: Ptr Body -> FunPtr BodyArbiterIteratorFun -> IO ()
bodyEachArbiter body func =
    [C.exp| void { cpBodyEachArbiter($(cpBody* body),
                                     $(void (*func)(cpBody*, cpArbiter*, void*)), NULL) } |]

bodyEachArbiterList :: Ptr Body -> IO [Ptr Arbiter]
bodyEachArbiterList body = do
    arbiterRef <- newIORef []
    arbiterCallback <- makeArbiterIterator (\_ arb -> modifyIORef' arbiterRef (arb:))
    bodyEachArbiter body arbiterCallback
    arbiters <- readIORef arbiterRef
    freeHaskellFunPtr arbiterCallback
    return arbiters

spaceGetStaticBody :: Ptr Space -> IO (Ptr Body)
spaceGetStaticBody space = [C.exp| cpBody* { cpSpaceGetStaticBody($(cpSpace* space)) } |]

spaceAddShape :: Ptr Space -> Ptr Shape -> IO ()
spaceAddShape space shape = [C.exp| void { cpSpaceAddShape($(cpSpace* space), $(cpShape* shape))} |]

spaceAddBody :: Ptr Space -> Ptr Body -> IO ()
spaceAddBody space body = [C.exp| void { cpSpaceAddBody($(cpSpace* space), $(cpBody* body)) } |]

spaceSegmentQueryFirst :: Ptr Space -> Vector -> Vector -> CpFloat -> ShapeFilter -> IO SegmentQueryInfo
spaceSegmentQueryFirst space start end radius (ShapeFilter group categories mask) = do
    startPtr <- malloc
    endPtr <- malloc
    infoPtr <- malloc

    poke startPtr start
    poke endPtr end
    [C.block| void {
            cpShapeFilter filter;
            filter.group = $(unsigned int group);
            filter.categories = $(unsigned int categories);
            filter.mask = $(unsigned int mask);
            cpSpaceSegmentQueryFirst($(cpSpace* space),
                                     *$(cpVect* startPtr),
                                     *$(cpVect* endPtr),
                                     $(double radius),
                                     filter,
                                     $(cpSegmentQueryInfo* infoPtr));
    } |]

    info <- peek infoPtr

    free startPtr
    free endPtr
    free infoPtr

    return info

spacePointQueryNearest :: Ptr Space -> Vector -> CpFloat -> ShapeFilter -> IO PointQueryInfo
spacePointQueryNearest space point maxDistance (ShapeFilter group categories mask) = do
    pointPtr <- malloc
    infoPtr <- malloc

    poke pointPtr point
    [C.block| void {
            cpShapeFilter filter;
            filter.group = $(unsigned int group);
            filter.categories = $(unsigned int categories);
            filter.mask = $(unsigned int mask);
            cpSpacePointQueryNearest($(cpSpace* space),
                                     *$(cpVect* pointPtr),
                                     $(double maxDistance),
                                     filter,
                                     $(cpPointQueryInfo* infoPtr));
    } |]

    info <- peek infoPtr

    free pointPtr
    free infoPtr

    return info

arbiterGetShapes :: Ptr Arbiter -> IO (Ptr Shape, Ptr Shape)
arbiterGetShapes arb = do
    s1Ptr <- malloc
    s2Ptr <- malloc
    [C.exp| void { cpArbiterGetShapes($(cpArbiter* arb), $(cpShape** s1Ptr), $(cpShape** s2Ptr)) } |]
    s1 <- peek s1Ptr
    s2 <- peek s2Ptr
    free s1Ptr
    free s2Ptr
    return (s1, s2)

applyImpulse :: Ptr Body -> Vector -> Vector -> IO ()
applyImpulse body impulse point = do
    imp <- malloc
    p <- malloc
    poke imp impulse
    poke p point
    [C.exp| void { cpBodyApplyImpulseAtLocalPoint($(cpBody* body),
                                                  *$(cpVect* imp),
                                                  *$(cpVect* p)) } |]
    free imp
    free p

momentForCircle :: CpFloat -> (CpFloat, CpFloat) -> Vector -> CpFloat
momentForCircle m (r1, r2) offset = unsafePerformIO $ alloca mfc
    where mfc vec = do
              poke vec offset
              [C.exp| double { cpMomentForCircle($(double m),
                                                 $(double r1),
                                                 $(double r2),
                                                 *$(cpVect* vec)) } |]
