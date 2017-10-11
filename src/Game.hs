{-# LANGUAGE FlexibleInstances, FlexibleContexts, TemplateHaskell, GADTs, MultiParamTypeClasses, RecursiveDo, ScopedTypeVariables #-}
module Game where

import Control.Monad (replicateM_)
import Control.Monad.Identity
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Bool (bool)
import Data.GADT.Compare.TH
import Data.List (zipWith4)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.StateVar (($=), get)
import qualified Data.Time.Clock as Time
import qualified Data.Vector.Storable as V
import Data.Word (Word8)

import Foreign.C.Types (CInt, CUInt, CDouble(..))
import Foreign.Ptr (Ptr, nullPtr)

import Linear (V2(..), V4(..))

import qualified ChipmunkBindings as H
import qualified ChipmunkTypes as H

import Reflex
import Reflex.Time

import SDL (Point(P))
import qualified SDL
import qualified SDL.Raw.Types as SDL (JoystickID)

import qualified SpriterTypes as Spriter
import qualified SpriterBindings as Spriter

shelfStart, shelfEnd :: Num a => (a, a)
shelfStart = (100, 200)
shelfEnd = (300, 320)

padButtonX, padButtonA :: Word8
padButtonA = 0
padButtonX = 2

padXAxis :: Word8
padXAxis = 0

makeHVector :: (CDouble, CDouble) -> H.Vector
makeHVector = uncurry H.Vector

makeSdlPoint :: Num a => (a, a) -> SDL.Point V2 a
makeSdlPoint = SDL.P . uncurry SDL.V2

startV, endV :: H.Vector
(startV, endV) = (makeHVector shelfStart, makeHVector shelfEnd)

startP, endP :: SDL.Point V2 CInt
(startP, endP) = (makeSdlPoint shelfStart, makeSdlPoint shelfEnd)

circleMass, circleRadius :: Num a => a
circleMass = 10
circleRadius = 20

isPress :: SDL.KeyboardEventData -> Bool
isPress event =
    SDL.keyboardEventKeyMotion event == SDL.Pressed

isRelease :: SDL.KeyboardEventData -> Bool
isRelease event =
    SDL.keyboardEventKeyMotion event == SDL.Released

eventKeycode :: SDL.KeyboardEventData -> SDL.Keycode
eventKeycode = SDL.keysymKeycode . SDL.keyboardEventKeysym

isKey :: SDL.Keycode -> SDL.KeyboardEventData -> Bool
isKey keycode = (== keycode) . eventKeycode

isKeyPressed :: SDL.Keycode -> SDL.Event -> Bool
isKeyPressed key event =
    case SDL.eventPayload event of
        SDL.KeyboardEvent eventData ->
            isPress eventData && isKey key eventData
        _ ->
            False

playerSpeed :: CDouble
playerSpeed = 200

controlVx :: Num a => a -> Bool -> Bool -> a
controlVx x True False = -x
controlVx x False True = x
controlVx _ _ _ = 0

data Direction = DLeft | DRight deriving (Eq, Show)

data AiDir = AiLeft | AiStay | AiRight deriving (Eq, Show)

aiDirToDirection :: AiDir -> Maybe Direction
aiDirToDirection AiStay = Nothing
aiDirToDirection AiLeft = Just DLeft
aiDirToDirection AiRight = Just DRight

data Renderable
    = AnimatedSprite (Ptr Spriter.CEntityInstance) String (V2 CDouble) Direction
    | Line (V4 Word8) (V2 CDouble) (V2 CDouble)
    | Shape (Ptr H.Shape) H.ShapeType

data LogicOutput t = LogicOutput
    { cameraCenterPosition :: Behavior t (V2 CDouble)
    --playerSurfaceVel :: Behavior t H.Vector
    --, mummySurfaceVel :: Behavior t H.Vector
    --, playerForce :: Behavior t H.Vector
    , renderCommands :: Behavior t [Renderable]
    , quit :: Event t ()
    }

data EnemyType = Mummy

data ObjectType = Ball

data LevelData = LevelData
    { playerStartPosition :: H.Vector
    , wallEdges :: [(H.Vector, H.Vector)]
    , initEnemies :: [(EnemyType, H.Vector)]
    , extraObjects :: [(ObjectType, H.Vector)]
    }

type CharacterPhysicsRefs = (Ptr H.Shape, Ptr H.Shape)

data LevelLoadedData = LevelLoadedData
    { playerPhysicsRefs :: CharacterPhysicsRefs
    , playerSpriterInstance :: Ptr Spriter.CEntityInstance
    , aiPhysicsRefs :: [(CharacterPhysicsRefs, Ptr Spriter.CEntityInstance)]
    , extraPhysicsRefs :: [(Ptr H.Shape, H.ShapeType)]
    , levelSpace :: Ptr H.Space
    }

data SdlEventTag a where
    ControllerDeviceEvent :: SdlEventTag SDL.ControllerDeviceEventData
    JoyAxisEvent :: SDL.JoystickID -> SdlEventTag SDL.JoyAxisEventData
    JoyButtonEvent :: SDL.JoystickID -> SdlEventTag SDL.JoyButtonEventData
    KeyEvent :: SDL.Keycode -> SdlEventTag SDL.KeyboardEventData
    OtherEvent :: SdlEventTag SDL.Event

deriveGEq ''SdlEventTag
deriveGCompare ''SdlEventTag

makeCharacterBody :: H.CpFloat -> H.CpFloat -> [H.Vector]
makeCharacterBody w h =
    [ H.Vector (-w * 0.5) (-h * 0.2)
    , H.Vector (-w * 0.5) (-h * 0.8)
    , H.Vector (-w * 0.3) (-h)
    , H.Vector (w * 0.3) (-h)
    , H.Vector (w * 0.5) (-h * 0.8)
    , H.Vector (w * 0.5) (-h * 0.2)
    , H.Vector (w * 0.3) (-h * 0.1)
    , H.Vector (-w * 0.3) (-h * 0.1)
    ]

characterWidth, characterHeight, characterMass, characterFeetFriction, characterSide :: H.CpFloat
characterWidth = 10
characterHeight = 30
characterMass = 5
characterFeetFriction = 2
characterSide = characterWidth * 0.7

playerFeetCollisionType :: CUInt
playerFeetCollisionType = 1

characterFeetShapeType, characterBodyShapeType :: H.ShapeType
characterFeetShapeType = H.Circle (characterWidth * 0.3) (H.Vector 0 $ - characterWidth * 0.2)
characterBodyShapeType = H.Polygon (V.fromList $ reverse $ makeCharacterBody characterWidth characterHeight) 0

playerKickCheckR, playerKickCheckL :: (H.Vector, H.Vector)
playerKickCheckR =
    ( H.Vector characterSide (-17)
    , H.Vector (2.2 * characterSide) (-17)
    )
playerKickCheckL =
    ( H.Vector (- characterSide) (-17)
    , H.Vector (- 2.2 * characterSide) (-17)
    )

mummySideCheckUR, mummySideCheckLR, mummySideCheckUL, mummySideCheckLL :: H.Vector
mummySideCheckUR = H.Vector characterSide (-10)
mummySideCheckLR = H.Vector characterSide 10
mummySideCheckUL = H.Vector (-characterSide) (-10)
mummySideCheckLL = H.Vector (-characterSide) 10

makeCharacter :: Ptr H.Space -> IO CharacterPhysicsRefs
makeCharacter space = do
    characterBody <- H.newBody characterMass $ 1/0
    H.spaceAddBody space characterBody
    characterFeetShape <- H.newShape characterBody characterFeetShapeType
    characterBodyShape <- H.newShape characterBody characterBodyShapeType
    H.spaceAddShape space characterFeetShape
    H.spaceAddShape space characterBodyShape
    H.friction characterFeetShape $= characterFeetFriction
    H.friction characterBodyShape $= 0

    return (characterFeetShape, characterBodyShape)

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

groundCheckFilter :: H.ShapeFilter
groundCheckFilter = H.ShapeFilter
    { H.shapeFilterGroup = H.noGroup
    , H.shapeFilterCategories = H.allCategories
    , H.shapeFilterMask = H.allCategories
    }

limit :: (Num a, Ord a, Reflex t) => Behavior t a -> Behavior t a
limit = fmap limf where
    limf x
        | x < -1 = -1
        | x > 1 = 1
        | otherwise = x

timeStep :: Time.NominalDiffTime
timeStep = 1/120

spriterTimeStep :: Time.NominalDiffTime
spriterTimeStep = 1/60

data PhysicsOutput = PhysicsOutput
    { physicsPlayerOnGround :: Bool
    , physicsPlayerPosition :: H.Vector
    , physicsAiPositions :: [H.Vector]
    }

class (MonadIO m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m), PostBuild t m, MonadHold t m, MonadFix m, MonadAdjust t m) => MonadGame t m

instance (MonadGame
             (SpiderTimeline Global)
             (PostBuildT
                 (SpiderTimeline Global)
                 (TriggerEventT
                     (SpiderTimeline Global)
                     (PerformEventT (SpiderTimeline Global) (SpiderHost Global)))))

holdGameMode :: MonadGame t m => m a -> Event t (m a) -> m (Dynamic t a)
holdGameMode mode newMode = do
    (res, newRes) <- runWithReplace mode newMode
    holdDyn res newRes

data CharacterOutput t = CharacterOutput
    { characterSurfaceVelocity :: Behavior t H.Vector
    , characterDirection :: Behavior t Direction
    , characterAnimation :: Behavior t String
    }

queryLineSeg :: Ptr H.Space -> (H.Vector, H.Vector) -> IO H.SegmentQueryInfo
queryLineSeg space (s, e) = H.spaceSegmentQueryFirst space s e 1 groundCheckFilter

aiCharacterNetwork :: forall t m. MonadGame t m =>
    Ptr H.Space ->
    Event t TickInfo ->
    Behavior t H.Vector ->
    m (CharacterOutput t)
aiCharacterNetwork space aiTick pos = do
    let mummySpeed AiRight = H.Vector (- playerSpeed / 6) 0
        mummySpeed AiLeft = H.Vector (playerSpeed / 6) 0
        mummySpeed AiStay = H.zero

        rightSideSegment currentPos =
            ( currentPos + mummySideCheckUR
            , currentPos + mummySideCheckLR
            )
        leftSideSegment currentPos =
            ( currentPos + mummySideCheckUL
            , currentPos + mummySideCheckLL
            )

        doAiCollisionChecks mp = do
            let (pkc1r, pkc2r) = playerKickCheckR
                (pkc1l, pkc2l) = playerKickCheckL
                segsToCheck =
                    [ rightSideSegment mp
                    , leftSideSegment mp
                    , (pkc1r + mp, pkc2r + mp)
                    , (pkc1l + mp, pkc2l + mp)
                    ]

            [cgr, cgl, cwr, cwl] <- forM segsToCheck $ \seg -> do
                segHitInfo <- queryLineSeg space seg
                return $ H.segQueryInfoShape segHitInfo /= nullPtr
            return (cgr, cgl, cwr, cwl)

        runAiLogic (colGroundRight, colGroundLeft, colWallRight, colWallLeft) currentDir =
            let canGoLeft = colGroundLeft && not colWallLeft
                canGoRight = colGroundRight && not colWallRight
            in case currentDir of
                AiLeft
                    | canGoLeft -> Nothing
                    | canGoRight -> Just AiRight
                    | otherwise -> Just AiStay
                AiRight
                    | canGoRight -> Nothing
                    | canGoLeft -> Just AiLeft
                    | otherwise -> Just AiStay
                AiStay
                    | canGoRight -> Just AiRight
                    | canGoLeft -> Just AiLeft
                    | otherwise -> Nothing
    colResults <- performEvent $ liftIO . doAiCollisionChecks <$> pos <@ aiTick

    mummyWalkDirection <- foldDynMaybe runAiLogic AiStay colResults
    mummyDisplayDir <- hold DRight $ fmapMaybe aiDirToDirection $ updated mummyWalkDirection

    let mummySurfaceVelocity = mummySpeed <$> current mummyWalkDirection
        mummyMoving = (\(H.Vector vx _) -> abs vx > 0) <$> mummySurfaceVelocity
        mummyAnimation = (\moving -> if moving then "Walk" else "Idle") <$> mummyMoving

    return CharacterOutput
        { characterSurfaceVelocity = mummySurfaceVelocity
        , characterDirection = mummyDisplayDir
        , characterAnimation = mummyAnimation
        }

samplePhysics :: Ptr H.Body -> [Ptr H.Body] -> IO PhysicsOutput
samplePhysics playerBody characterBodies = do
    arbiters <- H.bodyEachArbiterList playerBody
    playerCollisionWithGround <- forM arbiters $ \arb -> do
        (s1, s2) <- H.arbiterGetShapes arb
        ct1 <- get $ H.collisionType s1
        ct2 <- get $ H.collisionType s2
        return (ct1 == playerFeetCollisionType && ct2 == 0)
    playerPosition <- get $ H.position playerBody
    aiPositions <- mapM (get . H.position) characterBodies
    return PhysicsOutput
        { physicsPlayerOnGround = or playerCollisionWithGround
        , physicsPlayerPosition = playerPosition
        , physicsAiPositions = aiPositions
        }

initLevelNetwork :: forall t m. MonadGame t m =>
    Time.UTCTime ->
    EventSelector t SdlEventTag ->
    Event t Int ->
    Behavior t (SDL.Scancode -> Bool) ->
    Maybe (Behavior t Double) ->
    LevelLoadedData ->
    m (LogicOutput t)
initLevelNetwork startTime sdlEventFan eStepPhysics pressedKeys mAxis levelLoaded = do
    let space = levelSpace levelLoaded
        (playerFeetShape, playerBodyShape) = playerPhysicsRefs levelLoaded
        aiShapesAndSprites = aiPhysicsRefs levelLoaded :: [(CharacterPhysicsRefs, Ptr Spriter.CEntityInstance)]
        aiFeetShapes = fst . fst <$> aiShapesAndSprites
        aiSprites = snd <$> aiShapesAndSprites

    playerBody <- get $ H.shapeBody playerFeetShape
    aiBodies <- mapM (get . H.shapeBody) aiFeetShapes
    aiTick <- tickLossy (1/15) startTime

    let stepPhysics playerSurfaceVel aiSurfaceVels playerForce stepsToRun = liftIO $ do
            H.surfaceVel playerFeetShape $= H.scale playerSurfaceVel (-1)
            H.force playerBody $= playerForce

            forM_ (zip aiFeetShapes aiSurfaceVels) $ \(feetShape, surfVel) ->
                H.surfaceVel feetShape $= surfVel

            replicateM_ stepsToRun $ H.step space $ realToFrac timeStep
            samplePhysics playerBody aiBodies

    initialPhysicsState <- liftIO $ samplePhysics playerBody aiBodies

    let aPressed = ($ SDL.ScancodeA) <$> pressedKeys
        dPressed = ($ SDL.ScancodeD) <$> pressedKeys
        playerKeyMovement = controlVx 1 <$> aPressed <*> dPressed
        playerAxisMovement = CDouble <$> fromMaybe (pure 0) mAxis
        playerMovement = limit $ (+) <$> playerKeyMovement <*> playerAxisMovement
        playerAirForce = (\d -> H.Vector (d * 1000) 0) <$> playerMovement
        playerAcc = H.Vector <$> fmap (* playerSpeed) playerMovement <*> pure 0

    rec
        let playerForce = bool
                <$> playerAirForce <*> pure (H.Vector 0 0) <*> playerOnGround
            playerSurfaceVelocity :: Behavior t H.Vector
            playerSurfaceVelocity = bool
                <$> pure (H.Vector 0 0) <*> playerAcc <*> playerOnGround
            playerOnGround = physicsPlayerOnGround <$> physicsOutput

            aiSurfaceVelocities = sequenceA $ characterSurfaceVelocity <$> aiCharacters
            aiPositions = physicsAiPositions <$> physicsOutput :: Behavior t [H.Vector]

        -- TODO: Will only work for a single character and will crash if there are none :P
        -- fix with a normal map and holdGame maybe
        aiCharacters <- mapM (aiCharacterNetwork space aiTick) [head <$> aiPositions]

        eSteppedPhysics <- performEvent $ stepPhysics
            <$> playerSurfaceVelocity <*> aiSurfaceVelocities <*> playerForce <@> eStepPhysics
        physicsOutput <- hold initialPhysicsState eSteppedPhysics

    let playerPos = physicsPlayerPosition <$> physicsOutput

    let pressEvent kc = ffilter isPress $ select sdlEventFan (KeyEvent kc)
        eWPressed = pressEvent SDL.KeycodeW
        eKPressed = pressEvent SDL.KeycodeK
        eF1Pressed = pressEvent SDL.KeycodeF1
        eF2Pressed = pressEvent SDL.KeycodeF2
        eF3Pressed = pressEvent SDL.KeycodeF3
        eAPressed = pressEvent SDL.KeycodeA
        eDPressed = pressEvent SDL.KeycodeD
        padButtonPress = select sdlEventFan (JoyButtonEvent 0)
        padAxisMove = select sdlEventFan (JoyAxisEvent 0)
        padFilterButtonPress b = ffilter
            (\(SDL.JoyButtonEventData _ b' s) -> b == b' && s == 1) padButtonPress
        ePadAPressed = padFilterButtonPress padButtonA
        ePadXPressed = padFilterButtonPress padButtonX
        ePadNotCenter = ffilter
            (\(SDL.JoyAxisEventData _ a v) ->
                 a == padXAxis &&
                abs (fromIntegral v / 32768 :: Float) > 0.15)
            padAxisMove
        ePadChangeDir = (\(SDL.JoyAxisEventData _ _ v) -> if v > 0 then DRight else DLeft)
            <$> ePadNotCenter

    debugRendering <- current <$> toggle True eF1Pressed
    characterDbg <- current <$> toggle False (gate debugRendering eF2Pressed)
    colCheckDbg <- current <$> toggle False (gate debugRendering eF3Pressed)
    playerDir <- hold DRight $ leftmost
        [ DLeft <$ eAPressed
        , DRight <$ eDPressed
        , ePadChangeDir
        ]

    let ePlayerWantsToJump = mconcat [() <$ eWPressed, () <$ ePadAPressed]
        ePlayerWantsToKick = mconcat [() <$ eKPressed, () <$ ePadXPressed]
        ePlayerKick = ePlayerWantsToKick -- TODO: Add check for current player state

        jumpEvent = (-1000) <$ gate playerOnGround ePlayerWantsToJump

        playerMoving = (\(H.Vector vx _) -> abs vx > 0) <$> playerSurfaceVelocity

        jump imp = liftIO $ H.applyImpulse playerBody (H.Vector 0 imp) H.zero

    let kickDuration = 0.6
        kickDelay = 0.4
        pickAnimation moving onGround lastKickTime currentTime =
            let runOrIdle
                    | not onGround = "Falling"
                    | moving = "Run"
                    | otherwise = "Idle"
            in case lastKickTime of
                Just t -> if currentTime - t < kickDuration then "Kick" else runOrIdle
                Nothing -> runOrIdle
        clock = clockLossy (1/60) startTime
        playerKickEffect :: H.Vector -> Direction -> Performable m ()
        playerKickEffect playerP playerD = do
            let (pkc1, pkc2) = case playerD of
                    DRight -> playerKickCheckR
                    DLeft -> playerKickCheckL
                playerKickVec = case playerD of
                    DRight -> H.Vector 1000 (-1000)
                    DLeft -> H.Vector (-1000) (-1000)

            hitShapeInfo <- liftIO $ queryLineSeg space (playerP + pkc1, playerP + pkc2)

            let hitShape = H.segQueryInfoShape hitShapeInfo
            when (hitShape /= nullPtr) $ do
                hitBody <- get $ H.shapeBody hitShape
                liftIO $ H.applyImpulse hitBody playerKickVec H.zero
                when (hitBody `elem` aiBodies) $ liftIO $ putStrLn "Kicked mummy"

    tickInfo <- current <$> clock
    let timeSinceStart = flip Time.diffUTCTime startTime . _tickInfo_lastUTC <$> tickInfo

    latestPlayerKick <- hold Nothing $ Just <$> timeSinceStart <@ ePlayerKick
    eDelayedPlayerKick <- delay kickDelay ePlayerKick

    let playerEntityInstance = playerSpriterInstance levelLoaded

    performEvent_ $ liftIO (Spriter.setEntityInstanceCurrentTime playerEntityInstance 0) <$ ePlayerKick
    performEvent_ $ playerKickEffect <$> playerPos <*> playerDir <@ eDelayedPlayerKick

    performEvent_ $ jump <$> jumpEvent

    let playerAnimation =
            pickAnimation <$> playerMoving <*> playerOnGround <*> latestPlayerKick <*> timeSinceStart
        toV2 (H.Vector x y) = V2 x y

    eSpriterTick <- tickLossy spriterTimeStep startTime
    eSpriterDiffs <- mapAccum_ (\oldTime newTime -> (newTime, Time.diffUTCTime newTime oldTime))
        startTime $ _tickInfo_lastUTC <$> eSpriterTick
    performEvent_ $ liftIO . Spriter.setEntityInstanceTimeElapsed playerEntityInstance
        . realToFrac . (*1000) <$> eSpriterDiffs
    forM_ aiSprites $ \aiEntityInstance ->
        performEvent_ $ liftIO . Spriter.setEntityInstanceTimeElapsed aiEntityInstance
            . realToFrac . (*1000) <$> eSpriterDiffs


    let aiAnimations = do
            aiCurrentPositions <- aiPositions
            aiCurrentAnimations <- sequenceA $ characterAnimation <$> aiCharacters
            aiCurrentDirections <- sequenceA $ characterDirection <$> aiCharacters
            return $ zipWith4 AnimatedSprite
                aiSprites aiCurrentAnimations (toV2 <$> aiCurrentPositions) aiCurrentDirections

        renderCharacters = sequenceA
            [ AnimatedSprite playerEntityInstance
                <$> playerAnimation <*> fmap toV2 playerPos <*> playerDir
            ]
            <> aiAnimations

        renderColDebug = do
            currentAiPositions <- aiPositions
            playerP <- playerPos
            let aiRenderShapes =
                    concatMap (\((feetShape, bodyShape), _) ->
                                   [ Shape feetShape characterFeetShapeType
                                   , Shape bodyShape characterBodyShapeType
                                   ]) (aiPhysicsRefs levelLoaded)
                renderShapes =
                    uncurry Shape <$> extraPhysicsRefs levelLoaded
                renderCharacterColliders =
                    [ Shape playerFeetShape characterFeetShapeType
                    , Shape playerBodyShape characterBodyShapeType
                    ] ++ aiRenderShapes
                chrLine chrP (p1, p2) =
                    Line (V4 255 0 0 255) (toV2 $ p1 + chrP) (toV2 $ p2 + chrP)
                renderSideChecks =
                    [ chrLine playerP playerKickCheckR
                    , chrLine playerP playerKickCheckL
                    ]
                    ++ concatMap (\mummyP ->
                                      [ chrLine mummyP (mummySideCheckUL, mummySideCheckLL)
                                      , chrLine mummyP (mummySideCheckUR, mummySideCheckLR)
                                      , chrLine mummyP playerKickCheckR
                                      , chrLine mummyP playerKickCheckL
                                      ]
                                 ) currentAiPositions

            debugRender <- debugRendering
            dbgRenderChrs <- characterDbg
            dbgRenderCol <- colCheckDbg
            case (debugRender, dbgRenderChrs, dbgRenderCol) of
                (False, _, _) -> return []
                (True, False, False) -> return renderShapes
                (True, True, False) -> return $ renderShapes ++ renderCharacterColliders
                (True, False, True) -> return $ renderShapes ++ renderSideChecks
                (True, True, True) -> return $ renderShapes ++ renderCharacterColliders ++ renderSideChecks

    return LogicOutput
        { cameraCenterPosition = toV2 . physicsPlayerPosition <$> physicsOutput
        , renderCommands = renderCharacters <> renderColDebug
        , quit = () <$ pressEvent SDL.KeycodeQ
        }
