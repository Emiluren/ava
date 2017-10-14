{-# LANGUAGE FlexibleInstances, FlexibleContexts, TemplateHaskell, GADTs, MultiParamTypeClasses, RecursiveDo, ScopedTypeVariables #-}
module Game where

import Control.Monad (replicateM_)
import Control.Monad.Identity
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Bits (bit, complement)
import Data.Bool (bool)
import Data.GADT.Compare.TH
import Data.Int (Int16)
import Data.Monoid ((<>))
import Data.StateVar (($=), get)
import qualified Data.Time.Clock as Time
import qualified Data.Vector.Storable as V
import Data.Word (Word8)

import Foreign.C.Types (CInt, CUInt, CDouble(..))
import Foreign.Ptr (Ptr, nullPtr)

import Linear (V2(..), V4(..), (*^))

import Reflex
import Reflex.Time

import SDL (Point(P))
import qualified SDL
import qualified SDL.Raw.Types as SDL (JoystickID)
import qualified SDL.Image

import System.Random (newStdGen, randomRs)

import qualified ChipmunkBindings as H

import Level

import qualified SpriterTypes as Spriter
import qualified SpriterBindings as Spriter

-- TODO: Move to chipmunk bindings
toV2 :: H.Vector -> V2 CDouble
toV2 (H.Vector x y) = V2 x y

padButtonX, padButtonA, padButtonY :: Num a => a
padButtonA = 0
padButtonX = 2
padButtonY = 3

padXAxis :: Word8
padXAxis = 0

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
    | StaticSprite SDL.Texture (V2 CDouble) CDouble
    | Line (V4 Word8) (V2 CDouble) (V2 CDouble)
    | Shape (Ptr H.Shape) H.ShapeType

data LogicOutput t = LogicOutput
    { cameraCenterPosition :: Behavior t (V2 CDouble)
    , renderCommands :: Behavior t [Renderable]
    , quit :: Event t ()
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

playerFeetCollisionType, mummyCollisionType, wallCollisionType :: CUInt
playerFeetCollisionType = 1
mummyCollisionType = 2
wallCollisionType = 3

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

groundCheckFilter :: H.ShapeFilter
groundCheckFilter = H.ShapeFilter
    { H.shapeFilterGroup = H.noGroup
    , H.shapeFilterCategories = H.allCategories
    , H.shapeFilterMask = H.allCategories
    }

visibleByAiBit :: H.CpBitmask
visibleByAiBit = bit 31

aiVisibleFilter :: H.ShapeFilter
aiVisibleFilter = H.ShapeFilter
    { H.shapeFilterGroup = H.noGroup
    , H.shapeFilterMask = visibleByAiBit
    , H.shapeFilterCategories = visibleByAiBit
    }

aiInvisibleFilter :: H.ShapeFilter
aiInvisibleFilter = H.ShapeFilter
    { H.shapeFilterGroup = H.noGroup
    , H.shapeFilterMask = complement visibleByAiBit
    , H.shapeFilterCategories = complement visibleByAiBit
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
    , characterForce :: Behavior t H.Vector
    , characterDirection :: Behavior t Direction
    , characterAnimation :: Behavior t String
    , characterRendering :: Behavior t [ Renderable ]
    }

queryLineSeg :: Ptr H.Space -> (H.Vector, H.Vector) -> H.ShapeFilter -> IO H.SegmentQueryInfo
queryLineSeg space (s, e) = H.spaceSegmentQueryFirst space s e 1

data DebugRenderSettings = DebugRenderSettings
    { debugRenderAtAll :: Bool
    , debugRenderCharacterShapes :: Bool
    , debugRenderCharacterChecks :: Bool
    }

extend :: CDouble -> (H.Vector, H.Vector) -> (H.Vector, H.Vector)
extend amount (start, end) =
    let H.Vector x1 y1 = start
        H.Vector x2 y2 = end
        dx = x2 - x1
        dy = y2 - y1
        len = H.len (H.Vector dx dy)
        extension = H.Vector (dx / len * amount) (dy / len * amount)
    in (start, end + extension)

debugRenderCharacter :: Reflex t => Behavior t DebugRenderSettings -> [Renderable] -> [Renderable] -> Behavior t [Renderable]
debugRenderCharacter debugSettings renderFeetAndBody renderSideChecks = do
    debugRender <- debugRenderAtAll <$> debugSettings
    if not debugRender then
        return []
    else do
        dbgRenderChrs <- debugRenderCharacterShapes <$> debugSettings
        dbgRenderCol <- debugRenderCharacterChecks <$> debugSettings
        return $ case (dbgRenderChrs, dbgRenderCol) of
             (False, False) -> []
             (True, False) -> renderFeetAndBody
             (False, True) -> renderSideChecks
             (True, True) -> renderFeetAndBody <> renderSideChecks

attackEffect :: Ptr H.Space -> H.Vector -> Direction -> H.CpFloat -> (Ptr H.Shape -> Ptr H.Body -> IO Bool) -> IO Bool
attackEffect space pos dir imp onHit = do
    let (pkc1, pkc2) = case dir of
            DRight -> playerKickCheckR
            DLeft -> playerKickCheckL
        attackVec = case dir of
            DRight -> H.Vector imp (-imp)
            DLeft -> H.Vector (-imp) (-imp)

    hitShapeInfo <- queryLineSeg space (pos + pkc1, pos + pkc2) groundCheckFilter

    let hitShape = H.segQueryInfoShape hitShapeInfo
    if hitShape /= nullPtr then do
        hitBody <- get $ H.shapeBody hitShape
        H.applyImpulse hitBody attackVec H.zero
        onHit hitShape hitBody
    else
        return False

aiCharacterNetwork :: forall t m. MonadGame t m =>
    Time.UTCTime ->
    Ptr H.Space ->
    Ptr H.Body ->
    Event t TickInfo ->
    Behavior t H.Vector ->
    Behavior t DebugRenderSettings ->
    Behavior t H.Vector ->
    (Ptr H.Shape, Ptr H.Shape) ->
    Ptr Spriter.CEntityInstance ->
    m (CharacterOutput t)
aiCharacterNetwork startTime space playerBody aiTick playerPosition debugSettings pos (feetShape, bodyShape) sprite = do
    let speedForAnim "Run" = playerSpeed / 3
        speedForAnim _ = playerSpeed / 6

        mummySpeed AiRight anim = H.Vector (- speedForAnim anim) 0
        mummySpeed AiLeft anim = H.Vector (speedForAnim anim) 0
        mummySpeed AiStay _ = H.zero

        rightSideSegment currentPos =
            ( currentPos + mummySideCheckUR
            , currentPos + mummySideCheckLR
            )
        leftSideSegment currentPos =
            ( currentPos + mummySideCheckUL
            , currentPos + mummySideCheckLL
            )

        playerBodyPosition = playerPosition - pure (H.Vector 0 15)

        checkSide col =
            let shape = H.segQueryInfoShape col
            in if shape == nullPtr then
                   return Nothing
               else
                   (\b -> if b == playerBody then Just () else Nothing) <$> get (H.shapeBody shape)

        clock = clockLossy (1/60) startTime

        mummyPunchEffect :: H.Vector -> Direction -> Performable m Bool
        mummyPunchEffect position dir =
            liftIO $ attackEffect space position dir 500
                (\_ hitBody ->
                     if hitBody == playerBody then
                         putStrLn "punched player" >> return True
                     else
                         return False)


    --body <- get $ H.shapeBody feetShape

    rec
        let mummyEyePos = pos + pure (H.Vector 0 (-20))

            mummyVisionLine = extend 20 <$> ((,) <$> mummyEyePos <*> playerBodyPosition)

            checkForPlayer dir visionLine@(H.Vector mx my, H.Vector px py) = liftIO $ do

                playerSearchHitInfo <- queryLineSeg space visionLine aiVisibleFilter
                let hitShape = H.segQueryInfoShape playerSearchHitInfo
                    playerInFront = case dir of
                        DRight -> px > mx
                        DLeft -> px < mx
                    distToPlayer = H.len $ H.Vector (px - mx) (py - my)
                    playerAngleCos = abs (px - mx) / distToPlayer
                    playerAngleAcceptable = playerAngleCos > cos (pi / 4)
                playerNotBlocked <- if hitShape /= nullPtr
                    then do
                        hitBody <- get $ H.shapeBody hitShape
                        return (hitBody == playerBody)
                    else
                        return False

                return $ distToPlayer < 200 && playerInFront && playerAngleAcceptable && playerNotBlocked

            doAiCollisionChecks mp = liftIO $ do
                let (pkc1r, pkc2r) = playerKickCheckR
                    (pkc1l, pkc2l) = playerKickCheckL
                    segsToCheck =
                        [ rightSideSegment mp
                        , leftSideSegment mp
                        , (pkc1r + mp, pkc2r + mp)
                        , (pkc1l + mp, pkc2l + mp)
                        ]

                [cgr, cgl, cwr, cwl] <- forM segsToCheck $ \seg ->
                    queryLineSeg space seg groundCheckFilter
                return (cgr, cgl, cwr, cwl)

        eCanSeePlayer <- performEvent $ checkForPlayer <$> mummyDisplayDir <*> mummyVisionLine <@ aiTick

        mummySeesPlayer <- hold False eCanSeePlayer

        let runAiLogic (colGroundRight, colGroundLeft, colWallRight, colWallLeft) (currentDir, _currentAnim) = do
                canSeePlayer <- sample mummySeesPlayer
                dir <- sample mummyDisplayDir

                let canGoLeft = H.segQueryInfoShape colGroundLeft /= nullPtr && H.segQueryInfoShape colWallLeft == nullPtr
                    canGoRight = H.segQueryInfoShape colGroundRight /= nullPtr && H.segQueryInfoShape colWallRight == nullPtr

                return $ case currentDir of
                    AiLeft
                        | canGoLeft -> if not canSeePlayer then (AiLeft, "Walk") else (AiLeft, "Run")
                        | canGoRight -> if canSeePlayer then (AiStay, "Idle") else (AiRight, "Walk")
                        | otherwise -> (AiStay, "Idle")
                    AiRight
                        | canGoRight -> if not canSeePlayer then (AiRight, "Walk") else (AiRight, "Run")
                        | canGoLeft -> if canSeePlayer then (AiStay, "Idle") else (AiLeft, "Walk")
                        | otherwise -> (AiStay, "Idle")
                    AiStay
                        | canSeePlayer && dir == DRight && not canGoRight -> (AiStay, "Idle")
                        | canSeePlayer && dir == DLeft && not canGoLeft -> (AiStay, "Idle")
                        | canGoRight -> (AiRight, "Walk")
                        | canGoLeft -> (AiLeft, "Walk")
                        | otherwise -> (AiStay, "Idle")

        colResults <- performEvent $ liftIO <$> (doAiCollisionChecks <$> pos <@ aiTick)

        let checkForHitPossibility canSeePlayer dir (_, _, colRight, colLeft) = liftIO $
                if not canSeePlayer then
                    return Nothing
                else
                    case dir of
                        DLeft -> checkSide colLeft
                        DRight -> checkSide colRight

        eHitChecks <- performEvent $
            checkForHitPossibility <$> mummySeesPlayer <*> mummyDisplayDir <@> colResults
        let eWantsToHitPlayer = fmapMaybe id eHitChecks

        performEvent_ $ liftIO (Spriter.setEntityInstanceCurrentTime sprite 0) <$ eWantsToHitPlayer

        tickInfo <- current <$> clock
        let timeSinceStart = flip Time.diffUTCTime startTime . _tickInfo_lastUTC <$> tickInfo
            punchDelay = 0.25
            punchDuration = 0.6

        latestMummyPunch <- hold Nothing $ Just <$> timeSinceStart <@ eWantsToHitPlayer
        eDelayedPlayerKick <- delay punchDelay eWantsToHitPlayer

        eHitPlayer <- performEvent $ mummyPunchEffect <$> pos <*> mummyDisplayDir <@ eDelayedPlayerKick

        let isPunching = do
                t <- timeSinceStart
                mLatestPunchStart <- latestMummyPunch
                return $ case mLatestPunchStart of
                    Nothing -> False
                    Just latestPunchStart -> t < latestPunchStart + punchDuration

        mummyWalkingState <- foldDynM runAiLogic (AiStay, "Idle") colResults
        let mummyAiState = do
                currentlyPunching <- isPunching
                return $
                    if currentlyPunching then
                        pure (AiStay, "Punch")
                    else
                        mummyWalkingState

        let mummyWalkDirectionDyn = fmap fst <$> mummyAiState
            eMummyWalkDirection = switch $ updated <$> mummyWalkDirectionDyn
            mummyWalkDirection = join $ current <$> mummyWalkDirectionDyn

        mummyDisplayDir <- hold DRight $ fmapMaybe aiDirToDirection eMummyWalkDirection

    let mummyAnimation = join $ fmap current $ fmap snd <$> mummyAiState
        mummySurfaceVelocity = mummySpeed <$> mummyWalkDirection <*> mummyAnimation

    let aiAnimation = do
            aiCurrentPosition <- pos
            aiCurrentAnimation <- mummyAnimation
            aiCurrentDirection <- mummyDisplayDir
            return [ AnimatedSprite sprite aiCurrentAnimation (toV2 aiCurrentPosition) aiCurrentDirection ]

        renderColDebug = do
            currentAiPosition <- pos
            (eyePos, playerBodyPos) <- mummyVisionLine
            seesPlayer <- mummySeesPlayer

            let renderFeetAndBody =
                    [ Shape feetShape characterFeetShapeType
                    , Shape bodyShape characterBodyShapeType
                    ]
                chrLine chrP (p1, p2) =
                    Line (V4 255 0 0 255) (toV2 $ p1 + chrP) (toV2 $ p2 + chrP)
                mummyLines mummyP =
                    [ chrLine mummyP (mummySideCheckUL, mummySideCheckLL)
                    , chrLine mummyP (mummySideCheckUR, mummySideCheckLR)
                    , chrLine mummyP playerKickCheckR
                    , chrLine mummyP playerKickCheckL
                    ]
                renderEnemyVision =
                    let color = if seesPlayer then V4 0 255 0 255 else V4 255 0 0 255
                    in Line color (toV2 eyePos) (toV2 playerBodyPos)
                renderSideChecks =
                     mummyLines currentAiPosition ++ [ renderEnemyVision ]

            debugRenderCharacter debugSettings renderFeetAndBody renderSideChecks

    return CharacterOutput
        { characterSurfaceVelocity = mummySurfaceVelocity
        , characterForce = pure H.zero
        , characterDirection = mummyDisplayDir
        , characterAnimation = mummyAnimation
        , characterRendering = aiAnimation <> renderColDebug
        }

data GamepadInput = GamepadInput
    { leftXAxis :: CDouble
    , yPressed :: Bool
    }

initialInput :: GamepadInput
initialInput = GamepadInput
    { leftXAxis = 0
    , yPressed = False
    }

axisValue :: Int16 -> CDouble
axisValue v = fromIntegral v / 32768

pollInput :: MonadIO m => Maybe SDL.Joystick -> m GamepadInput
pollInput mGamepad =
    case mGamepad of
        Nothing -> return initialInput
        Just gamepad -> do
            currentXLeftAxis <- SDL.axisPosition gamepad 0
            currentYPressed <- SDL.buttonPressed gamepad padButtonY
            return GamepadInput
                { leftXAxis = (\v -> if abs v < 0.15 then 0 else v) $ axisValue currentXLeftAxis
                , yPressed = currentYPressed
                }

playerNetwork :: forall t m. MonadGame t m =>
    Time.UTCTime ->
    Ptr H.Space ->
    EventSelector t SdlEventTag ->
    Behavior t (SDL.Scancode -> Bool) ->
    Behavior t H.Vector ->
    Behavior t Bool ->
    Behavior t [Ptr H.Body] ->
    Behavior t DebugRenderSettings ->
    Maybe SDL.Joystick ->
    Ptr H.Body ->
    (Ptr H.Shape, Ptr H.Shape) ->
    Ptr Spriter.CEntityInstance ->
    m (CharacterOutput t, Behavior t Bool)
playerNetwork startTime space sdlEventFan pressedKeys pos onGround aiBodies debugSettings mGamepad body (feetShape, bodyShape) sprite = do
    let pressEvent kc = ffilter isPress $ select sdlEventFan (KeyEvent kc)
        eAPressed = pressEvent SDL.KeycodeA
        eDPressed = pressEvent SDL.KeycodeD
        eWPressed = pressEvent SDL.KeycodeW
        eKPressed = pressEvent SDL.KeycodeK
        padButtonPress = select sdlEventFan (JoyButtonEvent 0)
        padAxisMove = select sdlEventFan (JoyAxisEvent 0)
        padFilterButtonPress b = ffilter
            (\(SDL.JoyButtonEventData _ b' s) -> b == b' && s == 1) padButtonPress
        ePadAPressed = padFilterButtonPress padButtonA
        ePadXPressed = padFilterButtonPress padButtonX
        ePadNotCenter = ffilter
            (\(SDL.JoyAxisEventData _ a v) ->
                 a == padXAxis &&
                abs (axisValue v) > 0.15)
            padAxisMove
        ePadChangeDir = (\(SDL.JoyAxisEventData _ _ v) -> if v > 0 then DRight else DLeft)
            <$> ePadNotCenter

        ePlayerWantsToJump = mconcat [() <$ eWPressed, () <$ ePadAPressed]
        ePlayerWantsToKick = mconcat [() <$ eKPressed, () <$ ePadXPressed]
        ePlayerKick = ePlayerWantsToKick -- TODO: Add check for current player state

        jumpEvent = (-1000) <$ gate onGround ePlayerWantsToJump

        jump imp = liftIO $ H.applyImpulse body (H.Vector 0 imp) H.zero

        playerKickEffect :: H.Vector -> Direction -> [Ptr H.Body] -> Performable m Bool
        playerKickEffect playerP playerD currentAiBodies =
            liftIO $ attackEffect space playerP playerD 1000
                (\_ hitBody -> if hitBody `elem` currentAiBodies then
                        putStrLn "Kicked mummy" >> return True
                    else
                        return False)

    ePollInput <- tickLossy (1/15) startTime
    eCurrentInput <- performEvent $ pollInput mGamepad <$ ePollInput
    gamepadInput <- hold initialInput eCurrentInput

    performEvent_ $ liftIO . print <$> padButtonPress

    let aPressed = ($ SDL.ScancodeA) <$> pressedKeys
        dPressed = ($ SDL.ScancodeD) <$> pressedKeys
        iPressed = ($ SDL.ScancodeI) <$> pressedKeys
        playerKeyMovement = controlVx 1 <$> aPressed <*> dPressed
        playerAxisMovement = leftXAxis <$> gamepadInput
        playerMovement = limit $ (+) <$> playerKeyMovement <*> playerAxisMovement
        playerAirForce = (\d -> H.Vector (d * 1000) 0) <$> playerMovement
        playerAcc = H.Vector <$> fmap (* playerSpeed) playerMovement <*> pure 0
        playerSurfaceVelocity = bool
            <$> pure (H.Vector 0 0) <*> playerAcc <*> onGround
        playerMoving = (\(H.Vector vx _) -> abs vx > 0) <$> playerSurfaceVelocity

        kickDuration = 0.6
        kickDelay = 0.4
        pickAnimation moving onGroundNow lastKickTime currentTime =
            let runOrIdle
                    | not onGroundNow = "Falling"
                    | moving = "Run"
                    | otherwise = "Idle"
            in case lastKickTime of
                Just t -> if currentTime - t < kickDuration then "Kick" else runOrIdle
                Nothing -> runOrIdle

        clock = clockLossy (1/60) startTime

    tickInfo <- current <$> clock
    let timeSinceStart = flip Time.diffUTCTime startTime . _tickInfo_lastUTC <$> tickInfo

    latestPlayerKick <- hold Nothing $ Just <$> timeSinceStart <@ ePlayerKick
    eDelayedPlayerKick <- delay kickDelay ePlayerKick

    playerDir <- hold DRight $ leftmost
        [ DLeft <$ eAPressed
        , DRight <$ eDPressed
        , ePadChangeDir
        ]

    performEvent_ $ liftIO (Spriter.setEntityInstanceCurrentTime sprite 0) <$ ePlayerKick
    hitEnemy <- performEvent $ playerKickEffect <$> pos <*> playerDir <*> aiBodies <@ eDelayedPlayerKick

    performEvent_ $ jump <$> jumpEvent

    let playerAnimation =
            pickAnimation <$> playerMoving <*> onGround <*> latestPlayerKick <*> timeSinceStart
        horizontalForce = bool <$> playerAirForce <*> pure H.zero <*> onGround
        jetpackOn = (||) <$> iPressed <*> fmap yPressed gamepadInput
        verticalForce = bool H.zero (H.Vector 0 $ -6000) <$> jetpackOn
        playerForce = horizontalForce + verticalForce

        renderCharacter = sequenceA
            [ AnimatedSprite sprite <$> playerAnimation <*> fmap toV2 pos <*> playerDir
            ]

        renderColDebug = do
            playerP <- pos
            let renderFeetAndBody =
                    [ Shape feetShape characterFeetShapeType
                    , Shape bodyShape characterBodyShapeType
                    ]
                chrLine chrP (p1, p2) =
                    Line (V4 255 0 0 255) (toV2 $ p1 + chrP) (toV2 $ p2 + chrP)
                renderSideChecks =
                    [ chrLine playerP playerKickCheckR
                    , chrLine playerP playerKickCheckL
                    ]

            debugRenderCharacter debugSettings renderFeetAndBody renderSideChecks

    return ( CharacterOutput
             { characterSurfaceVelocity = playerSurfaceVelocity
             , characterForce = playerForce
             , characterAnimation = playerAnimation
             , characterDirection = playerDir
             , characterRendering = renderCharacter <> renderColDebug
             }
           , jetpackOn
           )

data PhysicsOutput = PhysicsOutput
    { physicsPlayerOnGround :: Bool
    , physicsPlayerPosition :: H.Vector
    , physicsPlayerVelocity :: H.Vector
    , physicsAiPositions :: [H.Vector]
    }

samplePhysics :: Ptr H.Body -> [Ptr H.Body] -> IO PhysicsOutput
samplePhysics playerBody characterBodies = do
    arbiters <- H.bodyEachArbiterList playerBody
    playerCollisionWithGround <- forM arbiters $ \arb -> do
        (s1, s2) <- H.arbiterGetShapes arb
        ct1 <- get $ H.collisionType s1
        ct2 <- get $ H.collisionType s2
        let isPlayer = ct1 == playerFeetCollisionType
            onSolid = ct2 == mummyCollisionType || ct2 == wallCollisionType
        return (isPlayer && onSolid)
    playerPosition <- get $ H.position playerBody
    playerVelocity <- get $ H.velocity playerBody
    aiPositions <- mapM (get . H.position) characterBodies
    return PhysicsOutput
        { physicsPlayerOnGround = or playerCollisionWithGround
        , physicsPlayerPosition = playerPosition
        , physicsPlayerVelocity = playerVelocity
        , physicsAiPositions = aiPositions
        }

particleSpawnInterval :: Time.NominalDiffTime
particleSpawnInterval = 0.02

data Particle = Particle
    { particleVelocity :: V2 CDouble
    , particleAngularVel :: CDouble
    , particleStartTime :: Time.UTCTime
    , particleLifetime :: Time.NominalDiffTime
    , particleStartPos :: V2 CDouble
    } deriving Show

particleState :: Time.UTCTime -> Particle -> (V2 CDouble, CDouble)
particleState time particle =
    let livedTime = Time.diffUTCTime time (particleStartTime particle)
    in ( particleStartPos particle + realToFrac livedTime *^ particleVelocity particle
       , particleAngularVel particle * realToFrac livedTime
       )

particleAlive :: Time.UTCTime -> Particle -> Bool
particleAlive time particle =
    let livedTime = Time.diffUTCTime time (particleStartTime particle)
    in livedTime < particleLifetime particle

initLevelNetwork :: forall t m. MonadGame t m =>
    Time.UTCTime ->
    SDL.Renderer ->
    EventSelector t SdlEventTag ->
    Event t Int ->
    Behavior t (SDL.Scancode -> Bool) ->
    Maybe SDL.Joystick ->
    LevelLoadedData ->
    m (LogicOutput t)
initLevelNetwork startTime textureRenderer sdlEventFan eStepPhysics pressedKeys mGamepad levelLoaded = do
    let space = levelSpace levelLoaded
        (playerFeetShape, playerBodyShape) = playerPhysicsRefs levelLoaded
        aiShapesAndSprites = aiPhysicsRefs levelLoaded :: [(CharacterPhysicsRefs, Ptr Spriter.CEntityInstance)]
        aiShapes = fst <$> aiShapesAndSprites
        aiFeetShapes = fst <$> aiShapes
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

    let pressEvent kc = ffilter isPress $ select sdlEventFan (KeyEvent kc)
        eF1Pressed = pressEvent SDL.KeycodeF1
        eF2Pressed = pressEvent SDL.KeycodeF2
        eF3Pressed = pressEvent SDL.KeycodeF3

    debugRendering <- current <$> toggle True eF1Pressed
    characterDbg <- current <$> toggle False (gate debugRendering eF2Pressed)
    colCheckDbg <- current <$> toggle False (gate debugRendering eF3Pressed)

    let debugRenderSettings = DebugRenderSettings
            <$> debugRendering
            <*> characterDbg
            <*> colCheckDbg

    rec
        let playerOnGround = physicsPlayerOnGround <$> physicsOutput
            playerPosition = physicsPlayerPosition <$> physicsOutput
            playerVelocity = physicsPlayerVelocity <$> physicsOutput
            aiSurfaceVelocities = sequenceA $ characterSurfaceVelocity <$> aiCharacters
            aiPositions = physicsAiPositions <$> physicsOutput :: Behavior t [H.Vector]

        (player, jetpackOn) <-
            playerNetwork
                startTime
                space
                sdlEventFan
                pressedKeys
                playerPosition
                playerOnGround
                (pure aiBodies)
                debugRenderSettings
                mGamepad
                playerBody
                (playerFeetShape, playerBodyShape)
                (playerSpriterInstance levelLoaded)

        -- TODO: Will only work for a single character and will crash if there are none :P
        -- fix with a normal map and holdGame maybe
        aiCharacters <-
            mapM
                (\(pos, shapes, sprite) ->
                     aiCharacterNetwork startTime space playerBody aiTick playerPosition debugRenderSettings pos shapes sprite)
                [(head <$> aiPositions, head aiShapes, head aiSprites)]

        eSteppedPhysics <- performEvent $ stepPhysics
            <$> characterSurfaceVelocity player
            <*> aiSurfaceVelocities
            <*> characterForce player
            <@> eStepPhysics
        physicsOutput <- hold initialPhysicsState eSteppedPhysics

    let playerEntityInstance = playerSpriterInstance levelLoaded

    eSpriterTick <- tickLossy spriterTimeStep startTime
    eSpriterDiffs <- mapAccum_ (\oldTime newTime -> (newTime, Time.diffUTCTime newTime oldTime))
        startTime $ _tickInfo_lastUTC <$> eSpriterTick
    performEvent_ $ liftIO . Spriter.setEntityInstanceTimeElapsed playerEntityInstance
        . realToFrac . (*1000) <$> eSpriterDiffs
    forM_ aiSprites $ \aiEntityInstance ->
        performEvent_ $ liftIO . Spriter.setEntityInstanceTimeElapsed aiEntityInstance
            . realToFrac . (*1000) <$> eSpriterDiffs

    jetpackTex <- SDL.Image.loadTexture textureRenderer "res/jetpack.png"
    liftIO $ putStrLn "Loaded: res/jetpack.png"
    particleTex <- SDL.Image.loadTexture textureRenderer "res/jetpack particle.png"
    liftIO $ putStrLn "Loaded: res/jetpack particle.png"

    let clock = clockLossy (1/60) startTime

    tickInfo <- current <$> clock
    stdGen <- liftIO newStdGen

    let time = _tickInfo_lastUTC <$> tickInfo
        particleXVelocities = randomRs (-20, 20) stdGen
        jetpackPosition = do
            pos <- playerPosition
            dir <- characterDirection player
            let jetpackOffset = case dir of
                    DLeft -> V2 3 (-25)
                    DRight -> V2 (-8) (-25)
            return $ toV2 pos + jetpackOffset


    eSpawnParticleTick <- tickLossy particleSpawnInterval startTime
    eParXVel <- zipListWithEvent const particleXVelocities $
        gate jetpackOn eSpawnParticleTick

    let spawnParticle xv = do
            playerV <- sample playerVelocity
            pos <- sample jetpackPosition
            t <- sample time
            return Particle
                { particleVelocity = toV2 playerV + V2 xv 200
                , particleAngularVel = xv
                , particleStartTime = t
                , particleLifetime = realToFrac $ 0.5 + 0.05 * xv
                , particleStartPos = pos
                }
        eNewParticle = pushAlways spawnParticle eParXVel

    rec
        particles <- hold [] $ flip (:) <$> aliveParticles <@> eNewParticle
        let aliveParticles = (filter . particleAlive) <$> time <*> particles

    let renderShapes = do
            dbgRender <- debugRendering
            if dbgRender then
                return $ uncurry Shape <$> extraPhysicsRefs levelLoaded
            else
                return []

        renderJetpack = do
            pos <- jetpackPosition
            anim <- characterAnimation player
            dir <- characterDirection player
            let angle =
                    if anim == "Run" then
                        if dir == DRight then 10 else -10
                    else
                        0
            return [ StaticSprite jetpackTex pos angle ]

        renderParticles = do
            pars <- aliveParticles
            currentTime <- time
            forM (particleState currentTime <$> pars) $ \(pos, angle) ->
                return $ StaticSprite particleTex pos angle

    return LogicOutput
        { cameraCenterPosition = toV2 . physicsPlayerPosition <$> physicsOutput
        , renderCommands =
                renderParticles
                <> renderJetpack
                <> characterRendering player
                <> mconcat (fmap characterRendering aiCharacters)
                <> renderShapes
        , quit = () <$ pressEvent SDL.KeycodeQ
        }
