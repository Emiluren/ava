{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, GADTs, TemplateHaskell #-}
module Main where

import Control.Arrow ((***))
import Control.Concurrent (forkIO, killThread)
import qualified Control.Concurrent.Chan as Chan
import Control.Lens ((^.))
import Control.Monad (unless, replicateM_)
import Control.Monad.Identity
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Ref (Ref, MonadRef)

import Data.Bool (bool)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum ((==>), DSum(..))
import Data.Fixed (div')
import Data.GADT.Compare.TH
import Data.IORef (newIORef, modifyIORef', readIORef, writeIORef)
import Data.Maybe (isJust, fromMaybe)
import Data.StateVar (($=), get)
import qualified Data.Time.Clock as Time
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as V
import Data.Word (Word8)

import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CInt, CUInt, CFloat(..), CDouble(..))
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr, castPtr, freeHaskellFunPtr, nullPtr)
import Foreign.StablePtr
    ( newStablePtr
    , deRefStablePtr
    , castStablePtrToPtr
    , castPtrToStablePtr
    , freeStablePtr
    )
import Foreign.Storable

import GHC.Float (float2Double)

import Linear (V2(..), V4(..))

import qualified ChipmunkBindings as H
import qualified ChipmunkTypes as H

import Reflex
import Reflex.Host.Class
    ( newEventWithTriggerRef
    , runHostFrame
    , fireEventRef
    , subscribeEvent
    , readEvent
    , MonadReflexHost
    )
import Reflex.Time

import SDL (Point(P))
import qualified SDL
import qualified SDL.Raw.Types as SDL (JoystickID)
import qualified SDL.Image
import qualified SDL.Primitive as SDL

import qualified SpriterTypes as Spriter
import qualified SpriterBindings as Spriter

timeStep :: Time.NominalDiffTime
timeStep = 1/120

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

cameraOffset :: MonadIO m => SDL.Window -> SDL.Renderer -> m (V2 Double)
cameraOffset window textureRenderer = do
    (V2 ww wh) <- get $ SDL.windowSize window
    (V2 (CFloat rsx) (CFloat rsy)) <- get $ SDL.rendererScale textureRenderer
    return $ V2
        (fromIntegral ww / 2 / float2Double rsx)
        (fromIntegral wh / 2 / float2Double rsy)

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

frameTime :: Double
frameTime = 0.05

playerFrames :: Int
playerFrames = 10

playerFeetCollisionType :: CUInt
playerFeetCollisionType = 1

data SdlEventTag a where
    ControllerDeviceEvent :: SdlEventTag SDL.ControllerDeviceEventData
    JoyAxisEvent :: SDL.JoystickID -> SdlEventTag SDL.JoyAxisEventData
    JoyButtonEvent :: SDL.JoystickID -> SdlEventTag SDL.JoyButtonEventData
    KeyEvent :: SDL.Keycode -> SdlEventTag SDL.KeyboardEventData
    OtherEvent :: SdlEventTag SDL.Event

deriveGEq ''SdlEventTag
deriveGCompare ''SdlEventTag

newtype RenderState = RenderState
    { cameraPosition :: V2 Double
    }

data Direction = DLeft | DRight deriving (Eq, Show)

data AiDir = AiLeft | AiStay | AiRight deriving (Eq, Show)

aiDirToDirection :: AiDir -> Maybe Direction
aiDirToDirection AiStay = Nothing
aiDirToDirection AiLeft = Just DLeft
aiDirToDirection AiRight = Just DRight

sortEvent :: SDL.Event -> DMap SdlEventTag Identity
sortEvent event =
    wrapInDMap $ case SDL.eventPayload event of
        SDL.ControllerDeviceEvent axis ->
            ControllerDeviceEvent ==> axis
        SDL.JoyAxisEvent eventData ->
            JoyAxisEvent (SDL.joyAxisEventWhich eventData) ==> eventData
        SDL.JoyButtonEvent eventData ->
            JoyButtonEvent (SDL.joyButtonEventWhich eventData) ==> eventData
        SDL.KeyboardEvent eventData ->
            KeyEvent (eventKeycode eventData) ==> eventData
        _ ->
            OtherEvent ==> event
    where wrapInDMap x = DMap.fromList [x]

data LogicOutput t = LogicOutput
    { playerSurfaceVel :: Behavior t H.Vector
    , mummySurfaceVel :: Behavior t H.Vector
    , playerForce :: Behavior t H.Vector
    , debugRenderingEnabled :: Behavior t Bool
    , debugRenderCharacters :: Behavior t Bool
    , debugCollisionChecksEnabled :: Behavior t Bool
    , playerAnimation :: Behavior t String
    , mummyAnimation :: Behavior t String
    , playerDirection :: Behavior t Direction
    , mummyDirection :: Behavior t Direction
    , quit :: Event t ()
    }

renderTextureSize :: Num a => a
renderTextureSize = 512

mutableBehavior :: (Ref m ~ Ref IO, MonadRef m, MonadReflexHost t m)
    => a -> m (Behavior t a, a -> m ())
mutableBehavior startValue = do
    (eUpdateValue, eUpdateValueTriggerRef) <- newEventWithTriggerRef
    value <- runHostFrame $ hold startValue eUpdateValue
    return (value, fireEventRef eUpdateValueTriggerRef)

limit :: (Num a, Ord a, Reflex t) => Behavior t a -> Behavior t a
limit = fmap limf where
    limf x
        | x < -1 = -1
        | x > 1 = 1
        | otherwise = x

main :: IO ()
main = do
    SDL.initializeAll

    let windowSettings = SDL.defaultWindow
            { SDL.windowResizable = True
            }
    window <- SDL.createWindow "Ava" windowSettings
    textureRenderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
                { SDL.rendererTargetTexture = True
                }

    renderTexture <- SDL.createTexture textureRenderer
        SDL.RGBA8888 SDL.TextureAccessTarget (V2 renderTextureSize renderTextureSize)

    loadedImages <- newIORef []

    let loadImage :: CString -> CDouble -> CDouble -> IO (Ptr Spriter.Sprite)
        loadImage filename pivotX pivotY = do
            name <- peekCString filename

            tex <- SDL.Image.loadTexture textureRenderer name
            putStrLn $ "Loaded: " ++ name

            let sprite = Spriter.Sprite
                    { Spriter._spriteTexture = tex
                    , Spriter._spritePivotX = pivotX
                    , Spriter._spritePivotY = pivotY
                    , Spriter._spriteName = name
                    }

            stablePtr <- newStablePtr sprite

            modifyIORef' loadedImages (stablePtr:)
            return $ castPtr $ castStablePtrToPtr stablePtr

    imgloader <- Spriter.makeImageLoader loadImage
    renderf <- Spriter.makeRenderer $ renderSprite textureRenderer

    Spriter.setErrorFunction

    playerSpriterModel <- withCString "res/princess/Princess.scon"
        (Spriter.loadSpriterModel imgloader renderf)
    playerEntityInstance <- withCString "Princess" $ Spriter.modelGetNewEntityInstance playerSpriterModel
    withCString "Idle" $ Spriter.setEntityInstanceCurrentAnimation playerEntityInstance

    mummySpriterModel <- withCString "res/mummy/Mummy.scon"
        (Spriter.loadSpriterModel imgloader renderf)
    mummyEntityInstance <- withCString "Mummy" $ Spriter.modelGetNewEntityInstance mummySpriterModel
    withCString "Idle" $ Spriter.setEntityInstanceCurrentAnimation mummyEntityInstance

    putStrLn "Creating chipmunk space"
    space <- H.newSpace
    H.gravity space $= H.Vector 0 400

    wall <- H.spaceGetStaticBody space
    --H.position wall $= H.Vector 0 0

    let shelfShapeType = H.LineSegment startV endV 1
    shelf <- H.newShape wall shelfShapeType
    H.friction shelf $= 1.0
    H.elasticity shelf $= 0.6
    H.spaceAddShape space shelf

    let tl = H.Vector 0 0
        bl = H.Vector 0 400
        tr = H.Vector 400 0
        br = H.Vector 400 400

        corners = [tl, H.Vector (-200) (-10), H.Vector (-200) 360, bl, H.Vector 150 380, br, tr]
        edges = zip corners $ tail corners ++ [head corners]

    wallShapes <- forM edges $ \(start, end) -> do
        let wst = H.LineSegment start end 1
        w <- H.newShape wall wst
        H.friction w $= 1.0
        H.elasticity w $= 0.6
        H.spaceAddShape space w
        return (w, wst)

    let circleMoment = H.momentForCircle circleMass (0, circleRadius) (H.Vector 0 0)

    circleBody <- H.newBody circleMass circleMoment
    H.spaceAddBody space circleBody

    let circleShapeType = H.Circle circleRadius H.zero
    circleShape <- H.newShape circleBody circleShapeType
    H.friction circleShape $= 1.0
    H.elasticity circleShape $= 0.9
    H.spaceAddShape space circleShape
    H.position circleBody $= H.Vector 200 20

    let makeCharacterBody w h =
            [ H.Vector (-w * 0.5) (-h * 0.2)
            , H.Vector (-w * 0.5) (-h * 0.8)
            , H.Vector (-w * 0.3) (-h)
            , H.Vector (w * 0.3) (-h)
            , H.Vector (w * 0.5) (-h * 0.8)
            , H.Vector (w * 0.5) (-h * 0.2)
            , H.Vector (w * 0.3) (-h * 0.1)
            , H.Vector (-w * 0.3) (-h * 0.1)
            ]
        characterWidth = 10
        characterHeight = 30
        characterFeetShapeType = H.Circle (characterWidth * 0.3) (H.Vector 0 $ - characterWidth * 0.2)
        characterBodyShapeType = H.Polygon (V.fromList $ reverse $ makeCharacterBody characterWidth characterHeight) 0
        characterMass = 5
        characterFeetFriction = 2
        characterSide = characterWidth * 0.7

        mummySideCheckUR = H.Vector characterSide (-10)
        mummySideCheckLR = H.Vector characterSide 10
        mummySideCheckUL = H.Vector (-characterSide) (-10)
        mummySideCheckLL = H.Vector (-characterSide) 10

        playerKickCheckR =
            ( H.Vector characterSide (-17)
            , H.Vector (2.2 * characterSide) (-17)
            )
        playerKickCheckL =
            ( H.Vector (- characterSide) (-17)
            , H.Vector (- 2.2 * characterSide) (-17)
            )

    mummyBody <- H.newBody characterMass $ 1/0
    H.spaceAddBody space mummyBody
    mummyFeetShape <- H.newShape mummyBody characterFeetShapeType
    mummyBodyShape <- H.newShape mummyBody characterBodyShapeType
    H.spaceAddShape space mummyFeetShape
    H.spaceAddShape space mummyBodyShape
    H.friction mummyFeetShape $= characterFeetFriction
    H.friction mummyBodyShape $= 0.5
    H.position mummyBody $= H.Vector 110 200
    --H.collisionType mummyFeetShape $= playerFeetCollisionType

    playerBody <- H.newBody characterMass (1/0)
    H.spaceAddBody space playerBody
    playerFeetShape <- H.newShape playerBody characterFeetShapeType
    playerBodyShape <- H.newShape playerBody characterBodyShapeType
    H.spaceAddShape space playerFeetShape
    H.spaceAddShape space playerBodyShape
    H.friction playerFeetShape $= characterFeetFriction
    H.friction playerBodyShape $= 0.5
    H.position playerBody $= H.Vector 240 100
    H.collisionType playerFeetShape $= playerFeetCollisionType

    let
        render :: MonadIO m => V2 CDouble -> V2 Double -> V2 Double -> Bool -> Bool -> Bool -> m ()
        render camPos playerPos mummyPos debugRendering characterDbg colCheckDbg = do
            SDL.rendererRenderTarget textureRenderer $= Just renderTexture
            SDL.rendererDrawColor textureRenderer $= V4 150 150 200 255
            SDL.clear textureRenderer

            liftIO $ do
                Spriter.setEntityInstancePosition playerEntityInstance ((CDouble <$> playerPos) - camPos)
                Spriter.renderEntityInstance playerEntityInstance

                Spriter.setEntityInstancePosition mummyEntityInstance ((CDouble <$> mummyPos) - camPos)
                Spriter.renderEntityInstance mummyEntityInstance

            when debugRendering $ do
                SDL.rendererDrawColor textureRenderer $= V4 255 255 255 255
                renderShape textureRenderer camPos shelf shelfShapeType
                renderShape textureRenderer camPos circleShape circleShapeType

                forM_ wallShapes $ uncurry (renderShape textureRenderer camPos)

                when characterDbg $ do
                    SDL.rendererDrawColor textureRenderer $= V4 100 255 100 255
                    renderShape textureRenderer camPos playerFeetShape characterFeetShapeType
                    renderShape textureRenderer camPos playerBodyShape characterBodyShapeType

                    renderShape textureRenderer camPos mummyFeetShape characterFeetShapeType
                    renderShape textureRenderer camPos mummyBodyShape characterBodyShapeType

                when colCheckDbg $ do
                    SDL.rendererDrawColor textureRenderer $= V4 255 0 0 255

                    let characterOffset chrPos = floor <$> fmap CDouble chrPos - camPos
                        drawCharacterLine chr (p1, p2)= SDL.drawLine textureRenderer
                            (baseOff + convV p1)
                            (baseOff + convV p2)
                            where
                                baseOff = SDL.P $ characterOffset chr

                    drawCharacterLine mummyPos (mummySideCheckUL, mummySideCheckLL)
                    drawCharacterLine mummyPos (mummySideCheckUR, mummySideCheckLR)

                    drawCharacterLine playerPos playerKickCheckR
                    drawCharacterLine playerPos playerKickCheckL
                    drawCharacterLine mummyPos playerKickCheckR
                    drawCharacterLine mummyPos playerKickCheckL

            SDL.rendererRenderTarget textureRenderer $= Nothing
            (V2 ww wh) <- get $ SDL.windowSize window
            let dispHeight = floor (renderTextureSize / fromIntegral ww * fromIntegral wh :: Float)
                renderRect = SDL.Rectangle
                    (SDL.P $ V2 0 $ (renderTextureSize - dispHeight) `div` 2)
                    (V2 renderTextureSize dispHeight)
            SDL.copy textureRenderer renderTexture (Just renderRect) Nothing
            SDL.present textureRenderer

    runSpiderHost $ do
        (eSdlEvent, sdlTriggerRef) <- newEventWithTriggerRef
        (eStart, startTriggerRef) <- newEventWithTriggerRef

        startTime <- liftIO Time.getCurrentTime

        (playerOnGround, setPlayerOnGround) <- mutableBehavior False
        (pressedKeys, setPressedKeys) <- mutableBehavior =<< SDL.getKeyboardState
        (mummyPos, setMummyPos) <- mutableBehavior H.zero
        (playerPos, setPlayerPos) <- mutableBehavior H.zero

        playerOnGroundRef <- liftIO $ newIORef False

        joysticks <- SDL.availableJoysticks
        mGamepad <- if null joysticks
            then return Nothing
            else Just <$> SDL.openJoystick (Vector.head joysticks)

        (mAxis, mSetAxis) <- case mGamepad of
            Nothing -> return (Nothing, Nothing)
            Just _ -> (Just *** Just) <$> mutableBehavior (0 :: Double)

        let groundCheckFilter = H.ShapeFilter
                { H.shapeFilterGroup = H.noGroup
                , H.shapeFilterCategories = H.allCategories
                , H.shapeFilterMask = H.allCategories
                }

            sdlEventFan = fan eSdlEvent

            mainReflex :: PostBuildT
                (SpiderTimeline Global)
                (TriggerEventT
                    (SpiderTimeline Global)
                    (PerformEventT (SpiderTimeline Global) (SpiderHost Global)))
                (LogicOutput (SpiderTimeline Global))
            mainReflex = do
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

                    rightSideSegment pos =
                        ( pos + mummySideCheckUR
                        , pos + mummySideCheckLR
                        )
                    leftSideSegment pos =
                        ( pos + mummySideCheckUL
                        , pos + mummySideCheckLL
                        )

                aiTick <- tickLossy (1/15) startTime
                let queryLineSeg :: MonadIO m => (H.Vector, H.Vector) -> m H.SegmentQueryInfo
                    queryLineSeg (s, e) = liftIO $ H.spaceSegmentQueryFirst space s e 1 groundCheckFilter

                debugRendering <- current <$> toggle True eF1Pressed
                characterDbg <- current <$> toggle False (gate debugRendering eF2Pressed)
                colCheckDbg <- current <$> toggle False (gate debugRendering eF3Pressed)
                playerDir <- hold DRight $ leftmost
                    [ DLeft <$ eAPressed
                    , DRight <$ eDPressed
                    , ePadChangeDir
                    ]

                let aPressed = ($ SDL.ScancodeA) <$> pressedKeys
                    dPressed = ($ SDL.ScancodeD) <$> pressedKeys
                    playerKeyMovement = controlVx 1 <$> aPressed <*> dPressed
                    playerAxisMovement = CDouble <$> fromMaybe (pure 0) mAxis
                    playerMovement = limit $ (+) <$> playerKeyMovement <*> playerAxisMovement
                    playerAirForce = (\d -> H.Vector (d * 1000) 0) <$> playerMovement
                    playerAcc = H.Vector <$> fmap (* playerSpeed) playerMovement <*> pure 0
                    ePlayerWantsToJump = mconcat [() <$ eWPressed, () <$ ePadAPressed]
                    ePlayerWantsToKick = mconcat [() <$ eKPressed, () <$ ePadXPressed]
                    ePlayerKick = ePlayerWantsToKick -- TODO: Add check for current player state

                    jumpEvent = (-1000) <$ gate playerOnGround ePlayerWantsToJump

                    mummySpeed AiRight = H.Vector (- playerSpeed / 6) 0
                    mummySpeed AiLeft = H.Vector (playerSpeed / 6) 0
                    mummySpeed AiStay = H.zero

                    playerSurfaceVelocity = bool
                        <$> pure (H.Vector 0 0) <*> playerAcc <*> playerOnGround
                    playerMoving = (\(H.Vector vx _) -> abs vx > 0) <$> playerSurfaceVelocity

                    jump imp = liftIO $ H.applyImpulse playerBody (H.Vector 0 imp) H.zero

                    doAiCollisionChecks mp = liftIO $ do
                        let (pkc1r, pkc2r) = playerKickCheckR
                            (pkc1l, pkc2l) = playerKickCheckL
                            segsToCheck =
                                [ rightSideSegment mp
                                , leftSideSegment mp
                                , (pkc1r + mp, pkc2r + mp)
                                , (pkc1l + mp, pkc2l + mp)
                                ]

                        [cgr, cgl, cwr, cwl] <- forM segsToCheck $ \seg -> do
                            segHitInfo <- queryLineSeg seg
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

                colResults <- performEvent $ doAiCollisionChecks <$> mummyPos <@ aiTick

                mummyWalkDirection <- foldDynMaybe runAiLogic AiStay colResults
                mummyDisplayDir <- hold DRight $ fmapMaybe aiDirToDirection $ updated mummyWalkDirection

                let mummySurfaceVelocity = mummySpeed <$> current mummyWalkDirection
                    mummyMoving = (\(H.Vector vx _) -> abs vx > 0) <$> mummySurfaceVelocity
                    kickDuration = 0.6
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
                    playerKickEffect :: MonadIO m => H.Vector -> Direction -> m ()
                    playerKickEffect playerP playerD = do
                        let (pkc1, pkc2) = case playerD of
                                DRight -> playerKickCheckR
                                DLeft -> playerKickCheckL
                            playerKickVec = case playerD of
                                DRight -> H.Vector 1000 (-1000)
                                DLeft -> H.Vector (-1000) (-1000)

                        hitShapeInfo <- queryLineSeg (playerP + pkc1, playerP + pkc2)

                        let hitShape = H.segQueryInfoShape hitShapeInfo
                        when (hitShape /= nullPtr) $ do
                            hitBody <- get $ H.shapeBody hitShape
                            liftIO $ H.applyImpulse hitBody playerKickVec H.zero
                            when (hitBody == mummyBody) $ liftIO $ putStrLn "Kicked mummy"

                tickInfo <- current <$> clock
                let timeSinceStart = flip Time.diffUTCTime startTime . _tickInfo_lastUTC <$> tickInfo

                latestPlayerKick <- hold Nothing $ Just <$> timeSinceStart <@ ePlayerKick
                eDelayedPlayerKick <- delay kickDelay ePlayerKick

                performEvent_ $ liftIO (Spriter.setEntityInstanceCurrentTime playerEntityInstance 0) <$ ePlayerKick
                performEvent_ $ playerKickEffect <$> playerPos <*> playerDir <@ eDelayedPlayerKick

                performEvent_ $ jump <$> jumpEvent

                return LogicOutput
                    { playerSurfaceVel = playerSurfaceVelocity
                    , mummySurfaceVel = mummySurfaceVelocity
                    , playerForce = bool
                        <$> playerAirForce <*> pure (H.Vector 0 0) <*> playerOnGround
                    , debugRenderingEnabled = debugRendering
                    , debugRenderCharacters = characterDbg
                    , debugCollisionChecksEnabled = colCheckDbg
                    , playerAnimation =
                            pickAnimation <$> playerMoving <*> playerOnGround <*> latestPlayerKick <*> timeSinceStart
                    , mummyAnimation = (\moving -> if moving then "Walk" else "Idle") <$> mummyMoving
                    , playerDirection = playerDir
                    , mummyDirection = mummyDisplayDir
                    , quit = () <$ pressEvent SDL.KeycodeQ
                    }

        eventChan <- liftIO Chan.newChan
        (logicOutput, FireCommand fire) <- hostPerformEventT $
            runTriggerEventT (runPostBuildT mainReflex eStart) eventChan

        hQuit <- subscribeEvent $ quit logicOutput
        quitRef <- liftIO $ newIORef False

        let eventChanTriggerThread = do
                eventTriggers <- Chan.readChan eventChan
                --putStrLn "yolo"
                runSpiderHost $ forM_ eventTriggers $ \(EventTriggerRef triggerRef :=> TriggerInvocation value onComplete) -> do
                    --liftIO $ putStrLn "swag"
                    mTrigger <- liftIO $ readIORef triggerRef
                    lmQuit <- case mTrigger of
                        Nothing -> return []
                        Just trigger ->
                            fire [trigger :=> Identity value] $
                                readEvent hQuit >>= sequence
                    liftIO onComplete
                    when (any isJust lmQuit) $ liftIO $ writeIORef quitRef True
                eventChanTriggerThread

        playerArbiterCallback <- liftIO $ H.makeArbiterIterator
            (\_ arb -> do
                    (s1, s2) <- H.arbiterGetShapes arb
                    ct1 <- get $ H.collisionType s1
                    ct2 <- get $ H.collisionType s2
                    when (ct1 == 1 && ct2 == 0) $ writeIORef playerOnGroundRef True
                    return ())

        let hSample = runHostFrame . sample

            appLoop :: Time.UTCTime -> Time.NominalDiffTime -> SpiderHost Global ()
            appLoop oldTime timeAcc = do
                newTime <- liftIO Time.getCurrentTime
                let dt = min (Time.diffUTCTime newTime oldTime) 0.25
                    timeAcc' = timeAcc + dt
                    stepsToRun = timeAcc' `div'` timeStep

                liftIO $ replicateM_ stepsToRun $ H.step space $ realToFrac timeStep

                onGround <- liftIO $ do
                    writeIORef playerOnGroundRef False
                    H.bodyEachArbiter playerBody playerArbiterCallback
                    readIORef playerOnGroundRef
                setPlayerOnGround onGround

                setPressedKeys =<< SDL.getKeyboardState

                fromMaybe (return ()) $ do
                    gamepad <- mGamepad
                    setAxis <- mSetAxis
                    let gateV v = if abs v < 0.15 then 0 else v
                    Just $ setAxis =<< gateV . (/ 32768) . fromIntegral
                        <$> SDL.axisPosition gamepad 0

                mSdlTrigger <- liftIO $ readIORef sdlTriggerRef
                mQuit <- case mSdlTrigger of
                    Nothing -> return []
                    Just sdlTrigger -> do
                        events <- SDL.pollEvents
                        let triggerSdlEvent evt = sdlTrigger :=> Identity (sortEvent evt)
                        fire (fmap triggerSdlEvent events) $
                            readEvent hQuit >>= sequence

                currentPlayerSurfaceVel <- hSample $ playerSurfaceVel logicOutput
                H.surfaceVel playerFeetShape $= H.scale currentPlayerSurfaceVel (-1)

                currentPlayerForce <- hSample $ playerForce logicOutput
                H.force playerBody $= currentPlayerForce

                currentPlayerAnimation <- hSample $ playerAnimation logicOutput
                liftIO $ withCString currentPlayerAnimation $
                    Spriter.setEntityInstanceCurrentAnimation playerEntityInstance

                currentMummySurfaceVel <- hSample $ mummySurfaceVel logicOutput
                H.surfaceVel mummyFeetShape $= currentMummySurfaceVel

                currentMummyAnimation <- hSample $ mummyAnimation logicOutput
                liftIO $ withCString currentMummyAnimation $
                    Spriter.setEntityInstanceCurrentAnimation mummyEntityInstance

                let spriterTimeStep = realToFrac $ dt * 1000
                liftIO $ do
                    Spriter.setEntityInstanceTimeElapsed playerEntityInstance spriterTimeStep
                    Spriter.setEntityInstanceTimeElapsed mummyEntityInstance spriterTimeStep

                currentPlayerPos@(H.Vector (CDouble playerX) (CDouble playerY)) <- get $ H.position playerBody
                currentMummyPos@(H.Vector (CDouble mummyX) (CDouble mummyY)) <- get $ H.position mummyBody
                setPlayerPos currentPlayerPos
                setMummyPos currentMummyPos

                isDebugRenderingEnabled <- hSample $ debugRenderingEnabled logicOutput
                characterDbg <- hSample $ debugRenderCharacters logicOutput
                colCheckDbg <- hSample $ debugCollisionChecksEnabled logicOutput
                playerDir <- hSample $ playerDirection logicOutput
                mummyDir <- hSample $ mummyDirection logicOutput
                case playerDir of
                    DRight -> liftIO $ Spriter.setEntityInstanceScale playerEntityInstance $ V2 1 1
                    DLeft -> liftIO $ Spriter.setEntityInstanceScale playerEntityInstance $ V2 (-1) 1
                case mummyDir of
                    DRight -> liftIO $ Spriter.setEntityInstanceScale mummyEntityInstance $ V2 1 1
                    DLeft -> liftIO $ Spriter.setEntityInstanceScale mummyEntityInstance $ V2 (-1) 1
                let camOffset = V2 (renderTextureSize / 2) (renderTextureSize / 2)

                render
                    (CDouble <$> V2 playerX playerY - camOffset)
                    (V2 playerX playerY)
                    (V2 mummyX mummyY)
                    isDebugRenderingEnabled
                    characterDbg
                    colCheckDbg

                let shouldQuit = any isJust mQuit
                unless shouldQuit $
                    appLoop newTime $ timeAcc' - fromIntegral stepsToRun * timeStep

        mStartTrigger <- liftIO $ readIORef startTriggerRef
        _ <- case mStartTrigger of
            Nothing -> return []
            Just eTrigger -> fire [eTrigger :=> Identity ()] $ return Nothing

        eventTriggerId <- liftIO $ forkIO eventChanTriggerThread

        appLoop startTime 0.0

        liftIO $ do
            killThread eventTriggerId
            freeHaskellFunPtr playerArbiterCallback

    spriterImages <- readIORef loadedImages
    forM_ spriterImages $ \sprPtr -> do
        spr <- deRefStablePtr sprPtr
        let tex = spr ^. Spriter.spriteTexture
        SDL.destroyTexture tex
        freeStablePtr sprPtr

    SDL.quit

    free playerEntityInstance
    free playerSpriterModel
    free mummyEntityInstance
    free mummySpriterModel
    freeHaskellFunPtr imgloader
    freeHaskellFunPtr renderf
    H.freeSpace space

renderSprite :: SDL.Renderer -> Spriter.Renderer
renderSprite textureRenderer spritePtr spriteStatePtr = do
    sprite <- deRefStablePtr $ castPtrToStablePtr $ castPtr spritePtr
    spriteState <- peek spriteStatePtr

    textureInfo <- SDL.queryTexture $ sprite ^. Spriter.spriteTexture
    -- TODO: alpha is not used
    let w = fromIntegral $ SDL.textureWidth textureInfo
        h = fromIntegral $ SDL.textureHeight textureInfo
        scaleX = spriteState ^. Spriter.spriteStateScale . Spriter.pointX
        scaleY = spriteState ^. Spriter.spriteStateScale . Spriter.pointY
        sw = floor $ scaleX * w
        sh = floor $ scaleY * h
        px = floor $ (sprite ^. Spriter.spritePivotX) * fromIntegral sw
        py = floor $ (sprite ^. Spriter.spritePivotY) * fromIntegral sh
        pivot = Just $ SDL.P $ V2 px py
        angle = spriteState ^. Spriter.spriteStateAngle
        degAngle = angle * (180/pi)
        x = floor $ spriteState ^. Spriter.spriteStatePosition . Spriter.pointX - fromIntegral px
        y = floor $ spriteState ^. Spriter.spriteStatePosition . Spriter.pointY - fromIntegral py
        texture = sprite ^. Spriter.spriteTexture
        renderRect = SDL.Rectangle (SDL.P $ V2 x y) (V2 sw sh)
    SDL.copyEx
        textureRenderer texture Nothing (Just renderRect) (CDouble degAngle) pivot (V2 False False)

convV :: H.Vector -> SDL.Point V2 CInt
convV (H.Vector x y) = SDL.P $ V2 (floor x) (floor y)

renderShape :: MonadIO m => SDL.Renderer -> V2 CDouble -> Ptr H.Shape -> H.ShapeType -> m ()
renderShape textureRenderer (V2 camX camY) shape shapeType = do
    body <- get $ H.shapeBody shape
    pos <- get $ H.position body
    angle <- get $ H.angle body

    case shapeType of
        (H.Circle radius offset) -> do

            let (H.Vector px py) = pos + H.rotate offset (H.fromAngle angle)
                sdlPos = V2 (floor $ px - camX) (floor $ py - camY)
                edgePoint = SDL.P $ sdlPos + V2
                    (floor $ cos angle * radius)
                    (floor $ sin angle * radius)

            SDL.rendererDrawColor textureRenderer $= V4 255 255 255 255
            SDL.circle textureRenderer sdlPos (floor radius) (V4 255 255 255 255)
            SDL.drawLine textureRenderer (SDL.P sdlPos) edgePoint

        (H.LineSegment p1 p2 _) -> do
            SDL.rendererDrawColor textureRenderer $= V4 255 255 255 255
            let camV = H.Vector camX camY
            SDL.drawLine textureRenderer (convV $ p1 + pos - camV) (convV $ p2 + pos - camV)

        (H.Polygon verts _radius) -> do
            let camV = H.Vector camX camY
                rot = H.rotate $ H.fromAngle angle
                sdlVerts = map (\v -> convV $ rot v + pos - camV) $ V.toList verts
            SDL.rendererDrawColor textureRenderer $= V4 255 255 255 255

            -- Would crash if there was a polygon without vertices but that should be impossible
            let edges = zip sdlVerts $ tail sdlVerts ++ [head sdlVerts]
            forM_ edges $ uncurry (SDL.drawLine textureRenderer)
            SDL.drawPoint textureRenderer $ convV $ pos - camV
