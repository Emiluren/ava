{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, GADTs, TemplateHaskell #-}
module Main where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Lens ((^.))
import Control.Monad (unless, replicateM_)
import Control.Monad.Identity
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Ref (Ref, MonadRef)
import Control.Monad.Primitive (PrimMonad)

import Data.Bool (bool)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum ((==>), DSum(..))
import Data.Fixed (div')
import Data.Foldable (asum)
import Data.GADT.Compare.TH
import Data.IORef (newIORef, modifyIORef', readIORef, writeIORef)
import Data.Maybe (isJust, fromMaybe)
import Data.StateVar (($=), get)
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as V

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
    , ReflexHost
    , HostFrame
    )

import SDL (Point(P))
import qualified SDL
import qualified SDL.Raw.Types as SDL (JoystickID)
import qualified SDL.Image
import qualified SDL.Primitive as SDL

import qualified SpriterTypes as Spriter
import qualified SpriterBindings as Spriter

timeStep :: Double
timeStep = 1/120

shelfStart, shelfEnd :: Num a => (a, a)
shelfStart = (100, 200)
shelfEnd = (300, 320)

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

data Direction = DLeft | DRight

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

data LogicInput t = LogicInput
    { sdlEvent :: EventSelector t SdlEventTag
    , time :: Behavior t Double
    , playerOnGround :: Behavior t Bool
    , pressedKeys :: Behavior t (SDL.Scancode -> Bool)
    , mPadAxis :: Maybe (Behavior t Double)
    , mummyPosition :: Behavior t H.Vector
    , eAiTick :: Event t ()
    }

data LogicOutput t = LogicOutput
    { playerSurfaceVel :: Behavior t H.Vector
    , mummySurfaceVel :: Behavior t H.Vector
    , playerForce :: Behavior t H.Vector
    , debugRenderingEnabled :: Behavior t Bool
    , debugRenderCharacters :: Behavior t Bool
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

delayEvent :: (TriggerEvent t m, PerformEvent t m, MonadIO (Performable m)) =>
    Event t a -> Int -> m (Event t a)
delayEvent evt delay =
    let runAfterDelay x callback = liftIO $ forkIO (threadDelay delay >> callback x) >> return ()
    in performEventAsync (runAfterDelay <$> evt)

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
    withCString "Idle" $ Spriter.entityInstanceSetCurrentAnimation playerEntityInstance

    mummySpriterModel <- withCString "res/mummy/Mummy.scon"
        (Spriter.loadSpriterModel imgloader renderf)
    mummyEntityInstance <- withCString "Mummy" $ Spriter.modelGetNewEntityInstance mummySpriterModel
    withCString "Idle" $ Spriter.entityInstanceSetCurrentAnimation mummyEntityInstance

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
        characterFeetShapeType = H.Circle (characterWidth * 0.3) (H.Vector 0 $ -characterWidth * 0.2)
        characterBodyShapeType = H.Polygon (V.fromList $ reverse $ makeCharacterBody characterWidth characterHeight) 0
        characterMass = 5
        characterFeetFriction = 2

    mummyBody <- H.newBody characterMass $ 1/0
    H.spaceAddBody space mummyBody
    mummyFeetShape <- H.newShape mummyBody characterFeetShapeType
    mummyBodyShape <- H.newShape mummyBody characterBodyShapeType
    H.spaceAddShape space mummyFeetShape
    H.spaceAddShape space mummyBodyShape
    H.friction mummyFeetShape $= characterFeetFriction
    H.friction mummyBodyShape $= 0
    H.position mummyBody $= H.Vector 110 200
    --H.collisionType mummyFeetShape $= playerFeetCollisionType

    playerBody <- H.newBody characterMass (1/0)
    H.spaceAddBody space playerBody
    playerFeetShape <- H.newShape playerBody characterFeetShapeType
    playerBodyShape <- H.newShape playerBody characterBodyShapeType
    H.spaceAddShape space playerFeetShape
    H.spaceAddShape space playerBodyShape
    H.friction playerFeetShape $= characterFeetFriction
    H.friction playerBodyShape $= 0
    H.position playerBody $= H.Vector 240 100
    H.collisionType playerFeetShape $= playerFeetCollisionType

    let
        render :: MonadIO m => V2 CDouble -> V2 Double -> V2 Double -> Bool -> Bool -> m ()
        render camPos playerPos mummyPos debugRendering characterDbg = do
            SDL.rendererRenderTarget textureRenderer $= Just renderTexture
            SDL.rendererDrawColor textureRenderer $= V4 150 150 200 255
            SDL.clear textureRenderer

            liftIO $ do
                Spriter.setEntityInstancePosition playerEntityInstance ((CDouble <$> playerPos) - camPos)
                Spriter.renderEntityInstance playerEntityInstance

                Spriter.setEntityInstancePosition mummyEntityInstance ((CDouble <$> mummyPos) - camPos)
                Spriter.renderEntityInstance mummyEntityInstance

            when debugRendering $ do
                renderShape textureRenderer camPos shelf shelfShapeType
                renderShape textureRenderer camPos circleShape circleShapeType

                forM_ wallShapes $ \(shape, shapeType) ->
                    renderShape textureRenderer camPos shape shapeType

                when characterDbg $ do
                    renderShape textureRenderer camPos playerFeetShape characterFeetShapeType
                    renderShape textureRenderer camPos playerBodyShape characterBodyShapeType

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
        (aiTick, aiTickTriggerRef) <- newEventWithTriggerRef
        --(colForRightSeg, colForSegTriggerRef) <- newEventWithTriggerRef

        startTime <- SDL.time
        (gameTime, setTime) <- mutableBehavior startTime
        (playerTouchingGround, setPlayerOnGround) <- mutableBehavior False
        (keysPressed, setPressedKeys) <- mutableBehavior =<< SDL.getKeyboardState
        (mummyPos, setMummyPos) <- mutableBehavior H.zero

        playerOnGroundRef <- liftIO $ newIORef False

        joysticks <- SDL.availableJoysticks
        mGamepad <- if null joysticks
            then return Nothing
            else Just <$> SDL.openJoystick (Vector.head joysticks)

        (mAxis, mSetAxis) <- case mGamepad of
            Nothing -> return (Nothing, Nothing)
            Just _ -> (\(v, s) -> (Just v, Just s)) <$> mutableBehavior 0

        let groundCheckFilter = H.ShapeFilter
                { H.shapeFilterGroup = H.noGroup
                , H.shapeFilterCategories = H.allCategories
                , H.shapeFilterMask = H.allCategories
                }

            mainReflex :: (Ref m ~ Ref IO, ReflexHost t, MonadIO (HostFrame t), PrimMonad (HostFrame t), MonadHold t m) =>
                LogicInput t -> PerformEventT t m (LogicOutput t)
            mainReflex input = do
                let pressEvent kc = ffilter isPress $ select (sdlEvent input) (KeyEvent kc)
                    eWPressed = pressEvent SDL.KeycodeW
                    eKPressed = pressEvent SDL.KeycodeK
                    eF1Pressed = pressEvent SDL.KeycodeF1
                    eF2Pressed = pressEvent SDL.KeycodeF2
                    eAPressed = pressEvent SDL.KeycodeA
                    eDPressed = pressEvent SDL.KeycodeD
                    padButtonPress = select (sdlEvent input) (JoyButtonEvent 0)
                    padAxisMove = select (sdlEvent input) (JoyAxisEvent 0)
                    ePadAPressed = ffilter
                        (\(SDL.JoyButtonEventData _ b s) -> b == 0 && s == 1) padButtonPress
                    ePadNotCenter = ffilter
                        (\(SDL.JoyAxisEventData _ a v) -> a == 0 && v /= 0) padAxisMove
                    ePadChangeDir = (\(SDL.JoyAxisEventData _ _ v) -> if v > 0 then DRight else DLeft)
                        <$> ePadNotCenter

                    rightSideSegment pos =
                        ( pos + H.Vector characterWidth (-20)
                        , pos + H.Vector characterWidth 20
                        )
                    leftSideSegment pos =
                        ( pos + H.Vector (-characterWidth) (-20)
                        , pos + H.Vector (-characterWidth) 20
                        )

                    mummyCheckRight = rightSideSegment <$> mummyPosition input <@ eAiTick input
                    mummyCheckLeft = leftSideSegment <$> mummyPosition input <@ eAiTick input

                debugRendering <- current <$> toggle True eF1Pressed
                characterDbg <- current <$> toggle False (gate debugRendering eF2Pressed)
                playerDir <- hold DRight $ leftmost
                    [ DLeft <$ eAPressed
                    , DRight <$ eDPressed
                    , ePadChangeDir
                    ]

                colForRightSeg <- performEvent $
                    (\(s, e) -> liftIO $
                        H.spaceSegmentQueryFirst space s e characterWidth groundCheckFilter)
                    <$> mummyCheckRight
                colForLeftSeg <- performEvent $
                    (\(s, e) -> liftIO $
                        H.spaceSegmentQueryFirst space s e characterWidth groundCheckFilter)
                    <$> mummyCheckLeft

                let eMummyCanGoRight = (\sqi -> nullPtr /= H.segQueryInfoShape sqi) <$> colForRightSeg
                    eMummyCanGoLeft = (\sqi -> nullPtr /= H.segQueryInfoShape sqi) <$> colForLeftSeg

                    aPressed = ($ SDL.ScancodeA) <$> pressedKeys input
                    dPressed = ($ SDL.ScancodeD) <$> pressedKeys input
                    playerKeyMovement = controlVx 1 <$> aPressed <*> dPressed
                    playerMovement = CDouble <$> fromMaybe playerKeyMovement (mPadAxis input)
                    playerAirForce = (\d -> H.Vector (d * 1000) 0) <$> playerMovement
                    playerAcc = H.Vector <$> fmap (* playerSpeed) playerMovement <*> pure 0
                    ePlayerWantsToJump = mconcat [() <$ eWPressed, () <$ ePadAPressed]
                    ePlayerWantsToKick = mconcat [() <$ eKPressed]

                    jumpEvent = (-1000) <$ gate (playerOnGround input) ePlayerWantsToJump

                    mummySpeed AiRight = H.Vector (- playerSpeed / 4) 0
                    mummySpeed AiLeft = H.Vector (playerSpeed / 4) 0
                    mummySpeed AiStay = H.zero

                    playerSurfaceVelocity = bool
                        <$> pure (H.Vector 0 0) <*> playerAcc <*> playerOnGround input
                    playerMoving = (\(H.Vector vx _) -> abs vx > 0) <$> playerSurfaceVelocity

                    jump imp = liftIO $ H.applyImpulse playerBody (H.Vector 0 imp) (H.Vector 0 0)

                mummyWalkDirection <- foldDynMaybe
                    (\checks currentDir ->
                         asum $ fmap
                             (\(dir, canGo) ->
                                  if dir == currentDir && not canGo then
                                      Just AiStay
                                  else if currentDir == AiStay && canGo then
                                      Just dir
                                  else
                                      Nothing)
                             checks)
                    AiStay
                    $ mergeList [(,) AiLeft <$> eMummyCanGoLeft, (,) AiRight <$> eMummyCanGoRight]

                mummyDisplayDir <- hold DRight $ fmapMaybe aiDirToDirection $ updated mummyWalkDirection

                let mummySurfaceVelocity = mummySpeed <$> current mummyWalkDirection
                    mummyMoving = (\(H.Vector vx _) -> abs vx > 0) <$> mummySurfaceVelocity
                    kickDuration = 0.6
                    pickAnimation moving lastKickTime currentTime =
                        let runOrIdle = if moving then "Run" else "Idle"
                        in case lastKickTime of
                            Just t -> if currentTime - t < kickDuration then "Kick" else runOrIdle
                            Nothing -> runOrIdle

                latestPlayerKick <- hold Nothing $ Just <$> time input <@ ePlayerWantsToKick

                performEvent_ $ jump <$> jumpEvent

                return LogicOutput
                    { playerSurfaceVel = playerSurfaceVelocity
                    , mummySurfaceVel = mummySurfaceVelocity
                    , playerForce = bool
                        <$> playerAirForce <*> pure (H.Vector 0 0) <*> playerOnGround input
                    , debugRenderingEnabled = debugRendering
                    , debugRenderCharacters = characterDbg
                    , playerAnimation =
                            pickAnimation <$> playerMoving <*> latestPlayerKick <*> time input
                    , mummyAnimation = (\moving -> if moving then "Run" else "Idle") <$> mummyMoving
                    , playerDirection = playerDir
                    , mummyDirection = mummyDisplayDir
                    , quit = () <$ pressEvent SDL.KeycodeQ
                    }

        (logicOutput, FireCommand fire) <- hostPerformEventT $ mainReflex $ LogicInput
            { sdlEvent = (fan eSdlEvent)
            , time = gameTime
            , playerOnGround = playerTouchingGround
            , pressedKeys = keysPressed
            , mPadAxis = mAxis
            , mummyPosition = mummyPos
            , eAiTick = aiTick
            }

        let fireAiTick = runSpiderHost $ do
                mAsdfTrigger <- liftIO $ readIORef aiTickTriggerRef
                case mAsdfTrigger of
                    Nothing -> return []
                    Just eTrigger -> fire [eTrigger :=> Identity ()] $ return Nothing

            ticker = threadDelay (1000000 `div` 15) >> fireAiTick >> ticker

        playerArbiterCallback <- liftIO $ H.makeArbiterIterator $
            (\_ arb -> do
                    (s1, s2) <- H.arbiterGetShapes arb
                    ct1 <- get $ H.collisionType s1
                    ct2 <- get $ H.collisionType s2
                    when (ct1 == 1 && ct2 == 0) $ writeIORef playerOnGroundRef True
                    return ())

        hQuit <- subscribeEvent $ quit logicOutput

        let hSample = runHostFrame . sample

            appLoop :: Double -> Double -> SpiderHost Global ()
            appLoop oldTime timeAcc = do
                newTime <- SDL.time
                let dt = min (newTime - oldTime) 0.25
                    timeAcc' = timeAcc + dt
                    stepsToRun = timeAcc' `div'` timeStep

                liftIO $ replicateM_ stepsToRun $ H.step space $ CDouble timeStep

                fireEventRef aiTickTriggerRef ()

                onGround <- liftIO $ do
                    writeIORef playerOnGroundRef False
                    H.bodyEachArbiter playerBody playerArbiterCallback
                    readIORef playerOnGroundRef
                setPlayerOnGround onGround

                setPressedKeys =<< SDL.getKeyboardState
                setTime newTime

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
                    Spriter.entityInstanceSetCurrentAnimation playerEntityInstance

                currentMummySurfaceVel <- hSample $ mummySurfaceVel logicOutput
                H.surfaceVel mummyFeetShape $= currentMummySurfaceVel

                currentMummyAnimation <- hSample $ mummyAnimation logicOutput
                liftIO $ withCString currentMummyAnimation $
                    Spriter.entityInstanceSetCurrentAnimation mummyEntityInstance

                let spriterTimeStep = CDouble $ dt * 1000
                liftIO $ do
                    Spriter.entityInstanceSetTimeElapsed playerEntityInstance spriterTimeStep
                    Spriter.entityInstanceSetTimeElapsed mummyEntityInstance spriterTimeStep

                (H.Vector (CDouble playerX) (CDouble playerY)) <- get $ H.position playerBody
                currentMummyPos@(H.Vector (CDouble mummyX) (CDouble mummyY)) <- get $ H.position mummyBody
                setMummyPos currentMummyPos

                isDebugRenderingEnabled <- hSample $ debugRenderingEnabled logicOutput
                characterDbg <- hSample $ debugRenderCharacters logicOutput
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

                let shouldQuit = any isJust mQuit
                unless shouldQuit $
                    appLoop newTime $ timeAcc' - fromIntegral stepsToRun * timeStep

        tickerId <- liftIO $ forkIO ticker
        appLoop startTime 0.0
        liftIO $ killThread tickerId

        liftIO $ freeHaskellFunPtr playerArbiterCallback

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

renderShape :: MonadIO m => SDL.Renderer -> V2 CDouble -> (Ptr H.Shape) -> H.ShapeType -> m ()
renderShape textureRenderer (V2 camX camY) shape (H.Circle radius offset) = do
    body <- get $ H.shapeBody shape
    pos <- get $ H.position $ body
    angle <- get $ H.angle $ body

    let (H.Vector px py) = pos + H.rotate offset (H.fromAngle angle)
        sdlPos = V2 (floor $ px - camX) (floor $ py - camY)
        edgePoint = SDL.P $ sdlPos + V2
            (floor $ cos angle * radius)
            (floor $ sin angle * radius)

    SDL.rendererDrawColor textureRenderer $= V4 255 255 255 255
    SDL.circle textureRenderer sdlPos (floor radius) (V4 255 255 255 255)
    SDL.drawLine textureRenderer (SDL.P sdlPos) edgePoint
renderShape textureRenderer (V2 camX camY) shape (H.LineSegment p1 p2 _) = do
    body <- get $ H.shapeBody shape
    pos <- get $ H.position $ body
    SDL.rendererDrawColor textureRenderer $= V4 255 255 255 255
    let camV = H.Vector camX camY
    SDL.drawLine textureRenderer (convV $ p1 + pos - camV) (convV $ p2 + pos - camV)
renderShape textureRenderer (V2 camX camY) shape (H.Polygon verts _radius) = do
    body <- get $ H.shapeBody shape
    pos <- get $ H.position $ body
    angle <- get $ H.angle $ body
    let camV = H.Vector camX camY
        rot = H.rotate $ H.fromAngle angle
        sdlVerts = map (\v -> convV $ rot v + pos - camV) $ V.toList verts
    SDL.rendererDrawColor textureRenderer $= V4 255 255 255 255

    -- Would crash if there was a polygon without vertices but that should be impossible
    let edges = zip sdlVerts $ tail sdlVerts ++ [head sdlVerts]
    forM_ edges $ uncurry (SDL.drawLine textureRenderer)
    SDL.drawPoint textureRenderer $ convV $ pos - camV
