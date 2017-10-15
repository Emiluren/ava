{-# LANGUAGE OverloadedStrings, GADTs #-}
module Main where

import qualified Control.Concurrent.Chan as Chan
import Control.Lens ((^.))
import Control.Monad (unless)
import Control.Monad.Identity
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Ref (Ref, MonadRef)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum ((==>), DSum(..))
import Data.Fixed (div')
import Data.IORef (newIORef, modifyIORef', readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.StateVar (($=), get)
import qualified Data.Time.Clock as Time
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as V

import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CInt, CFloat(..), CDouble(..))
import Foreign.Ptr (Ptr, castPtr, freeHaskellFunPtr, FunPtr)
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

import Reflex
import Reflex.Host.Class
    ( newEventWithTriggerRef
    , runHostFrame
    , fireEventRef
    , subscribeEvent
    , readEvent
    , MonadReflexHost
    )

import SDL (Point(P))
import qualified SDL
import qualified SDL.Image
import qualified SDL.Primitive as SDL

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

import Characters
import qualified ChipmunkBindings as H
import Game
import Graphics
import Input
import MonadGame
import Level

import qualified SpriterTypes as Spriter
import qualified SpriterBindings as Spriter

cameraOffset :: MonadIO m => SDL.Window -> SDL.Renderer -> m (V2 Double)
cameraOffset window textureRenderer = do
    (V2 ww wh) <- get $ SDL.windowSize window
    (V2 (CFloat rsx) (CFloat rsy)) <- get $ SDL.rendererScale textureRenderer
    return $ V2
        (fromIntegral ww / 2 / float2Double rsx)
        (fromIntegral wh / 2 / float2Double rsy)

frameTime :: Double
frameTime = 0.05

playerFrames :: Int
playerFrames = 10

newtype RenderState = RenderState
    { cameraPosition :: V2 Double
    }

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

renderTextureSize :: Num a => a
renderTextureSize = 512

mutableBehavior :: (Ref m ~ Ref IO, MonadRef m, MonadReflexHost t m)
    => a -> m (Behavior t a, a -> m ())
mutableBehavior startValue = do
    (eUpdateValue, eUpdateValueTriggerRef) <- newEventWithTriggerRef
    value <- runHostFrame $ hold startValue eUpdateValue
    return (value, fireEventRef eUpdateValueTriggerRef)

initLevel :: FunPtr Spriter.ImageLoader -> FunPtr Spriter.Renderer -> LevelData -> IO LevelLoadedData
initLevel imgloader renderf levelData = do
    putStrLn "Creating chipmunk space"
    space <- H.newSpace
    H.gravity space $= H.Vector 0 400

    playerSpriterModel <- withCString "res/princess/Princess.scon"
        (Spriter.loadSpriterModel imgloader renderf)
    playerEntityInstance <- withCString "Princess" $ Spriter.modelGetNewEntityInstance playerSpriterModel
    withCString "Idle" $ Spriter.setEntityInstanceCurrentAnimation playerEntityInstance

    mummySpriterModel <- withCString "res/mummy/Mummy.scon"
        (Spriter.loadSpriterModel imgloader renderf)

    putStrLn "Creating walls"

    wallShapes <- forM (wallEdges levelData) $ createWall space

    putStrLn "Creating misc objects"

    extraObjectRefs <- forM (extraObjects levelData) $ \(objectType, initPosition) ->
        case objectType of
            Ball -> do
                let circleMoment = H.momentForCircle circleMass (0, circleRadius) (H.Vector 0 0)
                    circleShapeType = H.Circle circleRadius H.zero

                circleBody <- H.newBody circleMass circleMoment
                H.spaceAddBody space circleBody

                circleShape <- H.newShape circleBody circleShapeType
                H.friction circleShape $= 1.0
                H.elasticity circleShape $= 0.9
                H.spaceAddShape space circleShape
                H.position circleBody $= initPosition
                return (circleShape, circleShapeType)
            Box ->
                error "Box creation not implemented!"

    putStrLn "Creating player"

    playerRefs@(playerFeetShape, _) <- makeCharacter space
    playerBody <- get $ H.shapeBody playerFeetShape
    -- H.shapeFilter playerFeetShape $= aiVisibleFilter
    -- H.shapeFilter playerBodyShape $= aiVisibleFilter

    putStrLn "Creating enemies"

    aiRefs <- forM (initEnemies levelData) $ \(enemyType, initPosition) ->
        case enemyType of
            Mummy -> do
                putStrLn $ "Creating mymmy at " ++ show initPosition
                mummyRefs@(mummyFeetShape, mummyBodyShape) <- makeCharacter space
                mummyBody <- get $ H.shapeBody mummyFeetShape
                H.position mummyBody $= initPosition
                H.collisionType mummyFeetShape $= mummyCollisionType
                H.collisionType mummyBodyShape $= mummyCollisionType
                H.shapeFilter mummyFeetShape $= aiInvisibleFilter
                H.shapeFilter mummyBodyShape $= aiInvisibleFilter

                mummyEntityInstance <- withCString "Mummy" $ Spriter.modelGetNewEntityInstance mummySpriterModel
                withCString "Idle" $ Spriter.setEntityInstanceCurrentAnimation mummyEntityInstance

                return (mummyRefs, mummyEntityInstance)
            Zombie ->
                error "Zombies not implemented!"

    H.position playerBody $= playerStartPosition levelData
    H.collisionType playerFeetShape $= playerFeetCollisionType

    putStrLn "Loaded level"

    return LevelLoadedData
        { initialData = levelData
        , playerPhysicsRefs = playerRefs
        , playerSpriterInstance = playerEntityInstance
        , aiPhysicsRefs = aiRefs
        , extraPhysicsRefs = wallShapes ++ extraObjectRefs
        , levelSpace = space
        }

mainReflex :: (MonadGame t m) =>
    FunPtr Spriter.ImageLoader ->
    FunPtr Spriter.Renderer ->
    Time.UTCTime ->
    SDL.Renderer ->
    EventSelector t SdlEventTag ->
    Event t Int ->
    Behavior t (SDL.Scancode -> Bool) ->
    Maybe SDL.Joystick ->
    m (LogicOutput t)
mainReflex imgloader renderf startTime textureRenderer sdlEventFan eStepPhysics pressedKeys mGamepad = do
    eInit <- getPostBuild

    let loadTestLevel = liftIO $ do
            -- TODO: add jetpack to 512 -111
            Just level <- (Aeson.decode :: BS.ByteString -> Maybe LevelData)
                <$> BS.readFile "res/levels/jetpack_corridor.json"
            initLevel imgloader renderf level

    eLevelLoaded <- performEvent $ loadTestLevel <$ eInit

    let initialOutput = LogicOutput
            { cameraCenterPosition = pure $ V2 0 0
            , renderCommands = pure []
            , quit = never
            }

    dGameMode <- holdGameMode
        (return initialOutput)
        (initLevelNetwork startTime textureRenderer sdlEventFan eStepPhysics pressedKeys mGamepad <$> eLevelLoaded)

    return LogicOutput
        { cameraCenterPosition = join $ current $ cameraCenterPosition <$> dGameMode
        , renderCommands = join $ current $ renderCommands <$> dGameMode
        , quit = switchPromptlyDyn $ quit <$> dGameMode
        }

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
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

    let
        render :: MonadIO m => V2 CDouble -> [Renderable] -> m ()
        render camOffset renderables = do
            SDL.rendererRenderTarget textureRenderer $= Just renderTexture
            SDL.rendererDrawColor textureRenderer $= V4 10 20 10 255
            SDL.clear textureRenderer

            liftIO $ forM_ renderables $ \renderable ->
                case renderable of
                    AnimatedSprite entityInstance animation position direction -> do
                        withCString animation $ Spriter.setEntityInstanceCurrentAnimation entityInstance
                        case direction of
                            DRight -> Spriter.setEntityInstanceScale entityInstance $ V2 1 1
                            DLeft -> Spriter.setEntityInstanceScale entityInstance $ V2 (-1) 1
                        Spriter.setEntityInstancePosition entityInstance (position - camOffset)

                        -- Sprite won't be in the right place unless it's updated
                        Spriter.setEntityInstanceTimeElapsed entityInstance 0
                        Spriter.renderEntityInstance entityInstance
                    StaticSprite texture pos angle -> do
                        textureInfo <- SDL.queryTexture texture
                        let px = SDL.textureWidth textureInfo `div` 2
                            py = SDL.textureHeight textureInfo `div` 2
                            center = SDL.P $ SDL.V2 px py
                            V2 x y = pos - camOffset
                            texSize = V2 (SDL.textureWidth textureInfo) (SDL.textureHeight textureInfo)
                            renderRect = SDL.Rectangle (SDL.P $ V2 (floor x) (floor y)) texSize
                        SDL.copyEx textureRenderer texture Nothing (Just renderRect) angle (Just center) (V2 False False)
                    Line color p1 p2 -> do
                        let (V2 x1 y1) = p1 - camOffset
                            (V2 x2 y2) = p2 - camOffset
                        SDL.rendererDrawColor textureRenderer $= color
                        SDL.drawLine textureRenderer
                            (SDL.P $ V2 (floor x1) (floor y1))
                            (SDL.P $ V2 (floor x2) (floor y2))
                    Shape shape shapeType ->
                        renderShape textureRenderer camOffset shape shapeType

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
        (eStepPhysics, stepPhysicsRef) <- newEventWithTriggerRef

        startTime <- liftIO Time.getCurrentTime

        (pressedKeys, setPressedKeys) <- mutableBehavior =<< SDL.getKeyboardState

        joysticks <- SDL.availableJoysticks
        mGamepad <- if null joysticks
            then return Nothing
            else Just <$> SDL.openJoystick (Vector.head joysticks)

        let sdlEventFan = fan eSdlEvent
            mainReflexWithParameters = mainReflex
                imgloader
                renderf
                startTime
                textureRenderer
                sdlEventFan
                eStepPhysics
                pressedKeys
                mGamepad

        eventChan <- liftIO Chan.newChan
        (logicOutput, FireCommand fire) <- hostPerformEventT $
            runTriggerEventT (runPostBuildT mainReflexWithParameters eStart) eventChan

        hQuit <- subscribeEvent $ quit logicOutput
        quitRef <- liftIO $ newIORef False

        let hSample = runHostFrame . sample

            appLoop :: Time.UTCTime -> Time.NominalDiffTime -> SpiderHost Global ()
            appLoop oldTime timeAcc = do
                newTime <- liftIO Time.getCurrentTime
                let dt = min (Time.diffUTCTime newTime oldTime) 0.25
                    timeAcc' = timeAcc + dt
                    stepsToRun = timeAcc' `div'` timeStep

                mPhysicsTrigger <- liftIO $ readIORef stepPhysicsRef
                case mPhysicsTrigger of
                    Nothing -> return ()
                    Just physicsTrigger -> do
                        lmQuit <- fire [ physicsTrigger ==> stepsToRun ] $
                            readEvent hQuit >>= sequence
                        when (any isJust lmQuit) $ liftIO $ writeIORef quitRef True

                setPressedKeys =<< SDL.getKeyboardState

                mSdlTrigger <- liftIO $ readIORef sdlTriggerRef
                case mSdlTrigger of
                    Nothing -> return ()
                    Just sdlTrigger -> do
                        events <- SDL.pollEvents
                        let triggerSdlEvent evt = sdlTrigger :=> Identity (sortEvent evt)
                        lmQuit <- fire (fmap triggerSdlEvent events) $
                            readEvent hQuit >>= sequence
                        when (any isJust lmQuit) $ liftIO $ writeIORef quitRef True

                eventTriggers <- liftIO $ Chan.readChan eventChan
                forM_ eventTriggers $ \(EventTriggerRef triggerRef :=> TriggerInvocation value onComplete) -> do
                    mTrigger <- liftIO $ readIORef triggerRef
                    lmQuit <- case mTrigger of
                        Nothing -> return []
                        Just trigger ->
                            fire [trigger :=> Identity value] $
                                readEvent hQuit >>= sequence
                    liftIO onComplete
                    when (any isJust lmQuit) $ liftIO $ writeIORef quitRef True

                renderables <- hSample $ renderCommands logicOutput
                cameraCenterPos <- hSample $ cameraCenterPosition logicOutput

                let camOffset = V2 (renderTextureSize / 2) (renderTextureSize / 2)

                render (cameraCenterPos - camOffset) renderables

                delayedEventTriggeredExit <- liftIO $ readIORef quitRef
                let shouldQuit = delayedEventTriggeredExit
                unless shouldQuit $
                    appLoop newTime $ timeAcc' - fromIntegral stepsToRun * timeStep

        mStartTrigger <- liftIO $ readIORef startTriggerRef
        _ <- case mStartTrigger of
            Nothing -> return []
            Just eTrigger -> fire [eTrigger :=> Identity ()] $ return Nothing

        appLoop startTime 0.0

    spriterImages <- readIORef loadedImages
    forM_ spriterImages $ \sprPtr -> do
        spr <- deRefStablePtr sprPtr
        let tex = spr ^. Spriter.spriteTexture
        SDL.destroyTexture tex
        freeStablePtr sprPtr

    SDL.quit

    -- free playerEntityInstance
    -- free playerSpriterModel
    -- free mummyEntityInstance
    -- free mummySpriterModel
    freeHaskellFunPtr imgloader
    freeHaskellFunPtr renderf
    -- H.freeSpace space

renderSprite :: SDL.Renderer -> Spriter.Renderer
renderSprite textureRenderer spritePtr spriteStatePtr = do
    sprite <- deRefStablePtr $ castPtrToStablePtr $ castPtr spritePtr
    spriteState <- peek spriteStatePtr

    textureInfo <- SDL.queryTexture $ sprite ^. Spriter.spriteTexture
    -- TODO: alpha is not used
    let wallShape = fromIntegral $ SDL.textureWidth textureInfo
        h = fromIntegral $ SDL.textureHeight textureInfo
        scaleX = spriteState ^. Spriter.spriteStateScale . Spriter.pointX
        scaleY = spriteState ^. Spriter.spriteStateScale . Spriter.pointY
        sw = floor $ scaleX * wallShape
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
