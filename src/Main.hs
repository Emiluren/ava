{-# LANGUAGE OverloadedStrings, GADTs, RecursiveDo #-}
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
import Data.Maybe (fromMaybe, isJust)
import Data.StateVar (($=), get)
import qualified Data.Time.Clock as Time

import Debug.Trace

import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CDouble(..))
import Foreign.Ptr (Ptr, castPtr, freeHaskellFunPtr, FunPtr)
import Foreign.StablePtr
    ( newStablePtr
    , deRefStablePtr
    , castStablePtrToPtr
    , castPtrToStablePtr
    , freeStablePtr
    )
import Foreign.Storable

import GHC.Float (double2Float)

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

import qualified SFML.Graphics as SFML
import qualified SFML.Window as SFML

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

cameraOffset :: SFML.Window -> IO (V2 Double)
cameraOffset window = do
    (SFML.Vec2u ww wh) <- SFML.getWindowSize window
    --(V2 (CFloat rsx) (CFloat rsy)) <- get $ SDL.rendererScale textureRenderer
    return $ V2
        (fromIntegral ww / 2)
        (fromIntegral wh / 2)

frameTime :: Double
frameTime = 0.05

playerFrames :: Int
playerFrames = 10

newtype RenderState = RenderState
    { cameraPosition :: V2 Double
    }

sortEvent :: SFML.SFEvent -> DMap SfmlEventTag Identity
sortEvent event =
    wrapInDMap $ case event of
        SFML.SFEvtJoystickMoved { SFML.joystickId = joyId } ->
            JoyAxisEvent joyId ==> event
        SFML.SFEvtJoystickButtonPressed { SFML.joystickId = joyId } ->
            JoyButtonEvent joyId ==> event
        SFML.SFEvtJoystickButtonReleased { SFML.joystickId = joyId } ->
            JoyButtonEvent joyId ==> event
        SFML.SFEvtKeyPressed {} ->
            KeyEvent ==> event
        SFML.SFEvtKeyReleased {} ->
            KeyEvent ==> event
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

initLevel :: FunPtr Spriter.ImageLoader -> FunPtr Spriter.Renderer -> LevelData -> Maybe H.Vector -> IO LevelLoadedData
initLevel imgloader renderf levelData altStartPos = do
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

    extraObjectRefs <- forM (physicalObjects levelData) $ \(objectType, initPosition) ->
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

    H.position playerBody $= fromMaybe (playerStartPosition levelData) altStartPos
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
    EventSelector t SfmlEventTag ->
    Event t Int ->
    Behavior t (SFML.KeyCode -> Bool) ->
    Maybe JoystickID ->
    m (LogicOutput t)
mainReflex imgloader renderf startTime sdlEventFan eStepPhysics pressedKeys mGamepad = do
    eInit <- getPostBuild

    let loadTestLevel levelName playerState = liftIO $ do
            putStrLn $ "Loading " ++ levelName ++ " with player state " ++ show playerState
            mLevel <- Aeson.eitherDecode <$> BS.readFile ("res/levels/" ++ levelName)
            case mLevel of
                Right level -> do
                    levelData <- initLevel imgloader renderf level $ playerStatePosition playerState
                    return (levelData, levelName, playerState)
                Left string -> error $ "Could not parse level: " ++ string

    let initialOutput = LogicOutput
            { cameraCenterPosition = pure $ V2 0 0
            , renderCommands = pure []
            , quit = never
            }

    rec
        eLevelLoaded <- performEvent $ leftmost
            [ loadTestLevel "start.json" (PlayerState False Nothing) <$ eInit
            , uncurry loadTestLevel <$> eSwitchGameMode
            ]


        dGameMode <- holdGameMode
            (return initialOutput)
            (initLevelNetwork
                startTime
                sdlEventFan
                eStepPhysics
                pressedKeys
                mGamepad
                <$> eLevelLoaded)

        let eGameModeQuit = switchPromptlyDyn $ quit <$> dGameMode

            filterSwitchMode Exit = Nothing
            filterSwitchMode (Loadlevel n ps) = Just (n, ps)
            eSwitchGameMode = fmapMaybe filterSwitchMode eGameModeQuit

    return LogicOutput
        { cameraCenterPosition = join $ current $ cameraCenterPosition <$> dGameMode
        , renderCommands = join $ current $ renderCommands <$> dGameMode
        , quit = ffilter (== Exit) eGameModeQuit
        }

pollEvents :: SFML.RenderWindow -> IO [ SFML.SFEvent ]
pollEvents window = pollEvents' [] where
    pollEvents' xs = do
        mEvent <- SFML.pollEvent window
        case mEvent of
            Nothing -> return xs
            Just x -> pollEvents' (x:xs)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    let videoMode = SFML.VideoMode
            { SFML.windowWidth = 800
            , SFML.windowHeight = 600
            , SFML.windowBPP = 32
            }

    window <- SFML.createRenderWindow videoMode "Ava" [ SFML.SFResize ] Nothing
    Right renderTexture <- SFML.createRenderTexture renderTextureSize renderTextureSize False

    -- SFML.setTexture renderTextureSprite renderTexture True

    loadedImages <- newIORef []

    let loadImage :: CString -> CDouble -> CDouble -> IO (Ptr Spriter.Sprite)
        loadImage filename pivotX pivotY = do
            name <- peekCString filename

            tex <- loadSprite name
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
    renderf <- Spriter.makeRenderer $ renderSprite renderTexture

    Spriter.setErrorFunction

    let
        render :: MonadIO m => V2 CDouble -> [Renderable] -> m ()
        render camOffset renderables = liftIO $ do
            --SDL.rendererRenderTarget softwareRenderer $= Just renderTexture
            SFML.clear renderTexture (SFML.Color 55 60 55 255)
            True <- SFML.setActive renderTexture True

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
                    StaticSprite sprite pos (CDouble angle) -> do
                        Just texture <- SFML.getTexture sprite
                        SFML.Vec2u w h <- SFML.textureSize texture
                        let px = w `div` 2
                            py = h `div` 2
                            V2 (CDouble x) (CDouble y) = pos - camOffset
                        SFML.setOrigin sprite $ SFML.Vec2f (fromIntegral px) (fromIntegral py)
                        SFML.setPosition sprite (SFML.Vec2f (double2Float x) (double2Float y))
                        SFML.setRotation sprite (double2Float angle)
                        SFML.drawSprite renderTexture sprite Nothing
                    Line color p1 p2 -> do
                        let (V2 x1 y1) = fmap (double2Float . fromCDouble) $ p1 - camOffset
                            (V2 x2 y2) = fmap (double2Float . fromCDouble) $ p2 - camOffset
                            V4 r g b a = color
                            c = SFML.Color r g b a
                        SFML.drawPrimitives renderTexture
                            [ SFML.Vertex (SFML.Vec2f x1 y1) c (SFML.Vec2f 0 0)
                            , SFML.Vertex (SFML.Vec2f x2 y2) c (SFML.Vec2f 0 0)
                            ]
                            SFML.Lines
                            Nothing
                    Shape shape shapeType ->
                        renderShape renderTexture camOffset shape shapeType

            SFML.display renderTexture
            True <- SFML.setActive renderTexture False
            --SDL.rendererRenderTarget textureRenderer $= Nothing
            --renderTexture <- SDL.createTextureFromSurface screenRenderer renderingSurface
            (SFML.Vec2u ww wh) <- SFML.getWindowSize window
            let scale = fromIntegral ww / renderTextureSize
                renderPos = SFML.Vec2f 0 $ fromIntegral wh * (1 - scale) / 2
                -- renderSize = V2 renderTextureSize dispHeight

            Right renderTextureSprite <- SFML.createSprite
            tex <- SFML.getRenderTexture renderTexture
            SFML.setTexture renderTextureSprite tex True
            SFML.setPosition renderTextureSprite renderPos
            SFML.setScale renderTextureSprite (SFML.Vec2f scale scale)

            SFML.drawSprite window renderTextureSprite Nothing
            SFML.display window
            SFML.destroy renderTextureSprite

    runSpiderHost $ do
        (eSdlEvent, sdlTriggerRef) <- newEventWithTriggerRef
        (eStart, startTriggerRef) <- newEventWithTriggerRef
        (eStepPhysics, stepPhysicsRef) <- newEventWithTriggerRef

        startTime <- liftIO Time.getCurrentTime

        let getKeyboardState = liftIO $ do
                wPressed <- SFML.isKeyPressed SFML.KeyW
                aPressed <- SFML.isKeyPressed SFML.KeyA
                sPressed <- SFML.isKeyPressed SFML.KeyS
                dPressed <- SFML.isKeyPressed SFML.KeyD
                iPressed <- SFML.isKeyPressed SFML.KeyI

                let pressed SFML.KeyW = wPressed
                    pressed SFML.KeyA = aPressed
                    pressed SFML.KeyS = sPressed
                    pressed SFML.KeyD = dPressed
                    pressed SFML.KeyI = iPressed

                return pressed

        (pressedKeys, setPressedKeys) <- mutableBehavior =<< getKeyboardState

        mGamepad <- (\b -> if b then Just 0 else Nothing) <$> liftIO (SFML.isJoystickConnected 0)

        let sdlEventFan = fan eSdlEvent
            mainReflexWithParameters = mainReflex
                imgloader
                renderf
                startTime
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

                setPressedKeys =<< getKeyboardState

                mSdlTrigger <- liftIO $ readIORef sdlTriggerRef
                case mSdlTrigger of
                    Nothing -> return ()
                    Just sdlTrigger -> do
                        events <- liftIO $ pollEvents window
                        -- let triggerSdlEvent evt = sdlTrigger :=> Identity (sortEvent evt)
                        -- lmQuit <- fire (fmap triggerSdlEvent events) $
                        --     readEvent hQuit >>= sequence
                        -- when (any isJust lmQuit) $ liftIO $ writeIORef quitRef True
                        forM_ events $ \evt -> do
                            let triggerList = [sdlTrigger :=> Identity (sortEvent evt)]
                                readPhase = readEvent hQuit >>= sequence
                            lmQuit <- fire triggerList readPhase
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
    -- forM_ spriterImages $ \sprPtr -> do
    --     spr <- deRefStablePtr sprPtr
    --     let tex = spr ^. Spriter.spriteTexture
    --     SDL.destroyTexture tex
    --     freeStablePtr sprPtr

    -- SDL.quit

    -- free playerEntityInstance
    -- free playerSpriterModel
    -- free mummyEntityInstance
    -- free mummySpriterModel
    freeHaskellFunPtr imgloader
    freeHaskellFunPtr renderf
    -- H.freeSpace space

renderSprite :: SFML.RenderTexture -> Spriter.Renderer
renderSprite renderTexture spritePtr spriteStatePtr = do
    sprite <- deRefStablePtr $ castPtrToStablePtr $ castPtr spritePtr
    spriteState <- peek spriteStatePtr

    let sfmlSprite = sprite ^. Spriter.spriteTexture
    Just texture <- SFML.getTexture sfmlSprite
    SFML.Vec2u w h <- SFML.textureSize texture
    -- TODO: alpha is not used
    let scaleX = spriteState ^. Spriter.spriteStateScale . Spriter.pointX
        scaleY = spriteState ^. Spriter.spriteStateScale . Spriter.pointY
        (CDouble px) = (sprite ^. Spriter.spritePivotX) * fromIntegral w
        (CDouble py) = (sprite ^. Spriter.spritePivotY) * fromIntegral h
        pivot = SFML.Vec2f (double2Float px) (double2Float py)
        angle = spriteState ^. Spriter.spriteStateAngle
        degAngle = angle * (180/pi)
        x = double2Float $ spriteState ^. Spriter.spriteStatePosition . Spriter.pointX
        y = double2Float $ spriteState ^. Spriter.spriteStatePosition . Spriter.pointY

    SFML.setOrigin sfmlSprite pivot
    SFML.setScale sfmlSprite (SFML.Vec2f (double2Float scaleX) (double2Float scaleY))
    SFML.setRotation sfmlSprite (double2Float degAngle)
    SFML.setPosition sfmlSprite (SFML.Vec2f x y)
    SFML.drawSprite renderTexture sfmlSprite Nothing

drawLine :: SFML.RenderTexture -> V2 Float -> V2 Float -> IO ()
drawLine renderTexture p1 p2 =
    let (V2 x1 y1) = p1
        (V2 x2 y2) = p2
    in SFML.drawPrimitives renderTexture
       [ SFML.Vertex (SFML.Vec2f x1 y1) SFML.white (SFML.Vec2f 0 0)
       , SFML.Vertex (SFML.Vec2f x2 y2) SFML.white (SFML.Vec2f 0 0)
       ] SFML.Lines Nothing

fromCDouble :: CDouble -> Double
fromCDouble (CDouble x) = x

renderShape :: MonadIO m => SFML.RenderTexture -> V2 CDouble -> Ptr H.Shape -> H.ShapeType -> m ()
renderShape renderTexture (V2 (CDouble camX) (CDouble camY)) shape shapeType = do
    body <- get $ H.shapeBody shape
    pos <- get $ H.position body
    (CDouble angle) <- get $ H.angle body

    case shapeType of
        (H.Circle (CDouble radius) offset) -> liftIO $ do

            let (H.Vector (CDouble px) (CDouble py)) = pos + H.rotate offset (H.fromAngle $ CDouble angle)
                sdlPos = SFML.Vec2f (double2Float $ px - camX) (double2Float $ py - camY)
                edgePoint = sdlPos + SFML.Vec2f
                    (double2Float $ cos angle * radius)
                    (double2Float $ sin angle * radius)
            Right circle <- SFML.createCircleShape
            SFML.setOutlineColor circle SFML.white
            SFML.setFillColor circle SFML.transparent
            SFML.drawCircle renderTexture circle Nothing

        (H.LineSegment p1 p2 _) -> do
            let camV = V2 camX camY
                (V2 x1 y1) = fmap double2Float $ (fromCDouble <$> H.toV2 (p1 + pos)) - camV
                (V2 x2 y2) = fmap double2Float $ (fromCDouble <$> H.toV2 (p2 + pos)) - camV
            liftIO $ SFML.drawPrimitives renderTexture
                [ SFML.Vertex (SFML.Vec2f x1 y1) SFML.white (SFML.Vec2f 0 0)
                , SFML.Vertex (SFML.Vec2f x2 y2) SFML.white (SFML.Vec2f 0 0)
                ]
                SFML.Lines
                Nothing

        (H.Polygon verts _radius) -> do
            -- let camV = H.Vector camX camY
            --     rot = H.rotate $ H.fromAngle angle
            --     sdlVerts = map (\v -> rot v + pos - camV) $ V.toList verts
            --SDL.rendererDrawColor textureRenderer $= V4 255 255 255 255

            -- Would crash if there was a polygon without vertices but that should be impossible
            -- let edges = zip sdlVerts $ tail sdlVerts ++ [head sdlVerts]
            -- forM_ edges $ uncurry (drawLine renderTexture)
            --SDL.drawPoint textureRenderer $ convV $ pos - camV
            liftIO $ putStrLn "Polygon rendering not implemented"
