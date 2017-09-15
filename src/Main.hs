{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables #-}
module Main where

import Control.Lens ((^.))
import Control.Monad (unless)
import Control.Monad.Identity
import Control.Monad.IO.Class (liftIO)

import Data.IORef (newIORef, modifyIORef', readIORef)
import Data.Map ((!), Map)
import qualified Data.Map as Map
import Data.StateVar (($=), get)

import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CInt, CDouble(..))
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr, castPtr, freeHaskellFunPtr)
import Foreign.StablePtr
    ( newStablePtr
    , deRefStablePtr
    , castStablePtrToPtr
    , castPtrToStablePtr
    , freeStablePtr
    )
import Foreign.Storable

import Linear (V2(..), V4(..))

import qualified Physics.Hipmunk as H

import Reflex
import Reflex.Host.Class
    ( newEventWithTriggerRef
    , runHostFrame
    , fireEventRef
    , fireEventRefAndRead
    , subscribeEvent
    )

import SDL (Point(P))
import qualified SDL
import qualified SDL.Image
import qualified SDL.Primitive as SDL

import qualified SpriterTypes as Spriter
import qualified SpriterBindings as Spriter

data RenderData = RenderData
    { playerPos :: V2 CInt
    , animFrame :: Int
    }

winWidth, winHeight :: Num a => a
winWidth = 800
winHeight = 600

winSize :: V2 CInt
winSize = V2 winWidth winHeight

timeStep :: Double
timeStep = 1/60

shelfStart, shelfEnd :: Num a => (a, a)
shelfStart = (100, 200)
shelfEnd = (400, 300)

makeHVector :: (Double, Double) -> H.Vector
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

initSDL :: IO SDL.Renderer
initSDL = do
    SDL.initializeAll

    let windowSettings = SDL.defaultWindow
            { SDL.windowInitialSize = winSize
            , SDL.windowResizable = True
            }
    window <- SDL.createWindow "Ava" windowSettings
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    SDL.rendererDrawColor renderer $= V4 50 50 50 255

    return renderer

loadImages :: SDL.Renderer -> [String] -> IO (Map String SDL.Texture)
loadImages renderer files =
    (Map.fromList . zip files) <$> textures
    where
        textures = mapM (SDL.Image.loadTexture renderer) files

destroyImages :: Map String SDL.Texture -> IO ()
destroyImages = mapM_ SDL.destroyTexture

textureSize :: SDL.Texture -> IO (V2 CInt)
textureSize texture = do
    info <- SDL.queryTexture texture
    return $ V2 (SDL.textureWidth info) (SDL.textureHeight info)

renderToPos :: SDL.Renderer -> SDL.Texture -> V2 CInt -> Int -> IO ()
renderToPos renderer texture pos frame = do
    _texSize <- textureSize texture
    let
        frameOffset = fromIntegral $ frame * 32
        sourceRect = Just $ SDL.Rectangle (P $ V2 frameOffset 0) (V2 32 32)
        destRect = Just $ SDL.Rectangle (P pos) (V2 64 64)

    SDL.copy renderer texture sourceRect destRect

isPress :: SDL.KeyboardEventData -> Bool
isPress event =
    SDL.keyboardEventKeyMotion event == SDL.Pressed

eventKeycode :: SDL.KeyboardEventData -> SDL.Keycode
eventKeycode = SDL.keysymKeycode . SDL.keyboardEventKeysym

isKey :: SDL.Keycode -> SDL.KeyboardEventData -> Bool
isKey keycode = (== keycode) . eventKeycode

getKeyEvent :: SDL.Event -> Maybe SDL.KeyboardEventData
getKeyEvent event =
    case SDL.eventPayload event of
        SDL.KeyboardEvent keyboardEvent ->
            Just keyboardEvent
        _ ->
            Nothing

isKeyPressed :: SDL.Keycode -> SDL.Event -> Bool
isKeyPressed key event =
    case SDL.eventPayload event of
        SDL.KeyboardEvent keyboardEvent ->
            isPress keyboardEvent && isKey key keyboardEvent
        _ ->
            False

playerSpeed :: Double
playerSpeed = 5.0 * 100

controlVx :: Num a => a -> Bool -> Bool -> a
controlVx x True False = -x
controlVx x False True = x
controlVx _ _ _ = 0

frameTime :: Double
frameTime = 0.05

playerFrames :: Int
playerFrames = 10

data Player = Player
    { startX :: Double
    , aPressed :: Bool
    , dPressed :: Bool
    , t0 :: Double
    }

mainReflex :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
           => Event t SDL.Event
           -> Behavior t Double
           -> m (Behavior t RenderData, Behavior t H.Vector, Event t Double)
mainReflex sdlEvent time = do
    let keyEvent = fmapMaybe getKeyEvent sdlEvent
        pressedKeyB key = hold False $ isPress <$> ffilter (isKey key) keyEvent
        spacePressedE = ffilter (\evt -> isKey SDL.KeycodeSpace evt && isPress evt) keyEvent

    leftPressed <- pressedKeyB SDL.KeycodeLeft
    rightPressed <- pressedKeyB SDL.KeycodeRight

    startTime <- sample time

    let ballAccX = controlVx 2000 <$> leftPressed <*> rightPressed
        ballAcc = H.Vector <$> ballAccX <*> pure 0

        startPlayer = Player
            { startX = 10
            , aPressed = False
            , dPressed = False
            , t0 = startTime
            }

        calcPos (Player x0 a d t0') t =
            let v = controlVx playerSpeed a d
            in x0 + (t - t0') * v

    rec
        let pos = calcPos <$> currentPlayer <*> time

            newPlayer :: SDL.KeyboardEventData -> PushM t (Maybe Player)
            newPlayer sdlE = do
                player <- sample currentPlayer
                t <- sample time
                let p = calcPos player t
                    player' = player { startX = p, t0 = t }
                case eventKeycode sdlE of
                    SDL.KeycodeA -> do
                        return $ Just $ player' { aPressed = isPress sdlE }
                    SDL.KeycodeD -> do
                        return $ Just $ player' { dPressed = isPress sdlE }
                    _ ->
                        return Nothing

        currentPlayer <- hold startPlayer $ push newPlayer keyEvent

    let posInt = V2 <$> (floor <$> pos) <*> pure 400
        playerAnimFrame = (\t -> floor (t / frameTime) `mod` playerFrames) <$> time

        renderData = RenderData <$> posInt <*> playerAnimFrame
        jumpEvent = (-2000) <$ spacePressedE

    return $ (renderData, ballAcc, jumpEvent)

main :: IO ()
main = do
    renderer <- initSDL
    textures <- loadImages renderer ["princess_running.png"]

    loadedImages <- newIORef []

    let loadImage :: CString -> CDouble -> CDouble -> IO (Ptr Spriter.Sprite)
        loadImage filename pivotX pivotY = do
            name <- peekCString filename

            tex <- SDL.Image.loadTexture renderer $ name
            putStrLn $ "Loaded " ++ name

            let sprite = Spriter.Sprite
                    { Spriter._spriteTexture = tex
                    , Spriter._spritePivotX = pivotX
                    , Spriter._spritePivotY = pivotY
                    , Spriter._spriteName = name
                    }

            stablePtr <- newStablePtr sprite

            modifyIORef' loadedImages (stablePtr:)
            return $ castPtr $ castStablePtrToPtr stablePtr

    imgloader <- Spriter.makeImageLoader $ loadImage
    renderf <- Spriter.makeRenderer $ renderSprite renderer

    Spriter.setErrorFunction
    spriterModel <- withCString "res/CharacterTest/CharacterTest.scon"
        (Spriter.loadSpriterModel imgloader renderf)
    entityInstance <- withCString "Character" $ Spriter.modelGetNewEntityInstance spriterModel
    withCString "Run" $ Spriter.entityInstanceSetCurrentAnimation entityInstance

    putStrLn "Initializing chipmunk"
    H.initChipmunk

    putStrLn "Creating chipmunk space"
    space <- H.newSpace
    H.gravity space $= H.Vector 0 (200)

    wall <- H.newBody H.infinity H.infinity
    H.position wall $= H.Vector 0 0

    let shelfShapeType = H.LineSegment startV endV 1
    shelf <- H.newShape wall shelfShapeType (H.Vector 0 0)
    H.friction shelf $= 1.0
    H.elasticity shelf $= 0.6
    H.spaceAdd space $ H.Static shelf

    let tl = H.Vector 0 0
        bl = H.Vector 0 winHeight
        tr = H.Vector winWidth 0
        br = H.Vector winWidth winHeight

        corners = [tl, bl, br, tr]
        edges = zip corners $ tail corners ++ [head corners]

    wallShapes <- forM edges $ \(start, end) -> do
        let wst = H.LineSegment start end 1
        w <- H.newShape wall wst (H.Vector 0 0)
        H.friction w $= 1.0
        H.elasticity w $= 0.6
        H.spaceAdd space $ H.Static w
        return (w, wst)

    let circleMoment = H.momentForCircle circleMass (0, circleRadius) (H.Vector 0 0)

    circleBody <- H.newBody circleMass circleMoment
    H.spaceAdd space circleBody

    let circleShapeType = H.Circle circleRadius
    circleShape <- H.newShape circleBody circleShapeType (H.Vector 0 0)
    H.friction circleShape $= 1.0
    H.elasticity circleShape $= 0.9
    H.spaceAdd space circleShape
    H.position circleBody $= H.Vector 200 20

    let
        princessTexture = textures ! "princess_running.png"

        render :: RenderData -> IO ()
        render (RenderData pos frame) = do
            SDL.rendererDrawColor renderer $= V4 0 0 0 255
            SDL.clear renderer

            renderToPos renderer princessTexture pos frame
            Spriter.renderEntityInstance entityInstance
            renderShape renderer shelf shelfShapeType
            renderShape renderer circleShape circleShapeType

            forM_ wallShapes $ \(shape, shapeType) -> do
                renderShape renderer shape shapeType

            SDL.present renderer

    runSpiderHost $ do
        (sdlEvent, sdlTriggerRef) <- newEventWithTriggerRef
        (eUpdateTime, eUpdateTimeTriggerRef) <- newEventWithTriggerRef

        startTime <- SDL.time

        -- Create a behavior with the current time
        time <- runHostFrame $ do
            hold startTime eUpdateTime

        (renderData, ballForce, jumpEvent) <- mainReflex sdlEvent time
        jumpHandle <- subscribeEvent jumpEvent

        let sampleRenderData = runSpiderHost $ runHostFrame (sample renderData)
            sampleBallForce = runSpiderHost $ runHostFrame (sample ballForce)

            appLoop :: Double -> IO ()
            appLoop oldTime = do
                newTime <- SDL.time
                runSpiderHost $ fireEventRef eUpdateTimeTriggerRef newTime

                events <- SDL.pollEvents
                let qPressed = any (isKeyPressed SDL.KeycodeQ) events
                forM_ events $ \evt -> runSpiderHost $ do
                    mJump <- fireEventRefAndRead sdlTriggerRef evt jumpHandle
                    case mJump of
                        Nothing -> return ()
                        Just imp -> liftIO $ H.applyImpulse circleBody (H.Vector 0 imp) (H.Vector 0 0)

                sampleRenderData >>= render
                currentBallForce <- sampleBallForce

                H.applyOnlyForce circleBody currentBallForce (H.Vector 0 0)

                let dt = newTime - oldTime
                    spriterTimeStep = CDouble $ dt * 1000

                Spriter.entityInstanceSetTimeElapsed entityInstance spriterTimeStep

                H.step space dt

                unless qPressed $ appLoop newTime

        liftIO $ appLoop startTime

    destroyImages textures

    spriterImages <- readIORef loadedImages
    forM_ spriterImages $ \sprPtr -> do
        spr <- deRefStablePtr sprPtr
        let tex = spr ^. Spriter.spriteTexture
        SDL.destroyTexture tex
        freeStablePtr sprPtr

    SDL.quit

    free entityInstance
    free spriterModel
    freeHaskellFunPtr imgloader
    freeHaskellFunPtr renderf
    H.freeSpace space

renderSprite :: SDL.Renderer -> Spriter.Renderer
renderSprite renderer spritePtr spriteStatePtr = do
    sprite <- deRefStablePtr $ castPtrToStablePtr $ castPtr $ spritePtr
    spriteState <- peek spriteStatePtr

    textureInfo <- SDL.queryTexture $ sprite ^. Spriter.spriteTexture
    --putStrLn $ "rendering: " ++ sprite ^. spriteName
    --print spriteState
    -- TODO: Scale and alpha are not used, maybe switch to SFML
    let w = fromIntegral $ SDL.textureWidth textureInfo
        h = fromIntegral $ SDL.textureHeight textureInfo
        px = floor $ (sprite ^. Spriter.spritePivotX) * fromIntegral w
        py = floor $ (sprite ^. Spriter.spritePivotY) * fromIntegral h
        pivot = Just $ SDL.P $ V2 px py
        angle = spriteState ^. Spriter.spriteStateAngle
        degAngle = angle * (180/pi)
        x = floor $ spriteState^.Spriter.spriteStatePosition.Spriter.pointX + 400 - fromIntegral px
        y = floor $ spriteState^.Spriter.spriteStatePosition.Spriter.pointY + 400 - fromIntegral py
        texture = sprite ^. Spriter.spriteTexture
        renderRect = SDL.Rectangle (SDL.P $ V2 x y) (V2 w h)
    SDL.copyEx
        renderer texture Nothing (Just $ renderRect) (CDouble degAngle) pivot (V2 False False)

convV :: H.Vector -> SDL.Point V2 CInt
convV (H.Vector x y) = SDL.P $ V2 (floor x) (floor y)

renderShape :: SDL.Renderer -> H.Shape -> H.ShapeType -> IO ()
renderShape renderer shape (H.Circle radius) = do
    H.Vector px py <- get $ H.position $ H.body shape
    angle <- get $ H.angle $ H.body shape

    let sdlPos = V2 (floor px) (floor py)
    SDL.fillCircle renderer sdlPos (floor radius) (V4 255 255 255 255)

    SDL.rendererDrawColor renderer $= V4 100 100 100 255
    let edgePoint = SDL.P $ sdlPos + V2
            (floor $ cos angle * radius)
            (floor $ sin angle * radius)
    SDL.drawLine renderer (SDL.P $ sdlPos) edgePoint
renderShape renderer shape (H.LineSegment p1 p2 _) = do
    pos <- get $ H.position $ H.body shape
    SDL.rendererDrawColor renderer $= V4 255 255 255 255
    SDL.drawLine renderer (convV $ p1 + pos) (convV $ p2 + pos)
renderShape renderer shape (H.Polygon verts) = do
    pos <- get $ H.position $ H.body shape
    angle <- get $ H.angle $ H.body shape
    let rot = H.rotate $ H.fromAngle angle
        verts' :: [H.Vector]
        verts' = map ((+pos) . rot) verts
        sdlVerts = map convV verts'
    SDL.rendererDrawColor renderer $= V4 255 255 255 255

    -- Would crash if there was a polygon without vertices but that should be impossible
    let edges = zip sdlVerts $ tail sdlVerts ++ [head sdlVerts]
    forM_ edges $ \(pos1, pos2) -> do
        SDL.drawLine renderer pos1 pos2
    SDL.drawPoint renderer $ convV pos
