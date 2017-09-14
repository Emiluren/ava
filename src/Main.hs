{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecursiveDo, ScopedTypeVariables #-}
module Main where

import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad (unless)
import Control.Monad.Identity
import Control.Monad.IO.Class (liftIO)

import Data.Dependent.Sum (DSum ((:=>)))
import Data.IORef (readIORef)
import Data.Map ((!), Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.StateVar (($=), get)
import Data.Word (Word32)

import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CInt, CDouble(..))
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr, castPtr, FunPtr, freeHaskellFunPtr)
import Foreign.StablePtr
    ( newStablePtr
    , deRefStablePtr
    , castStablePtrToPtr
    , castPtrToStablePtr
    )
import Foreign.Storable

import qualified Language.C.Inline.Cpp as C

import Linear (V2(..), V4(..))

import qualified Physics.Hipmunk as H

import Reflex
import Reflex.Host.Class (newEventWithTriggerRef, runHostFrame, fireEvents)

import SDL (Point(P))
import qualified SDL
import qualified SDL.Image
import qualified SDL.Primitive as SDL

import SpriterTypes

data RenderData = RenderData
    { playerPos :: V2 CInt
    , animFrame :: Int
    }

C.context $ C.cppCtx <> C.funCtx <> spriterCtx

C.include "<spriterengine/spriterengine.h>"
C.include "<spriterengine/override/filefactory.h>"
C.include "<spriterengine/global/settings.h>"
C.include "SpriterHelpers.hpp"

C.using "namespace SpriterEngine"

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

makeLenses ''Sprite
makeLenses ''SpriterPoint
makeLenses ''CSpriteState

type SpriterFolders = Map Int (Map Int Sprite)

type ImageLoader = CString -> CDouble -> CDouble -> IO (Ptr Sprite)
type Renderer = Ptr Sprite -> Ptr CSpriteState -> IO ()

initSDL :: IO SDL.Renderer
initSDL = do
    SDL.initializeAll

    let windowSettings = SDL.defaultWindow { SDL.windowInitialSize = winSize }
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
        spacePressedE = isPress <$> ffilter (isKey SDL.KeycodeSpace) keyEvent

    leftPressed <- pressedKeyB SDL.KeycodeLeft
    rightPressed <- pressedKeyB SDL.KeycodeRight

    startTime <- sample time

    let -- vx = controlVx playerSpeed <$> aPressed <*> dPressed
        -- dx = attachWith (*) vx tickEvent

        ballAccX = controlVx 2000 <$> leftPressed <*> rightPressed
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

    -- pos <- foldDyn (+) startPos $ fmap (`V2` 0) dx

    rec
        let pos = calcPos <$> currentPlayer <*> time

            newPlayer :: SDL.KeyboardEventData -> PushM t (Maybe Player)
            newPlayer sdlE = do
                player <- sample currentPlayer
                t <- sample time
                let p = calcPos player t
                    player' = player
                        { startX = p
                        , t0 = t
                        }
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
        jumpEvent = 1000 <$ spacePressedE

    return $ (renderData, ballAcc, jumpEvent)

main :: IO ()
main = do
    renderer <- initSDL
    textures <- loadImages renderer ["princess_running.png"]

    imgloader <- $(C.mkFunPtr [t| ImageLoader |]) $ loadImage renderer
    renderf <- $(C.mkFunPtr [t| Renderer |]) $ renderSprite renderer

    [C.exp| void { Settings::setErrorFunction(Settings::simpleError); } |]
    spriterModel <- withCString "res/CharacterTest/CharacterTest.scon"
        (loadSpriterModel imgloader renderf)
    entityInstance <- withCString "Character" $ modelGetNewEntityInstance spriterModel
    withCString "Run" $ entityInstanceSetCurrentAnimation entityInstance

    putStrLn "Initializing chipmunk"
    H.initChipmunk

    putStrLn "Creating chipmunk space"
    space <- H.newSpace
    H.gravity space $= H.Vector 0 (200)

    wall <- H.newBody H.infinity H.infinity
    H.position wall $= H.Vector 0 0

    shelf <- H.newShape wall (H.LineSegment startV endV 1) (H.Vector 0 0)
    H.friction shelf $= 1.0
    H.elasticity shelf $= 0.6
    H.spaceAdd space $ H.Static shelf

    let tl = H.Vector 0 0
        bl = H.Vector 0 winHeight
        tr = H.Vector winWidth 0
        br = H.Vector winWidth winHeight

        corners = [tl, bl, br, tr]
        edges = zip corners $ tail corners ++ [head corners]

    forM_ edges $ \(start, end) -> do
        w <- H.newShape wall (H.LineSegment start end 1) (H.Vector 0 0)
        H.friction w $= 1.0
        H.spaceAdd space $ H.Static w

    let circleMoment = H.momentForCircle circleMass (0, circleRadius) (H.Vector 0 0)

    circleBody <- H.newBody circleMass circleMoment
    H.spaceAdd space circleBody

    circleShape <- H.newShape circleBody (H.Circle circleRadius) (H.Vector 0 0)
    H.friction circleShape $= 1.0
    H.spaceAdd space circleShape
    H.position circleBody $= H.Vector 200 20

    let
        princessTexture = textures ! "princess_running.png"

        handleTrigger e trigger = do
            mETrigger <- liftIO $ readIORef trigger
            case mETrigger of
                Nothing ->
                    return ()

                Just eTrigger ->
                    fireEvents [eTrigger :=> Identity e]

        render :: RenderData -> IO ()
        render (RenderData pos frame) = do
            SDL.clear renderer

            renderToPos renderer princessTexture pos frame
            [C.exp| void { $(EntityInstance* entityInstance)->render() } |]
            renderShelf renderer
            renderCircleBody renderer circleBody

            SDL.present renderer

    runSpiderHost $ do
        (sdlEvent, sdlTriggerRef) <- newEventWithTriggerRef
        (eUpdateTime, eUpdateTimeTriggerRef) <- newEventWithTriggerRef

        startTime <- SDL.time

        -- Create a behavior with the current time
        time <- runHostFrame $ do
            hold startTime eUpdateTime

        (renderData, ballForce, jumpEvent) <- mainReflex sdlEvent time
        let sampleRenderData = runSpiderHost $ runHostFrame (sample renderData)
            sampleBallForce = runSpiderHost $ runHostFrame (sample ballForce)
            sdlEventCallback evt = runSpiderHost $ handleTrigger evt sdlTriggerRef
            updateTimeCallback t = runSpiderHost $ handleTrigger t eUpdateTimeTriggerRef

            appLoop :: Double -> IO ()
            appLoop oldTime = do
                newTime <- SDL.time
                updateTimeCallback newTime

                events <- SDL.pollEvents
                let qPressed = any (isKeyPressed SDL.KeycodeQ) events
                mapM_ sdlEventCallback events

                sampleRenderData >>= render
                currentBallForce <- sampleBallForce

                H.applyOnlyForce circleBody currentBallForce (H.Vector 0 0)

                let dt = newTime - oldTime
                    spriterTimeStep = CDouble $ dt * 1000

                [C.exp| void {
                    $(EntityInstance* entityInstance)->setTimeElapsed($(double spriterTimeStep))
                } |]

                H.step space dt

                unless qPressed $ appLoop newTime

        liftIO $ appLoop startTime

    destroyImages textures
    SDL.quit

    free entityInstance
    free spriterModel
    freeHaskellFunPtr imgloader
    freeHaskellFunPtr renderf
    H.freeSpace space

loadSpriterModel :: (FunPtr ImageLoader) -> (FunPtr Renderer) -> CString -> IO (Ptr CSpriterModel)
loadSpriterModel imgloader renderer modelPath =
    [C.exp| SpriterModel*
        {
            new SpriterModel(
                $(char* modelPath),
                new SpriterFileFactory(
                        $(HaskellSprite* (*imgloader)(const char*, double, double)),
                        $(void (*renderer)(HaskellSprite*, SpriteState*))
                )
          )
        }|]

modelGetNewEntityInstance :: Ptr CSpriterModel -> CString -> IO (Ptr CEntityInstance)
modelGetNewEntityInstance model entityName =
    [C.exp| EntityInstance*
        { $(SpriterModel* model)->getNewEntityInstance($(char* entityName)) }|]

entityInstanceSetCurrentAnimation :: Ptr CEntityInstance -> CString -> IO ()
entityInstanceSetCurrentAnimation ptr animName =
    [C.exp| void
        { $(EntityInstance* ptr)->setCurrentAnimation($(char* animName)) } |]

loadImage :: SDL.Renderer -> CString -> CDouble -> CDouble -> IO (Ptr Sprite)
loadImage renderer filename pivotX pivotY = do
    name <- peekCString filename

    tex <- SDL.Image.loadTexture renderer $ name
    putStrLn $ "Loaded " ++ name

    let sprite = Sprite
            { _spriteTexture = tex
            , _spritePivotX = pivotX
            , _spritePivotY = pivotY
            , _spriteName = name
            }

    stablePtr <- newStablePtr sprite

    -- TODO: Memory leak, make sure stablePtr is freed on exit
    return $ castPtr $ castStablePtrToPtr stablePtr

renderSprite :: SDL.Renderer -> Renderer
renderSprite renderer spritePtr spriteStatePtr = do
    sprite <- deRefStablePtr $ castPtrToStablePtr $ castPtr $ spritePtr
    spriteState <- peek spriteStatePtr

    textureInfo <- SDL.queryTexture $ sprite ^. spriteTexture
    --putStrLn $ "rendering: " ++ sprite ^. spriteName
    --print spriteState
    -- TODO: Scale and alpha are not used, maybe switch to SFML
    let w = fromIntegral $ SDL.textureWidth textureInfo
        h = fromIntegral $ SDL.textureHeight textureInfo
        px = floor $ (sprite ^. spritePivotX) * fromIntegral w
        py = floor $ (sprite ^. spritePivotY) * fromIntegral h
        pivot = Just $ SDL.P $ V2 px py
        angle = spriteState ^. spriteStateAngle
        degAngle = angle * (180/pi)
        x = floor $ spriteState^.spriteStatePosition.pointX + 400 - fromIntegral px
        y = floor $ spriteState^.spriteStatePosition.pointY + 400 - fromIntegral py
        texture = sprite ^. spriteTexture
        renderRect = SDL.Rectangle (SDL.P $ V2 x y) (V2 w h)
    SDL.copyEx
        renderer texture Nothing (Just $ renderRect) (CDouble degAngle) pivot (V2 False False)

renderShelf :: SDL.Renderer -> IO ()
renderShelf renderer = do
    SDL.rendererDrawColor renderer $= V4 255 255 255 255
    SDL.drawLine renderer startP endP

renderCircleBody :: SDL.Renderer -> H.Body -> IO ()
renderCircleBody renderer ballBody = do
    H.Vector x y <- get $ H.position ballBody

    let sdlPos = V2 (floor x) (floor y)
    SDL.fillCircle renderer sdlPos circleRadius (V4 255 255 255 255)

    SDL.rendererDrawColor renderer $= V4 100 100 100 255
    angle <- get $ H.angle ballBody
    let edgePoint = SDL.P $ sdlPos + V2
            (floor $ cos angle * circleRadius)
            (floor $ sin angle * circleRadius)
    SDL.drawLine renderer (SDL.P $ sdlPos) edgePoint
