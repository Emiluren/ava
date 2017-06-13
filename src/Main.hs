{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Main where

import Foreign.C.Types (CInt)
import Data.Word (Word32)
import Control.Monad.Identity
import Control.Monad.IO.Class (liftIO)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.IORef (readIORef)

import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!), Map)

import SDL.Vect (V2(..), V4(..))
import SDL (($=), Point(P))
import qualified SDL
import qualified SDL.Image

import Reflex
import Reflex.Host.Class (newEventWithTriggerRef, runHostFrame, fireEvents)

data RenderData = RenderData
    { playerPos :: V2 CInt
    , animFrame :: Int
    }

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

winSize :: V2 CInt
winSize = V2 screenWidth screenHeight

initSDL :: IO SDL.Renderer
initSDL = do
    SDL.initializeAll

    let windowSettings = SDL.defaultWindow { SDL.windowInitialSize = winSize }
    window <- SDL.createWindow "SDL Tutorial" windowSettings
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
    texSize <- textureSize texture
    let
        frameOffset = fromIntegral $ frame * 32
        sourceRect = Just $ SDL.Rectangle (P $ V2 frameOffset 0) (V2 32 32)
        destRect = Just $ SDL.Rectangle (P pos) (V2 64 64)

    SDL.copy renderer texture sourceRect destRect

isPress :: SDL.KeyboardEventData -> Bool
isPress event =
    SDL.keyboardEventKeyMotion event == SDL.Pressed

isKey :: SDL.Keycode -> SDL.KeyboardEventData -> Bool
isKey keycode event =
    SDL.keysymKeycode (SDL.keyboardEventKeysym event) == keycode

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

playerSpeed :: Float
playerSpeed = 5.0 * 100

playerVx :: Bool -> Bool -> Float
playerVx True False = -playerSpeed
playerVx False True = playerSpeed
playerVx _ _ = 0

addWithMax :: (Num a, Ord a) => a -> a -> a -> a
addWithMax maxV x y =
    if x + y > maxV then
        x + y - maxV
    else
        x + y

frameTime :: Float
frameTime = 0.05

playerFrames :: Int
playerFrames = 10

accumAnimTime :: Float -> Float -> Float
accumAnimTime = addWithMax $ frameTime * fromIntegral playerFrames

mainReflex :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
           => Event t Float
           -> Event t SDL.Event
           -> m (Behavior t RenderData)
mainReflex tickEvent sdlEvent = do
    let keyEvent = fmapMaybe getKeyEvent sdlEvent
        startPos = V2 10.0 400.0 :: V2 Float
        pressedKeyB key = hold False $ isPress <$> ffilter (isKey key) keyEvent

    aPressed <- pressedKeyB SDL.KeycodeA
    dPressed <- pressedKeyB SDL.KeycodeD

    let vx = playerVx <$> aPressed <*> dPressed
        dx = attachWith (*) vx tickEvent

    pos <- foldDyn (+) startPos $ fmap (`V2` 0) dx
    animTime <- foldDyn accumAnimTime 0 tickEvent

    let posInt = fmap floor <$> current pos
        playerAnimFrame = (\x -> floor $ x / frameTime) <$> current animTime

    return $ RenderData <$> posInt <*> playerAnimFrame

main :: IO ()
main = do
    renderer <- initSDL
    textures <- loadImages renderer ["princess_running.png"]

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
            SDL.present renderer

        appLoop ::
            IO RenderData ->
            (Float -> IO ()) ->
            (SDL.Event -> IO ()) ->
            Word32 ->
            IO ()
        appLoop renderDataCallback tickCallback sdlEventCallback oldTime = do
            events <- SDL.pollEvents
            let qPressed = any (isKeyPressed SDL.KeycodeQ) events
            mapM_ sdlEventCallback events

            renderDataCallback >>= render

            newTime <- SDL.ticks
            let dtMs = newTime - oldTime
            tickCallback $ fromIntegral dtMs / 1000

            unless qPressed $ appLoop
                renderDataCallback
                tickCallback
                sdlEventCallback
                newTime

    runSpiderHost $ do
        (tickEvent, tickTriggerRef) <- newEventWithTriggerRef
        (sdlEvent, sdlTriggerRef) <- newEventWithTriggerRef

        renderData <- mainReflex tickEvent sdlEvent
        let renderDataCallback = runSpiderHost $ runHostFrame (sample renderData)
            tickCallback dt = runSpiderHost $ handleTrigger dt tickTriggerRef
            sdlEventCallback dt = runSpiderHost $ handleTrigger dt sdlTriggerRef

        startTicks <- SDL.ticks
        liftIO $ appLoop renderDataCallback tickCallback sdlEventCallback startTicks

    destroyImages textures
