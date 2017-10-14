{-# LANGUAGE TemplateHaskell, GADTs #-}
module Input where

import Control.Monad.IO.Class (MonadIO)
import Data.Int (Int16)
import Data.GADT.Compare.TH
import Data.Word (Word8)
import Foreign.C.Types (CDouble(..))
import qualified SDL
import qualified SDL.Raw.Types as SDL (JoystickID)

padButtonA, padButtonB, padButtonX, padButtonY :: Num a => a
padButtonA = 0
padButtonB = 1
padButtonX = 2
padButtonY = 3

padTriggerLeft, padTriggerRight, padButtonBack, padButtonStart :: Num a => a
padTriggerLeft = 4
padTriggerRight = 5
padButtonBack = 6
padButtonStart = 7

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

data GamepadInput = GamepadInput
    { leftXAxis :: CDouble
    , leftYAxis :: CDouble
    , yPressed :: Bool
    }

initialInput :: GamepadInput
initialInput = GamepadInput
    { leftXAxis = 0
    , leftYAxis = 0
    , yPressed = False
    }

axisValue :: Int16 -> CDouble
axisValue v = fromIntegral v / 32768

pollInput :: MonadIO m => Maybe SDL.Joystick -> m GamepadInput
pollInput mGamepad =
    let deadzone v = if abs v < 0.15 then 0 else v
    in case mGamepad of
           Nothing -> return initialInput
           Just gamepad -> do
               currentLeftXAxis <- SDL.axisPosition gamepad 0
               currentLeftYAxis <- SDL.axisPosition gamepad 1
               currentYPressed <- SDL.buttonPressed gamepad padButtonY
               return GamepadInput
                   { leftXAxis = deadzone $ axisValue currentLeftXAxis
                   , leftYAxis = deadzone $ axisValue currentLeftYAxis
                   , yPressed = currentYPressed
                   }

data SdlEventTag a where
    ControllerDeviceEvent :: SdlEventTag SDL.ControllerDeviceEventData
    JoyAxisEvent :: SDL.JoystickID -> SdlEventTag SDL.JoyAxisEventData
    JoyButtonEvent :: SDL.JoystickID -> SdlEventTag SDL.JoyButtonEventData
    KeyEvent :: SDL.Keycode -> SdlEventTag SDL.KeyboardEventData
    OtherEvent :: SdlEventTag SDL.Event

deriveGEq ''SdlEventTag
deriveGCompare ''SdlEventTag
