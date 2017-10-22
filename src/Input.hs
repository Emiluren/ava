{-# LANGUAGE TemplateHaskell, GADTs #-}
module Input where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.GADT.Compare.TH
import Foreign.C.Types (CDouble(..))
import GHC.Float (float2Double)

import qualified SFML.Window as SFML

circleMass, circleRadius :: Num a => a
circleMass = 10
circleRadius = 20

isPress :: SFML.SFEvent -> Bool
isPress SFML.SFEvtKeyPressed {} = True
isPress _ = False

isRelease :: SFML.SFEvent -> Bool
isRelease SFML.SFEvtKeyReleased {} = True
isRelease _ = False

eventKeycode :: SFML.SFEvent -> SFML.KeyCode
eventKeycode = SFML.code

isKey :: SFML.KeyCode -> SFML.SFEvent -> Bool
isKey keycode = (== keycode) . eventKeycode

isKeyPressed :: SFML.KeyCode -> SFML.SFEvent -> Bool
isKeyPressed key event = isPress event && isKey key event

isCtrlKeyPressed :: SFML.KeyCode -> SFML.SFEvent -> Bool
isCtrlKeyPressed key event = isKeyPressed key event && SFML.ctrl event

wasButtonPressed :: Int -> SFML.SFEvent -> Bool
wasButtonPressed button (SFML.SFEvtJoystickButtonPressed _ b) = button == b
wasButtonPressed _ _ = False

data GamepadInput = GamepadInput
    { leftXAxis :: CDouble
    , leftYAxis :: CDouble
    , rightXAxis :: CDouble
    , yPressed :: Bool
    }

initialInput :: GamepadInput
initialInput = GamepadInput
    { leftXAxis = 0
    , leftYAxis = 0
    , rightXAxis = 0
    , yPressed = False
    }

type JoystickID = Int

data JoystickAxis = JoyLX | JoyLY | JoyRX

sfmlAxisIndex :: JoystickAxis -> Int
sfmlAxisIndex JoyLX = 0
sfmlAxisIndex JoyLY = 1
sfmlAxisIndex JoyRX = 4

-- TODO: check
padButtonA, padButtonB, padButtonX, padButtonY :: Int
padButtonA = 0
padButtonB = 1
padButtonX = 2
padButtonY = 3

padTriggerLeft, padTriggerRight, padButtonBack, padButtonStart :: Int
padTriggerLeft = 4
padTriggerRight = 5
padButtonBack = 6
padButtonStart = 7

padButtonHome, padLeftStick, padRightStick :: Int
padButtonHome = 8
padLeftStick = 9
padRightStick = 10

pollInput :: MonadIO m => Maybe JoystickID -> m GamepadInput
pollInput mGamepad =
    let deadzone v = if abs v < 0.15 then 0 else v
        getAxis g a = fmap (/100) $ liftIO $ SFML.getAxisPosition g $ sfmlAxisIndex a
    in case mGamepad of
           Nothing -> return initialInput
           Just gamepad -> do
               currentLeftXAxis <- getAxis gamepad JoyLX
               currentLeftYAxis <- getAxis gamepad JoyLY
               currentRightXAxis <- getAxis gamepad JoyRX
               currentYPressed <- liftIO $ SFML.isJoystickButtonPressed gamepad padButtonY
               return GamepadInput
                   { leftXAxis = CDouble $ float2Double $ deadzone currentLeftXAxis
                   , leftYAxis = CDouble $ float2Double $ deadzone currentLeftYAxis
                   , rightXAxis = CDouble $ float2Double $ deadzone currentRightXAxis
                   , yPressed = currentYPressed
                   }

data SfmlEventTag a where
    JoyAxisEvent :: JoystickID -> SfmlEventTag SFML.SFEvent
    JoyButtonEvent :: JoystickID -> SfmlEventTag SFML.SFEvent
    KeyEvent :: SfmlEventTag SFML.SFEvent
    OtherEvent :: SfmlEventTag SFML.SFEvent

deriveGEq ''SfmlEventTag
deriveGCompare ''SfmlEventTag
