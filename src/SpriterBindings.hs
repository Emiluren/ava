{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module SpriterBindings where

import qualified Language.C.Inline.Cpp as C

import Data.Map (Map)
import Data.Monoid ((<>))

import Foreign.C.String (CString)
import Foreign.C.Types (CDouble(..))
import Foreign.Ptr (Ptr, FunPtr)

import Linear (V2(..))

import SpriterTypes

type SpriterFolders = Map Int (Map Int Sprite)

type ImageLoader = CString -> CDouble -> CDouble -> IO (Ptr Sprite)
type Renderer = Ptr Sprite -> Ptr CSpriteState -> IO ()

C.context $ C.cppCtx <> C.funCtx <> spriterCtx

C.include "<spriterengine/spriterengine.h>"
C.include "<spriterengine/override/filefactory.h>"
C.include "<spriterengine/global/settings.h>"
C.include "SpriterHelpers.hpp"

C.using "namespace SpriterEngine"

loadSpriterModel :: FunPtr ImageLoader -> FunPtr Renderer -> CString -> IO (Ptr CSpriterModel)
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

makeImageLoader :: ImageLoader -> IO (FunPtr ImageLoader)
makeImageLoader = $(C.mkFunPtr [t| ImageLoader |])

makeRenderer :: Renderer -> IO (FunPtr Renderer)
makeRenderer = $(C.mkFunPtr [t| Renderer |])

setErrorFunction :: IO ()
setErrorFunction = [C.exp| void { Settings::setErrorFunction(Settings::simpleError); } |]

renderEntityInstance :: Ptr CEntityInstance -> IO ()
renderEntityInstance entityInstance = [C.exp| void { $(EntityInstance* entityInstance)->render() } |]

setEntityInstanceCurrentAnimation :: Ptr CEntityInstance -> CString -> IO ()
setEntityInstanceCurrentAnimation ptr animName =
    [C.exp| void
        { $(EntityInstance* ptr)->setCurrentAnimation($(char* animName)) } |]

setEntityInstanceTimeElapsed :: Ptr CEntityInstance -> CDouble -> IO ()
setEntityInstanceTimeElapsed entityInstance dt =
    [C.exp| void {
        $(EntityInstance* entityInstance)->setTimeElapsed($(double dt))
    } |]

setEntityInstanceCurrentTime :: Ptr CEntityInstance -> CDouble -> IO ()
setEntityInstanceCurrentTime entityInstance t =
    [C.exp| void {
        $(EntityInstance* entityInstance)->setCurrentTime($(double t))
    } |]

setEntityInstancePosition :: Ptr CEntityInstance -> V2 CDouble -> IO ()
setEntityInstancePosition entityInstance (V2 x y) =
    [C.exp| void {
        $(EntityInstance* entityInstance)->setPosition(point($(double x), $(double y)))
    }|]

setEntityInstanceScale :: Ptr CEntityInstance -> V2 CDouble -> IO ()
setEntityInstanceScale entityInstance (V2 x y) =
    [C.exp| void {
        $(EntityInstance* entityInstance)->setScale(point($(double x), $(double y)))
    }|]
