{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module SpriterTypes where

import Control.Lens.TH (makeLenses)
import Data.Map (Map)
import qualified Data.Map as Map

import Foreign.C.Types (CDouble)
import Foreign.Storable

import qualified Language.C.Inline.Cpp as C
import Language.C.Inline.Context (ctxTypesTable)
import Language.C.Types (TypeSpecifier(..))

import qualified Language.Haskell.TH as TH

import qualified SFML.Graphics as SFML

data CSpriterModel
data CEntityInstance

data SpriterPoint = SpriterPoint
    { _pointX :: Double
    , _pointY :: Double
    } deriving Show
makeLenses ''SpriterPoint

data CSpriteState = CSpriteState
    { _spriteStateAlpha :: Double
    , _spriteStatePosition :: SpriterPoint
    , _spriteStateAngle :: Double
    , _spriteStateScale :: SpriterPoint
    , _spriteStatePivot :: SpriterPoint
    } deriving Show
makeLenses ''CSpriteState

data Sprite = Sprite
    { _spriteTexture :: SFML.Sprite
    , _spritePivotX :: CDouble
    , _spritePivotY :: CDouble
    , _spriteName :: String
    }
makeLenses ''Sprite

-- TODO: alignment may be completely wrong
instance Storable SpriterPoint where
    sizeOf _ = 2 * sizeOf (undefined :: CDouble)
    alignment _ = 8
    peek ptr = SpriterPoint
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr 8
    poke ptr (SpriterPoint x y) =
        pokeByteOff ptr 0 x
        >> pokeByteOff ptr 8 y

instance Storable CSpriteState where
    sizeOf _ = 2 * sizeOf (undefined :: CDouble) +
               3 * sizeOf (undefined :: SpriterPoint)
    alignment _ = 8
    peek ptr = CSpriteState
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr 8
        <*> peekByteOff ptr 24
        <*> peekByteOff ptr 32
        <*> peekByteOff ptr 48
    poke ptr =
        pokeByteOff ptr 0
        >> pokeByteOff ptr 8
        >> pokeByteOff ptr 24
        >> pokeByteOff ptr 32
        >> pokeByteOff ptr 48

spriterCtx :: C.Context
spriterCtx = mempty
    { ctxTypesTable = spriterTypeTable
    }

spriterTypeTable :: Map TypeSpecifier TH.TypeQ
spriterTypeTable = Map.fromList
    [ (TypeName "SpriterModel", [t| CSpriterModel |])
    , (TypeName "EntityInstance", [t| CEntityInstance |])
    , (TypeName "HaskellSprite", [t| Sprite |])
    , (TypeName "SpriteState", [t| CSpriteState |])
    ]
