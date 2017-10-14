{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}
module Game where

import Control.Monad (replicateM_)
import Control.Monad.Identity
import Control.Monad.IO.Class (liftIO)

import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.Bool (bool)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Monoid ((<>))
import Data.StateVar (($=), get)
import qualified Data.Time.Clock as Time

import Foreign.C.Types (CDouble(..))
import Foreign.Ptr (Ptr, nullPtr)

import Linear (V2(..), (*^), (^*))

import Reflex
import Reflex.Time

import qualified SDL
import qualified SDL.Image

import System.Random (newStdGen, randomRs)

import qualified ChipmunkBindings as H
import Characters
import Graphics
import Input
import Level
import MonadGame
import qualified SpriterTypes as Spriter
import qualified SpriterBindings as Spriter

data LogicOutput t = LogicOutput
    { cameraCenterPosition :: Behavior t (V2 CDouble)
    , renderCommands :: Behavior t [Renderable]
    , quit :: Event t ()
    }

data LevelLoadedData = LevelLoadedData
    { initialData :: LevelData
    , playerPhysicsRefs :: CharacterPhysicsRefs
    , playerSpriterInstance :: Ptr Spriter.CEntityInstance
    , aiPhysicsRefs :: [(CharacterPhysicsRefs, Ptr Spriter.CEntityInstance)]
    , extraPhysicsRefs :: [(Ptr H.Shape, H.ShapeType)]
    , levelSpace :: Ptr H.Space
    }

timeStep :: Time.NominalDiffTime
timeStep = 1/120

spriterTimeStep :: Time.NominalDiffTime
spriterTimeStep = 1/60

data PhysicsOutput = PhysicsOutput
    { physicsPlayerOnGround :: Bool
    , physicsPlayerPosition :: H.Vector
    , physicsPlayerVelocity :: H.Vector
    , physicsAiPositions :: [H.Vector]
    }

samplePhysics :: Ptr H.Body -> [Ptr H.Body] -> IO PhysicsOutput
samplePhysics playerBody characterBodies = do
    arbiters <- H.bodyEachArbiterList playerBody
    playerCollisionWithGround <- forM arbiters $ \arb -> do
        (s1, s2) <- H.arbiterGetShapes arb
        ct1 <- get $ H.collisionType s1
        ct2 <- get $ H.collisionType s2
        let isPlayer = ct1 == playerFeetCollisionType
            onSolid = ct2 == mummyCollisionType || ct2 == wallCollisionType
        return (isPlayer && onSolid)
    playerPosition <- get $ H.position playerBody
    playerVelocity <- get $ H.velocity playerBody
    aiPositions <- mapM (get . H.position) characterBodies
    return PhysicsOutput
        { physicsPlayerOnGround = or playerCollisionWithGround
        , physicsPlayerPosition = playerPosition
        , physicsPlayerVelocity = playerVelocity
        , physicsAiPositions = aiPositions
        }

particleSpawnInterval :: Time.NominalDiffTime
particleSpawnInterval = 0.02

data Particle = Particle
    { particleVelocity :: V2 CDouble
    , particleAngularVel :: CDouble
    , particleStartTime :: Time.NominalDiffTime
    , particleLifetime :: Time.NominalDiffTime
    , particleStartPos :: V2 CDouble
    } deriving Show

particleState :: Time.NominalDiffTime -> Particle -> (V2 CDouble, CDouble)
particleState gameTime particle =
    let livedTime = gameTime - particleStartTime particle
    in ( particleStartPos particle + realToFrac livedTime *^ particleVelocity particle
       , particleAngularVel particle * realToFrac livedTime
       )

particleAlive :: Time.NominalDiffTime -> Particle -> Bool
particleAlive gameTime particle =
    let livedTime = gameTime - particleStartTime particle
    in livedTime < particleLifetime particle

createWall :: Ptr H.Space -> (H.Vector, H.Vector) -> IO (Ptr H.Shape, H.ShapeType)
createWall space (start, end) = do
    wallBody <- H.spaceGetStaticBody space
    let wst = H.LineSegment start end 1
    wallShape <- H.newShape wallBody wst
    H.collisionType wallShape $= wallCollisionType
    H.shapeFilter wallShape $= groundCheckFilter
    H.friction wallShape $= 1.0
    H.elasticity wallShape $= 0.6
    H.spaceAddShape space wallShape
    return (wallShape, wst)

initLevelNetwork :: forall t m. MonadGame t m =>
    Time.UTCTime ->
    SDL.Renderer ->
    EventSelector t SdlEventTag ->
    Event t Int ->
    Behavior t (SDL.Scancode -> Bool) ->
    Maybe SDL.Joystick ->
    LevelLoadedData ->
    m (LogicOutput t)
initLevelNetwork startTime textureRenderer sdlEventFan eStepPhysics pressedKeys mGamepad levelLoaded = do
    let space = levelSpace levelLoaded
        (playerFeetShape, playerBodyShape) = playerPhysicsRefs levelLoaded
        aiShapesAndSprites = aiPhysicsRefs levelLoaded :: [(CharacterPhysicsRefs, Ptr Spriter.CEntityInstance)]
        aiShapes = fst <$> aiShapesAndSprites
        aiFeetShapes = fst <$> aiShapes
        aiSprites = snd <$> aiShapesAndSprites

        pressEvent kc = ffilter isPress $ select sdlEventFan (KeyEvent kc)
        eF1Pressed = pressEvent SDL.KeycodeF1
        eF2Pressed = pressEvent SDL.KeycodeF2
        eF3Pressed = pressEvent SDL.KeycodeF3
        eBackspacePressed = pressEvent SDL.KeycodeBackspace
        eEscPressed = pressEvent SDL.KeycodeEscape

        ePadButtonPress = select sdlEventFan (JoyButtonEvent 0)
        padFilterButtonPress b = ffilter
            (\(SDL.JoyButtonEventData _ b' s) -> b == b' && s == 1) ePadButtonPress
        ePadAPressed = padFilterButtonPress padButtonA
        ePadBPressed = padFilterButtonPress padButtonB
        ePadLTPressed = padFilterButtonPress padTriggerLeft
        ePadBackPressed = padFilterButtonPress padButtonBack

    ePollInput <- tickLossy (1/15) startTime
    eCurrentInput <- performEvent $ pollInput mGamepad <$ ePollInput
    gamepadInput <- hold initialInput eCurrentInput

    debugRendering <- current <$> toggle True eF1Pressed
    characterDbg <- current <$> toggle False (gate debugRendering eF2Pressed)
    colCheckDbg <- current <$> toggle False (gate debugRendering eF3Pressed)
    dynNotEditing <- toggle True $ mconcat [ () <$ eBackspacePressed, () <$ ePadBackPressed ]

    let notEditing = current dynNotEditing

    playerBody <- get $ H.shapeBody playerFeetShape
    aiBodies <- mapM (get . H.shapeBody) aiFeetShapes
    aiTick <- gate notEditing <$> tickLossy (1/15) startTime

    clock <- clockLossy (1/60) startTime
    clockDiffs <- mapAccum_ (\t t' -> (t', Time.diffUTCTime t' t) ) startTime $ _tickInfo_lastUTC <$> updated clock
    gameTime <- foldDyn (+) 0 $ gate notEditing clockDiffs

    let stepPhysics playerSurfaceVel aiSurfaceVels playerForce stepsToRun = liftIO $ do
            H.surfaceVel playerFeetShape $= H.scale playerSurfaceVel (-1)
            H.force playerBody $= playerForce

            forM_ (zip aiFeetShapes aiSurfaceVels) $ \(feetShape, surfVel) ->
                H.surfaceVel feetShape $= surfVel

            replicateM_ stepsToRun $ H.step space $ realToFrac timeStep
            samplePhysics playerBody aiBodies

    initialPhysicsState <- liftIO $ samplePhysics playerBody aiBodies

    let debugRenderSettings = DebugRenderSettings
            <$> debugRendering
            <*> characterDbg
            <*> colCheckDbg

    rec
        let playerOnGround = physicsPlayerOnGround <$> physicsOutput
            playerPosition = physicsPlayerPosition <$> physicsOutput
            playerVelocity = physicsPlayerVelocity <$> physicsOutput
            aiSurfaceVelocities = sequenceA $ characterSurfaceVelocity <$> aiCharacters
            aiPositions = physicsAiPositions <$> physicsOutput :: Behavior t [H.Vector]

        (player, jetpackOn) <-
            playerNetwork
                startTime
                space
                sdlEventFan
                gameTime
                notEditing
                (bool (const False) <$> pressedKeys <*> notEditing)
                playerPosition
                playerOnGround
                (pure aiBodies)
                debugRenderSettings
                mGamepad
                playerBody
                (playerFeetShape, playerBodyShape)
                (playerSpriterInstance levelLoaded)

        -- TODO: Will only work for a single character and will crash if there are none :P
        -- fix with a normal map and holdGame maybe
        aiCharacters <-
            mapM
                (\(pos, shapes, sprite) ->
                     aiCharacterNetwork startTime space playerBody aiTick gameTime playerPosition debugRenderSettings pos shapes sprite)
                [(head <$> aiPositions, head aiShapes, head aiSprites)]

        eSteppedPhysics <- performEvent $ stepPhysics
            <$> characterSurfaceVelocity player
            <*> aiSurfaceVelocities
            <*> characterForce player
            <@> gate notEditing eStepPhysics
        physicsOutput <- hold initialPhysicsState eSteppedPhysics

    let playerEntityInstance = playerSpriterInstance levelLoaded

    eSpriterTick <- tickLossy spriterTimeStep startTime
    eSpriterDiffs <- mapAccum_ (\oldTime newTime -> (newTime, Time.diffUTCTime newTime oldTime))
        startTime $ _tickInfo_lastUTC <$> eSpriterTick
    performEvent_ $ liftIO . Spriter.setEntityInstanceTimeElapsed playerEntityInstance
        . realToFrac . (*1000) <$> gate notEditing eSpriterDiffs
    forM_ aiSprites $ \aiEntityInstance ->
        performEvent_ $ liftIO . Spriter.setEntityInstanceTimeElapsed aiEntityInstance
            . realToFrac . (*1000) <$> gate notEditing eSpriterDiffs

    jetpackTex <- SDL.Image.loadTexture textureRenderer "res/jetpack.png"
    liftIO $ putStrLn "Loaded: res/jetpack.png"
    particleTex <- SDL.Image.loadTexture textureRenderer "res/jetpack particle.png"
    liftIO $ putStrLn "Loaded: res/jetpack particle.png"

    stdGen <- liftIO newStdGen

    let particleXVelocities = randomRs (-20, 20) stdGen
        jetpackPosition = do
            pos <- playerPosition
            dir <- characterDirection player
            let jetpackOffset = case dir of
                    DLeft -> V2 3 (-25)
                    DRight -> V2 (-8) (-25)
            return $ H.toV2 pos + jetpackOffset

    eSpawnParticleTick <- gate notEditing <$> tickLossy particleSpawnInterval startTime
    eParXVel <- zipListWithEvent const particleXVelocities $
        gate jetpackOn eSpawnParticleTick

    let spawnParticle xv = do
            playerV <- sample playerVelocity
            pos <- sample jetpackPosition
            t <- sample $ current gameTime
            return Particle
                { particleVelocity = H.toV2 playerV + V2 xv 200
                , particleAngularVel = xv
                , particleStartTime = t
                , particleLifetime = realToFrac $ 0.5 + 0.05 * xv
                , particleStartPos = pos
                }
        eNewParticle = pushAlways spawnParticle eParXVel

    rec
        particles <- hold [] $ flip (:) <$> aliveParticles <@> eNewParticle
        let aliveParticles = (filter . particleAlive) <$> current gameTime <*> particles

    let aPressed = ($ SDL.ScancodeA) <$> pressedKeys
        dPressed = ($ SDL.ScancodeD) <$> pressedKeys
        keyMovementX = controlVx 1 <$> aPressed <*> dPressed
        wPressed = ($ SDL.ScancodeW) <$> pressedKeys
        sPressed = ($ SDL.ScancodeS) <$> pressedKeys
        keyMovementY = controlVx 1 <$> wPressed <*> sPressed
        ctrlPressed = do
            modState <- SDL.getModState
            return (SDL.keyModifierLeftCtrl modState || SDL.keyModifierRightCtrl modState)

        xInput = limit $ fmap leftXAxis gamepadInput + keyMovementX
        yInput = limit $ fmap leftYAxis gamepadInput + keyMovementY

        editMoveInput = V2 <$> xInput <*> yInput

        eSpacePressed = pressEvent SDL.KeycodeSpace
        eSPressed = pressEvent SDL.KeycodeS
        eUPressed = pressEvent SDL.KeycodeU
        editing = not <$> notEditing
        eCreateNewWall = gate editing $ leftmost
            [ () <$ eSpacePressed
            , () <$ ePadAPressed
            ]

        eSnapToShape = gate editing $ leftmost
            [ () <$ eUPressed
            , () <$ ePadLTPressed
            ]

        cameraBehavior _ True = return $ H.toV2 <$> playerPosition
        cameraBehavior startPos False = do
            p <- sample startPos
            let deltaMovements = (^*) <$> editMoveInput <@>
                    fmap ((*400) . realToFrac) clockDiffs
            movement <- foldDyn (+) (V2 0 0) deltaMovements
            return $ pure p + current movement

        findClosestShape point = liftIO $ do
            pointQueryInfo <- H.spacePointQueryNearest space (H.fromV2 point) 20 groundCheckFilter
            let closestShape = H.pointQueryInfoShape pointQueryInfo
            if closestShape == nullPtr
                then return Nothing
                else return $ Just $ H.toV2 $ H.pointQueryInfoPoint pointQueryInfo

    rec
        eSnapPos <- fmapMaybe id <$> performEvent (findClosestShape <$> cameraPosition <@ eSnapToShape)

        cameraPosition <- switcher (H.toV2 <$> playerPosition) $ leftmost
            [ pushAlways (cameraBehavior $ fmap H.toV2 playerPosition) (updated dynNotEditing)
            , pushAlways (\p -> cameraBehavior (pure p) False) eSnapPos
            ]

    eSaveLevel <- ffilter id <$> performEvent (ctrlPressed <$ gate editing eSPressed)

    newWallPos <- hold Nothing $ leftmost
        [ Nothing <$ updated dynNotEditing
        , Nothing <$ ePadBPressed
        , Nothing <$ eEscPressed
        , Just <$> cameraPosition <@ eCreateNewWall
        ]

    let maybeSpawnWall () = do
            mWPos <- sample newWallPos
            camPos <- sample cameraPosition
            case mWPos of
                Nothing -> return Nothing
                Just p -> return $ Just (H.fromV2 p, H.fromV2 camPos)

        eNewWallEdge = push maybeSpawnWall eCreateNewWall

    eNewCreatedWall <- performEvent $ liftIO . createWall space <$> eNewWallEdge

    let saveLevel levelData = liftIO $ do
            putStr "File name: ./res/levels/"
            filename <- ("res/levels/" ++) <$> getLine
            BS.writeFile filename $ Aeson.encodePretty levelData
            putStrLn $ filename ++ " was saved"

        addWallToLevelData wall levelData = levelData
            { wallEdges = wall : wallEdges levelData
            }
    currentLevelData <- foldDyn addWallToLevelData (initialData levelLoaded) eNewWallEdge

    performEvent_ $ saveLevel <$> current currentLevelData <@ eSaveLevel

    miscRefs <- current <$> foldDyn (:) (extraPhysicsRefs levelLoaded) eNewCreatedWall

    let renderShapes = do
            dbgRender <- debugRendering
            currentMiscRefs <- miscRefs
            if dbgRender
                then return (uncurry Shape <$> currentMiscRefs)
                else return []

        renderJetpack = do
            pos <- jetpackPosition
            anim <- characterAnimation player
            dir <- characterDirection player
            let angle =
                    if anim == "Run" then
                        if dir == DRight then 10 else -10
                    else
                        0
            return [ StaticSprite jetpackTex pos angle ]

        renderParticles = do
            pars <- aliveParticles
            currentTime <- current gameTime
            forM (particleState currentTime <$> pars) $ \(pos, angle) ->
                return $ StaticSprite particleTex pos angle

        renderCrosshair = do
            p <- cameraPosition
            let xoff = V2 5 0
                yoff = V2 0 5
            return
                [ Line white (p + xoff) (p - xoff)
                , Line white (p + yoff) (p - yoff)
                ]

        renderNewWall = do
            mWPos <- newWallPos
            camPos <- cameraPosition
            case mWPos of
                Nothing -> return []
                Just p -> return [ Line blue p camPos ]

        renderInterface = bool []
            <$> renderNewWall <> renderCrosshair
            <*> editing

    return LogicOutput
        { cameraCenterPosition = cameraPosition
        , renderCommands =
                renderParticles
                <> renderJetpack
                <> characterRendering player
                <> mconcat (fmap characterRendering aiCharacters)
                <> renderShapes
                <> renderInterface
        , quit = () <$ pressEvent SDL.KeycodeQ
        }
