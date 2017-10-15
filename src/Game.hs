{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts #-}
module Game where

import Control.Monad (replicateM_)
import Control.Monad.Identity
import Control.Monad.IO.Class (liftIO)

import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.Bool (bool)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (find)
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
    --, physicsAiPositions :: [H.Vector]
    }

samplePhysics :: Ptr H.Body -> IO PhysicsOutput
samplePhysics playerBody = do
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
    --aiPositions <- mapM (get . H.position) characterBodies
    return PhysicsOutput
        { physicsPlayerOnGround = or playerCollisionWithGround
        , physicsPlayerPosition = playerPosition
        , physicsPlayerVelocity = playerVelocity
        --, physicsAiPositions = aiPositions
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
    let pressEvent kc = ffilter isPress $ select sdlEventFan (KeyEvent kc)
        eF1Pressed = pressEvent SDL.KeycodeF1
        eF2Pressed = pressEvent SDL.KeycodeF2
        eF3Pressed = pressEvent SDL.KeycodeF3
        eBackspacePressed = pressEvent SDL.KeycodeBackspace
        eEscPressed = pressEvent SDL.KeycodeEscape
        eEnterPressed = pressEvent SDL.KeycodeReturn

        ePadButtonPress = select sdlEventFan (JoyButtonEvent 0)
        padFilterButtonPress b = ffilter
            (\(SDL.JoyButtonEventData _ b' s) -> b == b' && s == 1) ePadButtonPress
        ePadAPressed = padFilterButtonPress padButtonA
        ePadBPressed = padFilterButtonPress padButtonB
        ePadLTPressed = padFilterButtonPress padTriggerLeft
        ePadRTPressed = padFilterButtonPress padTriggerRight
        ePadBackPressed = padFilterButtonPress padButtonBack
        ePadLeftStickPressed = padFilterButtonPress padLeftStick

    ePollInput <- tickLossy (1/15) startTime
    eCurrentInput <- performEvent $ pollInput mGamepad <$ ePollInput
    gamepadInput <- hold initialInput eCurrentInput

    debugRendering <- current <$> toggle True eF1Pressed
    characterDbg <- current <$> toggle False (gate debugRendering eF2Pressed)
    colCheckDbg <- current <$> toggle False (gate debugRendering eF3Pressed)
    dynNotEditing <- toggle True $ mconcat [ () <$ eBackspacePressed, () <$ ePadBackPressed ]

    let notEditing = current dynNotEditing

    let space = levelSpace levelLoaded
        (playerFeetShape, playerBodyShape) = playerPhysicsRefs levelLoaded
        initialShapesAndSprites = aiPhysicsRefs levelLoaded :: [(CharacterPhysicsRefs, Ptr Spriter.CEntityInstance)]

    playerBody <- get $ H.shapeBody playerFeetShape
    --aiBodies <- mapM (get . H.shapeBody) aiFeetShapes
    aiTick <- gate notEditing <$> tickLossy (1/15) startTime

    clock <- clockLossy (1/60) startTime
    clockDiffs <- mapAccum_ (\t t' -> (t', Time.diffUTCTime t' t) ) startTime $ _tickInfo_lastUTC <$> updated clock
    gameTime <- foldDyn (+) 0 $ gate notEditing clockDiffs

    let stepPhysics playerSurfaceVel aiSurfaceVels playerForce feetShapes stepsToRun = liftIO $ do
            H.surfaceVel playerFeetShape $= H.scale playerSurfaceVel (-1)
            H.force playerBody $= playerForce

            forM_ (zip feetShapes aiSurfaceVels) $ \(feetShape, surfVel) ->
                H.surfaceVel feetShape $= surfVel

            replicateM_ stepsToRun $ H.step space $ realToFrac timeStep

            samplePhysics playerBody

        initialAiShapes = fst <$> initialShapesAndSprites
        initialAiSprites = snd <$> initialShapesAndSprites

    initialPhysicsState <- liftIO $ samplePhysics playerBody

    let debugRenderSettings = DebugRenderSettings
            <$> debugRendering
            <*> characterDbg
            <*> colCheckDbg

        --initialAiPositions = physicsAiPositions initialPhysicsState

    rec
        let playerOnGround = physicsPlayerOnGround <$> physicsOutput
            playerPosition = physicsPlayerPosition <$> physicsOutput
            playerVelocity = physicsPlayerVelocity <$> physicsOutput

            aiSurfaceVelocities :: Behavior t [H.Vector]
            aiSurfaceVelocities = join $ do
                currentAiCharacters <- current aiCharacters
                return $ sequenceA $ characterSurfaceVelocity <$> currentAiCharacters
                --join . fmap characterSurfaceVelocity <$> current aiCharacters
            --aiPositions = physicsAiPositions <$> physicsOutput :: Behavior t [H.Vector]

        aiShapesAndSprites :: Behavior t [(CharacterPhysicsRefs, Ptr Spriter.CEntityInstance)] <-
            hold initialShapesAndSprites never

        let aiShapes :: Behavior t [(Ptr H.Shape, Ptr H.Shape)]
            aiShapes = fmap fst <$> aiShapesAndSprites

            aiFeetShapes :: Behavior t [Ptr H.Shape]
            aiFeetShapes = fmap fst <$> aiShapes

            aiSprites :: Behavior t [Ptr Spriter.CEntityInstance]
            aiSprites = fmap snd <$> aiShapesAndSprites

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
                aiShapes
                debugRenderSettings
                mGamepad
                playerBody
                (playerFeetShape, playerBodyShape)
                (playerSpriterInstance levelLoaded)

        let initAiCharacter :: (Ptr H.Shape, Ptr H.Shape) -> Ptr Spriter.CEntityInstance -> m (CharacterOutput t)
            initAiCharacter =
                     aiCharacterNetwork
                         startTime
                         space
                         playerBody
                         aiTick
                         (() <$ eSteppedPhysics)
                         gameTime
                         playerPosition
                         debugRenderSettings

        -- TODO: Will only work for a single character and will crash if there are none :P
        -- fix with a normal map and holdGame maybe
        (aiCharacters :: Dynamic t [CharacterOutput t]) <-
            holdGameMode
                (sequenceA $ uncurry initAiCharacter <$> zip initialAiShapes initialAiSprites)
                never

        eSteppedPhysics <- performEvent $ stepPhysics
            <$> characterSurfaceVelocity player
            <*> aiSurfaceVelocities
            <*> characterForce player
            <*> aiFeetShapes
            <@> gate notEditing eStepPhysics
        physicsOutput <- hold initialPhysicsState eSteppedPhysics

    let playerEntityInstance = playerSpriterInstance levelLoaded

    eSpriterTick <- tickLossy spriterTimeStep startTime
    eSpriterDiff <- mapAccum_ (\oldTime newTime -> (newTime, Time.diffUTCTime newTime oldTime))
        startTime $ _tickInfo_lastUTC <$> eSpriterTick
    performEvent_ $ liftIO . Spriter.setEntityInstanceTimeElapsed playerEntityInstance
        . realToFrac . (*1000) <$> gate notEditing eSpriterDiff

    let setAiDeltas currentAiSprites diff =
            forM_ currentAiSprites $ \aiEntityInstance ->
                liftIO . Spriter.setEntityInstanceTimeElapsed aiEntityInstance . realToFrac $ diff * 1000

    performEvent_ $ setAiDeltas <$> aiSprites <@> gate notEditing eSpriterDiff

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
        eDeletePressed = pressEvent SDL.KeycodeDelete
        eSPressed = pressEvent SDL.KeycodeS
        eUPressed = pressEvent SDL.KeycodeU

        editing = not <$> notEditing
        editingCommand eKey ePad = gate editing $ leftmost [ () <$ eKey, () <$ ePad ]

        eCreateNewWall = editingCommand eSpacePressed ePadAPressed
        eSnapToShape = editingCommand eUPressed ePadLTPressed
        eDeleteShape = editingCommand eDeletePressed ePadRTPressed

        cameraBehavior _ True = return $ H.toV2 <$> playerPosition
        cameraBehavior startPos False = do
            p <- sample startPos
            let deltaMovements = (^*) <$> editMoveInput <@>
                    fmap ((*400) . realToFrac) clockDiffs
            movement <- foldDyn (+) (V2 0 0) deltaMovements
            return $ pure p + current movement

        snappingDistance = 40

        findClosestShape point = liftIO $ do
            pointQueryInfo <- H.spacePointQueryNearest space (H.fromV2 point) snappingDistance groundCheckFilter
            let closestShape = H.pointQueryInfoShape pointQueryInfo
            if closestShape == nullPtr
                then return Nothing
                else return $ Just pointQueryInfo

        findCorner currentMiscRefs pointQueryInfo =
            let mShapeRefs = find (\(shape, _) -> shape == H.pointQueryInfoShape pointQueryInfo) currentMiscRefs
                point = H.pointQueryInfoPoint pointQueryInfo
            in case mShapeRefs of
                Just (_, H.LineSegment c1 c2 _) ->
                    let dc1 = H.len (point - c1)
                        dc2 = H.len (point - c2)
                    in
                        if dc1 <= dc2 && dc1 < snappingDistance then
                            H.toV2 c1
                        else if dc2 <= dc1 && dc2 < snappingDistance then
                            H.toV2 c2
                        else
                            H.toV2 point
                _ ->
                    H.toV2 point

        checkShapeForDeletion currentMiscRefs pointQueryInfo = liftIO $ do
            let mShapeRefs = find (\(shape, _) -> shape == H.pointQueryInfoShape pointQueryInfo) currentMiscRefs
            case mShapeRefs of
                Just refs@(shape, H.LineSegment s e _) -> do
                    putStrLn $ "Gonna delete " ++ show refs
                    return $ Just (shape, (s, e))
                Just refs -> do
                    putStrLn $ "Can't delete " ++ show refs ++ ". Not a wall"
                    return Nothing
                _ ->
                    return Nothing

    rec
        let findClosestShapeOn :: Event t a -> m (Event t H.PointQueryInfo)
            findClosestShapeOn event = fmapMaybe id <$> performEvent (findClosestShape <$> cameraPosition <@ event)

        eShapeToSnapTo <- findClosestShapeOn eSnapToShape
        eShapeToDelete <- findClosestShapeOn eDeleteShape

        eShapeShouldBeDeleted <- fmapMaybe id <$> performEvent (checkShapeForDeletion <$> miscRefs <@> eShapeToDelete)
        performEvent_ $ liftIO . H.spaceRemoveShape space . fst <$> eShapeShouldBeDeleted

        let updateMiscRefs ::
                Either (Ptr H.Shape) (Ptr H.Shape, H.ShapeType) ->
                [(Ptr H.Shape, H.ShapeType)] ->
                [(Ptr H.Shape, H.ShapeType)]

            updateMiscRefs (Right newWallRef) = (newWallRef :)
            updateMiscRefs (Left shapeToRemove) = filter ((/= shapeToRemove) . fst)

            eMiscUpdates = leftmost
                [ Right <$> eNewCreatedWall
                , Left . fst <$> eShapeShouldBeDeleted
                ]

        miscRefs <- current <$> foldDyn updateMiscRefs (extraPhysicsRefs levelLoaded) eMiscUpdates
        let eSnapPos = findCorner <$> miscRefs <@> eShapeToSnapTo

        newWallPos <- hold Nothing $ leftmost
            [ Nothing <$ updated dynNotEditing
            , Nothing <$ ePadBPressed
            , Nothing <$ eEscPressed
            , Just <$> cameraPosition <@ eCreateNewWall
            ]

        let maybeSpawnWall :: () -> PushM t (Maybe (H.Vector, H.Vector))
            maybeSpawnWall () = do
                mWPos <- sample newWallPos
                camPos <- sample cameraPosition
                case mWPos of
                    Nothing -> return Nothing
                    Just p -> return $ Just (H.fromV2 p, H.fromV2 camPos)

            eNewWallEdge = push maybeSpawnWall eCreateNewWall

        eNewCreatedWall <- performEvent $ liftIO . createWall space <$> eNewWallEdge

        let updateLevelData (Right wall) levelData = levelData
                { wallEdges = wall : wallEdges levelData
                }
            updateLevelData (Left wall) levelData = levelData
                { wallEdges = filter (/= wall) $ wallEdges levelData
                }
            eWallDataUpdates = leftmost
                [ Right <$> eNewWallEdge
                , Left . snd <$> eShapeShouldBeDeleted
                ]
        currentLevelData <- foldDyn updateLevelData (initialData levelLoaded) eWallDataUpdates

        cameraPosition <- switcher (H.toV2 <$> playerPosition) $ leftmost
            [ pushAlways (cameraBehavior $ fmap H.toV2 playerPosition) (updated dynNotEditing)
            , pushAlways (\p -> cameraBehavior (pure p) False) eSnapPos
            ]

    let saveLevel levelData = liftIO $ do
            putStr "File name: ./res/levels/"
            filename <- ("res/levels/" ++) <$> getLine
            BS.writeFile filename $ Aeson.encodePretty levelData
            putStrLn $ filename ++ " was saved"

        ePrintCamPos = leftmost [ () <$ eEnterPressed, () <$ ePadLeftStickPressed ]

    performEvent_ $ liftIO . print <$> (cameraPosition <@ ePrintCamPos)

    eSaveLevel <- ffilter id <$> performEvent (ctrlPressed <$ gate editing eSPressed)

    performEvent_ $ saveLevel <$> current currentLevelData <@ eSaveLevel

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

        aiRendering :: Behavior t [Renderable]
        aiRendering = do
            currentAiCharacters <- current aiCharacters
            fmap concat $ sequenceA $ characterRendering <$> currentAiCharacters

    return LogicOutput
        { cameraCenterPosition = cameraPosition
        , renderCommands =
                renderParticles
                <> renderJetpack
                <> characterRendering player
                <> aiRendering
                <> renderShapes
                <> renderInterface
        , quit = () <$ pressEvent SDL.KeycodeQ
        }
