{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts #-}
module Game where

import Control.Monad (replicateM_)
import Control.Monad.Identity
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.Bool (bool)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Foldable (asum)
import Data.List (find)
import Data.Maybe (catMaybes, isNothing, maybeToList)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.StateVar (($=), get)
import qualified Data.Time.Clock as Time

import Foreign.C.Types (CDouble(..))
import Foreign.Ptr (Ptr, nullPtr)

import Linear (V2(..), (^*))

import Reflex
import Reflex.Time

import qualified SFML.Graphics as SFML
import qualified SFML.Window as SFML

import qualified System.Console.Haskeline as HL

import qualified ChipmunkBindings as H
import Characters
import Graphics
import Input
import Level
import MonadGame
import qualified SpriterTypes as Spriter
import qualified SpriterBindings as Spriter

-- TODO: Maybe add health here (right now you respawn at the same level with full health though so it wouldn't matter)
data PlayerState = PlayerState
    { playerStartsWithJetpack :: Bool
    , playerStatePosition :: Maybe H.Vector
    } deriving (Eq, Show)
data QuitData = Exit | Loadlevel String PlayerState deriving (Eq, Show)

data LogicOutput t = LogicOutput
    { cameraCenterPosition :: Behavior t (V2 CDouble)
    , renderCommands :: Behavior t [Renderable]
    , logicPlayerHealth :: Behavior t Double
    , quit :: Event t QuitData
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
    return PhysicsOutput
        { physicsPlayerOnGround = or playerCollisionWithGround
        , physicsPlayerPosition = playerPosition
        , physicsPlayerVelocity = playerVelocity
        }

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

loadSprite :: MonadIO m => FilePath -> m (Maybe SFML.Sprite)
loadSprite filepath = liftIO $ do
    mtex <- SFML.textureFromFile filepath Nothing
    mspr <- SFML.createSprite
    case (mtex, mspr) of
        (Right texture, Right sprite) ->
            SFML.setTexture sprite texture True
            >> return (Just sprite)
        _ -> return Nothing

mseq :: Monad m => m (Maybe a) -> m (Maybe b) -> (a -> b -> c) -> m (Maybe c)
mseq act1 act2 comb = do
    mres1 <- act1
    case mres1 of
        Nothing -> return Nothing
        Just res1 -> do
            mres2 <- act2
            case mres2 of
                Nothing -> return Nothing
                Just res2 -> return $ Just $ comb res1 res2

initLevelNetwork :: forall t m. MonadGame t m =>
    Time.UTCTime ->
    EventSelector t SfmlEventTag ->
    Event t Int ->
    Behavior t (SFML.KeyCode -> Bool) ->
    Maybe JoystickID ->
    (LevelLoadedData, String, PlayerState) ->
    m (LogicOutput t)
initLevelNetwork startTime sfmlEventFan eStepPhysics pressedKeys mGamepad (levelLoaded, levelName, initialPlayerState) = do
    let pressEvent kc = ffilter (isKeyPressed kc) $ select sfmlEventFan KeyEvent
        ctrlPressEvent kc = ffilter (isCtrlKeyPressed kc) $ select sfmlEventFan KeyEvent
        eF1Pressed = pressEvent SFML.KeyF1
        eF2Pressed = pressEvent SFML.KeyF2
        eF3Pressed = pressEvent SFML.KeyF3
        eBackspacePressed = pressEvent SFML.KeyBack
        eEscPressed = pressEvent SFML.KeyEscape
        eEnterPressed = pressEvent SFML.KeyReturn

        padFilterButtonPress b = ffilter (wasButtonPressed b) $
            select sfmlEventFan (JoyButtonEvent 0)
        ePadAPressed = padFilterButtonPress padButtonA
        ePadBPressed = padFilterButtonPress padButtonB
        ePadYPressed = padFilterButtonPress padButtonY
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
    dynEditing <- toggle False $ mconcat [ () <$ eBackspacePressed, () <$ ePadBackPressed ]

    let editing = current dynEditing
        notEditing = not <$> editing
        eExitEditMode = ffilter not $ updated dynEditing

    let space = levelSpace levelLoaded
        (playerFeetShape, playerBodyShape) = playerPhysicsRefs levelLoaded
        initialShapesAndSprites = aiPhysicsRefs levelLoaded :: [(CharacterPhysicsRefs, Ptr Spriter.CEntityInstance)]

    playerBody <- get $ H.shapeBody playerFeetShape
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

    Just jetpackTex <- loadSprite "res/jetpack.png"
    liftIO $ putStrLn "Loaded: res/jetpack.png"
    Just particleTex <- loadSprite "res/jetpack particle.png"
    liftIO $ putStrLn "Loaded: res/jetpack particle.png"

    initialImages <- liftIO $ do
        let images = levelBackgroundImages $ initialData levelLoaded
            filenames = (\(x, _, _) -> x) <$> images
        sprites <- forM filenames $ \file -> do
            msprite <- loadSprite $ "res/background_images/" ++ file
            when (isNothing msprite) $ putStrLn $ "Could not load " ++ file
            return $ (,) <$> Just file <*> msprite
        return $ Map.fromList $ catMaybes sprites

    let eQueryForImageName = gate editing $ ctrlPressEvent SFML.KeyI
        performGetLine = liftIO . HL.runInputT HL.defaultSettings . HL.getInputLine
        askForImage = performGetLine "Image: "
        editTurnInput = limit $ fmap rightXAxis gamepadInput -- + keyTurn

    eChosenImage <- fmapMaybe id <$> performEvent (askForImage <$ eQueryForImageName)
    let eSetGhost = leftmost
            [ (\name -> Just (name, 0)) <$> eChosenImage
            , Nothing <$ eEscPressed
            , Nothing <$ ePadBPressed
            , Nothing <$ eExitEditMode
            ]
        updateAngle _ _ Nothing = Nothing
        updateAngle turn dt (Just (img, angle)) =
            Just (img, angle + turn * dt)

    ghostImage <- foldDyn ($) Nothing $ leftmost
        [ const <$> eSetGhost
        , updateAngle <$> editTurnInput <@> fmap ((*400) . realToFrac) clockDiffs
        ]

    rec
        let eLoadNewImage = fmapMaybe id $
                (\db n -> if Map.member n db then Nothing else Just n)
                <$> current loadedImages
                <@> eChosenImage

            loadBackgroundImage :: String -> Performable m (Maybe (String, SFML.Sprite))
            loadBackgroundImage name = liftIO $ do
                putStrLn $ "loading " ++ name
                let filepath = "res/background_images/" ++ name
                msprite <- loadSprite filepath
                case msprite of
                    Just sprite -> return $ Just (name, sprite)
                    Nothing -> putStrLn ("Could not load: " ++ filepath) >> return Nothing

        eLoadedImage <- fmapMaybe id <$> performEvent (loadBackgroundImage <$> eLoadNewImage)

        let updateImages = uncurry Map.insert
        loadedImages <- foldDyn updateImages initialImages eLoadedImage

    let mJetpackPosition = snd <$> find (\(t, _) -> t == Jetpack) (pickupableObjects $ initialData levelLoaded)

    rec
        let playerOnGround = physicsPlayerOnGround <$> physicsOutput
            playerPosition = physicsPlayerPosition <$> physicsOutput
            playerVelocity = physicsPlayerVelocity <$> physicsOutput

            isPlayerTouchingJetpack :: Time.NominalDiffTime -> PushM t (Maybe ())
            isPlayerTouchingJetpack _ = do
                playerPos <- sample playerPosition
                return $ case mJetpackPosition of
                    Just p | near playerPos p -> Just ()
                    _ -> Nothing
                where near pp jp = H.len (pp - H.Vector 0 20 - jp) < 20

            aiSurfaceVelocities :: Behavior t [H.Vector]
            aiSurfaceVelocities = join $ do
                currentAiCharacters <- current aiCharacters
                return $ sequenceA $ characterSurfaceVelocity <$> currentAiCharacters

        let aiShapesAndSprites = current $ fmap characterRefsAndSprite <$> aiCharacters

            aiShapes :: Behavior t [(Ptr H.Shape, Ptr H.Shape)]
            aiShapes = fmap fst <$> aiShapesAndSprites

            aiFeetShapes :: Behavior t [Ptr H.Shape]
            aiFeetShapes = fmap fst <$> aiShapes

            aiSprites :: Behavior t [Ptr Spriter.CEntityInstance]
            aiSprites = fmap snd <$> aiShapesAndSprites

            ePickUpJetpack = push isPlayerTouchingJetpack $ updated gameTime
            ePlayerHurt = switchPromptlyDyn $ fmap leftmost $ fmap characterAttack <$> aiCharacters

        playerHasJetpack <- hold (playerStartsWithJetpack initialPlayerState) $ True <$ ePickUpJetpack

        player <-
            playerNetwork
                startTime
                space
                sfmlEventFan
                (snd <$> ePlayerHurt)
                gameTime
                playerHasJetpack
                notEditing
                (bool (const False) <$> pressedKeys <*> notEditing)
                (playerPosition, playerVelocity)
                playerOnGround
                aiShapes
                debugRenderSettings
                mGamepad
                playerBody
                (playerFeetShape, playerBodyShape)
                (playerSpriterInstance levelLoaded)
                (jetpackTex, particleTex)

        let initAiCharacter :: (Ptr H.Shape, Ptr H.Shape) -> Ptr Spriter.CEntityInstance -> m (CharacterOutput t)
            ePlayerAttack = characterAttack player
            initAiCharacter =
                     aiCharacterNetwork
                         space
                         playerBody
                         aiTick
                         (() <$ eSteppedPhysics)
                         ePlayerAttack
                         gameTime
                         playerPosition
                         debugRenderSettings

            thisCharacterDied character =
                ffilter (<= 0) $ updated $ characterHealth character
            eACharacterDied = switchPromptlyDyn $ leftmost . fmap thisCharacterDied <$> aiCharacters

            removeCharacter :: CharacterOutput t -> IO ()
            removeCharacter character = do
                let ((feetShape, bodyShape), sprite) = characterRefsAndSprite character
                body <- get $ H.shapeBody feetShape
                putStr "Removing "
                print (feetShape, bodyShape, body)
                H.spaceRemoveShape space feetShape
                H.spaceRemoveShape space bodyShape
                H.spaceRemoveBody space body
                -- TODO: release allocated memory

            removeDeadCharacters :: [ CharacterOutput t ] -> [ Int ] -> Performable m [ CharacterOutput t ]
            removeDeadCharacters currentCharacters currentHealths =
                fmap catMaybes $ forM (zip currentCharacters currentHealths) $ \(character, health) ->
                    if health > 0
                        then return $ Just character
                        else liftIO (removeCharacter character) >> return Nothing

            characterHealths :: Behavior t [ Int ]
            characterHealths = current $ join $ fmap sequenceA $ fmap characterHealth <$> aiCharacters

        initialCharacters <- sequenceA $
                uncurry initAiCharacter <$> zip initialAiShapes initialAiSprites

        eDeadCharactersRemoved <- performEvent $
            removeDeadCharacters <$> current aiCharacters <*> characterHealths <@ eACharacterDied

        (aiCharacters :: Dynamic t [CharacterOutput t]) <-
            holdDyn initialCharacters eDeadCharactersRemoved

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

    let aPressed = ($ SFML.KeyA) <$> pressedKeys
        dPressed = ($ SFML.KeyD) <$> pressedKeys
        keyMovementX = controlVx 1 <$> aPressed <*> dPressed
        wPressed = ($ SFML.KeyW) <$> pressedKeys
        sPressed = ($ SFML.KeyS) <$> pressedKeys
        keyMovementY = controlVx 1 <$> wPressed <*> sPressed

        xInputLeft = limit $ fmap leftXAxis gamepadInput + keyMovementX
        yInputLeft = limit $ fmap leftYAxis gamepadInput + keyMovementY

        editMoveInput = V2 <$> xInputLeft <*> yInputLeft

        eSpacePressed = pressEvent SFML.KeySpace
        eDeletePressed = pressEvent SFML.KeyDelete
        eUPressed = pressEvent SFML.KeyU
        editingCommand eKey ePad = gate editing $ leftmost [ () <$ eKey, () <$ ePad ]

        eCreateNewWall = gate (isNothing <$> current ghostImage) $
            editingCommand eSpacePressed ePadAPressed
        eSnapToShape = editingCommand eUPressed ePadLTPressed
        eDeleteShape = editingCommand eDeletePressed ePadRTPressed

        cameraBehavior _ False = return $ H.toV2 <$> playerPosition
        cameraBehavior startPos True = do
            p <- sample startPos
            let deltaMovements = (^*) <$> editMoveInput <@>
                    fmap ((*400) . realToFrac) clockDiffs
            movement <- foldDyn (+) (V2 0 0) deltaMovements
            return $ pure p + current movement

        snappingDistance = 40

        findClosestShape point = liftIO $ do
            pointQueryInfo <-
                H.spacePointQueryNearest space (H.fromV2 point) snappingDistance groundCheckFilter
            let closestShape = H.pointQueryInfoShape pointQueryInfo
            if closestShape == nullPtr
                then return Nothing
                else return $ Just pointQueryInfo

        findCorner currentMiscRefs pointQueryInfo =
            let mShapeRefs =
                    find (\(shape, _) -> shape == H.pointQueryInfoShape pointQueryInfo) currentMiscRefs
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
            let mShapeRefs =
                    find (\(shape, _) -> shape == H.pointQueryInfoShape pointQueryInfo) currentMiscRefs
            case mShapeRefs of
                Just refs@(shape, H.LineSegment s e _) -> do
                    putStrLn $ "Gonna delete " ++ show refs
                    return $ Just (shape, (s, e))
                Just refs -> do
                    putStrLn $ "Can't delete " ++ show refs ++ ". Not a wall"
                    return Nothing
                _ ->
                    return Nothing

    let addWall wall levelData = levelData
            { wallEdges = wall : wallEdges levelData
            }
        removeWall wall levelData = levelData
            { wallEdges = filter (/= wall) $ wallEdges levelData
            }
        addBackgroundImage img levelData = levelData
            { levelBackgroundImages = img : levelBackgroundImages levelData
            }
        removeBackgroundImage img levelData = levelData
            { levelBackgroundImages = filter (/= img) $ levelBackgroundImages levelData
            }

    rec
        let findClosestShapeOn :: Event t a -> m (Event t H.PointQueryInfo)
            findClosestShapeOn event = fmapMaybe id <$>
                performEvent (findClosestShape <$> cameraPosition <@ event)

        eShapeToSnapTo <- findClosestShapeOn eSnapToShape
        eShapeToDelete <- findClosestShapeOn eDeleteShape

        eShapeShouldBeDeleted <- fmapMaybe id <$>
            performEvent (checkShapeForDeletion <$> miscRefs <@> eShapeToDelete)
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
            [ Nothing <$ eExitEditMode
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

        let ePlaceImage = push (\_ -> sample $ current ghostImage) $
                leftmost [ () <$ ePadAPressed, () <$ eSpacePressed ]

            eAddNewImage = (\pos (name, angle) -> (name, H.fromV2 pos, angle))
                <$> cameraPosition <@> ePlaceImage

            imageRemoveRange = 100

            findImageToRemove :: SFML.SFEvent -> PushM t (Maybe (String, H.Vector, H.CpFloat))
            findImageToRemove _ = do
                camPos <- H.fromV2 <$> sample cameraPosition
                let pickClosest img@(_, imgPos, _) old =
                        let imgDist = H.len (camPos - imgPos)
                        in if imgDist > imageRemoveRange then
                            old
                        else
                            case old of
                                Nothing -> Just img
                                Just (_, oldPos, _) ->
                                    if H.len (camPos - oldPos) > imgDist then
                                        Just img
                                    else
                                        old
                images <- sample $ levelBackgroundImages <$> current currentLevelData
                return $ foldr pickClosest Nothing images

            eLevelDataUpdates = mergeWith (.)
                [ addWall <$> eNewWallEdge
                , removeWall . snd <$> eShapeShouldBeDeleted
                , addBackgroundImage <$> eAddNewImage
                , removeBackgroundImage <$> push findImageToRemove ePadYPressed
                ]
        currentLevelData <- foldDyn ($) (initialData levelLoaded) eLevelDataUpdates

        cameraPosition <- switcher (H.toV2 <$> playerPosition) $ leftmost
            [ pushAlways (cameraBehavior $ fmap H.toV2 playerPosition) (updated dynEditing)
            , pushAlways (\p -> cameraBehavior (pure p) True) eSnapPos
            ]

    let saveLevel levelData = liftIO $ HL.runInputT HL.defaultSettings $ do
            liftIO $ putStr "File name: ./res/levels/"
            mFilename <- HL.getInputLine "> "
            case mFilename of
                Nothing -> return ()
                Just filename -> liftIO $ do
                    BS.writeFile ("res/levels/" ++ filename) $ Aeson.encodePretty levelData
                    putStrLn $ filename ++ " was saved"

        ePrintCamPos = leftmost [ () <$ eEnterPressed, () <$ ePadLeftStickPressed ]

    performEvent_ $ liftIO . print <$> (cameraPosition <@ ePrintCamPos)

    let eSaveLevel = gate editing $ ctrlPressEvent SFML.KeyS

    performEvent_ $ saveLevel <$> current currentLevelData <@ eSaveLevel

    let renderShapes = do
            dbgRender <- debugRendering
            currentMiscRefs <- miscRefs
            if dbgRender
                then return (uncurry Shape <$> currentMiscRefs)
                else return []

        renderBackground = do
            imageDb <- current loadedImages
            images <- levelBackgroundImages <$> current currentLevelData
            let spriteForPath (imgfile, pos, angle) =
                    (\s -> StaticSprite s (H.toV2 pos) angle 1)
                    <$> Map.lookup imgfile imageDb
            return $ fmapMaybe spriteForPath images

        renderGhost = do
            imageDb <- current loadedImages
            mghost <- current ghostImage
            pos <- cameraPosition

            let spriteForPath (imgfile, angle) = (\s -> StaticSprite s pos angle 0.8)
                    <$> Map.lookup imgfile imageDb

            return $ maybeToList $ mghost >>= spriteForPath

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
        aiRendering =
            join $ fmap mconcat $ current $ fmap characterRendering <$> aiCharacters

        renderJetpack :: Behavior t [Renderable]
        renderJetpack = do
            hasJet <- playerHasJetpack
            gt <- current gameTime
            let float = V2 0 $ 5 * sin (2 * realToFrac gt)
            case mJetpackPosition of
                Just pos | not hasJet ->
                    return [ StaticSprite jetpackTex (H.toV2 pos + float) 30 1 ]
                _ -> return []

    let eRPressed = pressEvent SFML.KeyR
        eQPressed = pressEvent SFML.KeyQ
        promptForLevel = liftIO $ HL.runInputT HL.defaultSettings $
            mseq
                (HL.getInputLine "Level to load: ")
                (HL.getInputLine "Have jetpack? ")
                (\name jet -> Loadlevel name $ PlayerState (jet == "y") Nothing)

        currentLevelExits = levelexits <$> current currentLevelData
        playerInside jp (H.Vector px py) (H.Vector ex ey, H.Vector ew eh, name, initPos)
            | px < ex = Nothing
            | py < ey = Nothing
            | px > ex + ew = Nothing
            | py > ey + eh = Nothing
            | otherwise = Just $ Loadlevel name $ PlayerState jp (Just initPos)
        getExitData _ = do
            pos <- sample playerPosition
            exits <- sample currentLevelExits
            hasJet <- sample playerHasJetpack
            return $ asum $ playerInside hasJet pos <$> exits

    performEvent_ $ liftIO (putStrLn "Restarting level") <$ eRPressed

    let eCtrlLPressed = ctrlPressEvent SFML.KeyL
    eCheckForLevelExit <- tickLossy (1/15) startTime

    eLoadLevel <- fmap (fmapMaybe id) $ performEvent $ promptForLevel <$ eCtrlLPressed

    let levelMessages = messages $ initialData levelLoaded

    messageSprites <- liftIO $ forM levelMessages $ \(pos, filename) -> do
        Just texture <- loadSprite $ "res/" ++ filename
        return (pos, texture)

    let approach desired speed startValue = do
            let step = speed / 60
                updateVal target old =
                    let dif = target - old
                    in if abs dif < step then
                        target
                    else
                        old + signum dif * step
            val <- foldDyn updateVal startValue $ desired <@ updated gameTime
            return $ current val

        messagesWithAlphas =
            forM messageSprites $ \(pos@(H.Vector _ y), sprite) -> do
                let alpha = do
                        ppos@(H.Vector _ py) <- playerPosition
                        if H.len (pos - ppos) < 100 && py < y then 1 else 0
                a <- approach alpha 0.3 0
                return (H.toV2 pos, sprite, a)

    msgs <- messagesWithAlphas

    let renderMessage (pos, texture, alpha) = StaticSprite texture pos 0 <$> alpha
        renderMessages =
            traverse renderMessage msgs
        reloadLevel = Loadlevel levelName initialPlayerState

        playerHealth = (\h -> fromIntegral h / 100) <$> current (characterHealth player)

    fadedHealth <- approach playerHealth 0.1 1

    return LogicOutput
        { cameraCenterPosition = cameraPosition
        , renderCommands =
                renderBackground
                <> renderGhost
                <> renderJetpack
                <> characterRendering player
                <> aiRendering
                <> renderShapes
                <> renderInterface
                <> renderMessages
        , logicPlayerHealth = fadedHealth
        , quit = leftmost
            [ Exit <$ eQPressed
            , reloadLevel <$ eRPressed
            , reloadLevel <$ thisCharacterDied player
            , eLoadLevel
            , push getExitData eCheckForLevelExit
            ]
        }
