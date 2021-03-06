{-# LANGUAGE FlexibleInstances, FlexibleContexts, GADTs, MultiParamTypeClasses, RecursiveDo, ScopedTypeVariables #-}
module Characters where

import Control.Monad.Identity
import Control.Monad.IO.Class (liftIO)

import Data.Bits (bit, complement)
import Data.Bool (bool)
import Data.Monoid ((<>))
import Data.StateVar (($=), get)
import qualified Data.Time.Clock as Time
import qualified Data.Vector.Storable as V

import Foreign.C.Types (CUInt, CDouble(..))
import Foreign.Ptr (Ptr, nullPtr)

import Linear (V2(..), V4(..), (*^))

import Reflex
import Reflex.Time

import qualified SFML.Graphics as SFML
import qualified SFML.Window as SFML

import System.Random (newStdGen, randomRs)

import qualified ChipmunkBindings as H
import Graphics
import Input
import MonadGame
import qualified SpriterTypes as Spriter
import qualified SpriterBindings as Spriter

playerSpeed :: CDouble
playerSpeed = 200

controlVx :: Num a => a -> Bool -> Bool -> a
controlVx x True False = -x
controlVx x False True = x
controlVx _ _ _ = 0

data AiDir = AiLeft | AiStay | AiRight deriving (Eq, Show)

aiDirToDirection :: AiDir -> Maybe Direction
aiDirToDirection AiStay = Nothing
aiDirToDirection AiLeft = Just DLeft
aiDirToDirection AiRight = Just DRight

type CharacterPhysicsRefs = (Ptr H.Shape, Ptr H.Shape)

data CharacterOutput t = CharacterOutput
    { characterSurfaceVelocity :: Behavior t H.Vector
    , characterForce :: Behavior t H.Vector
    , characterDirection :: Behavior t Direction
    , characterAnimation :: Behavior t String
    , characterRendering :: Behavior t [ Renderable ]
    , characterHealth :: Dynamic t Int
    , characterAttack :: Event t (Ptr H.Body, Int)
    , characterRefsAndSprite :: (CharacterPhysicsRefs, Ptr Spriter.CEntityInstance)
    }

queryLineSeg :: Ptr H.Space -> (H.Vector, H.Vector) -> H.ShapeFilter -> IO H.SegmentQueryInfo
queryLineSeg space (s, e) = H.spaceSegmentQueryFirst space s e 1

data DebugRenderSettings = DebugRenderSettings
    { debugRenderAtAll :: Bool
    , debugRenderCharacterShapes :: Bool
    , debugRenderCharacterChecks :: Bool
    }

makeCharacterBody :: H.CpFloat -> H.CpFloat -> [H.Vector]
makeCharacterBody w h =
    [ H.Vector (-w * 0.5) (-h * 0.2)
    , H.Vector (-w * 0.5) (-h * 0.8)
    , H.Vector (-w * 0.3) (-h)
    , H.Vector (w * 0.3) (-h)
    , H.Vector (w * 0.5) (-h * 0.8)
    , H.Vector (w * 0.5) (-h * 0.2)
    , H.Vector (w * 0.3) (-h * 0.1)
    , H.Vector (-w * 0.3) (-h * 0.1)
    ]

characterWidth, characterHeight, characterMass, characterFeetFriction, characterSide :: H.CpFloat
characterWidth = 10
characterHeight = 30
characterMass = 5
characterFeetFriction = 2
characterSide = characterWidth * 0.7

playerFeetCollisionType, mummyCollisionType, wallCollisionType :: CUInt
playerFeetCollisionType = 1
mummyCollisionType = 2
wallCollisionType = 3

characterFeetShapeType, characterBodyShapeType :: H.ShapeType
characterFeetShapeType = H.Circle (characterWidth * 0.3) (H.Vector 0 $ - characterWidth * 0.2)
characterBodyShapeType = H.Polygon (V.fromList $ reverse $ makeCharacterBody characterWidth characterHeight) 0

playerKickCheckR, playerKickCheckL :: (H.Vector, H.Vector)
playerKickCheckR =
    ( H.Vector characterSide (-17)
    , H.Vector (2.2 * characterSide) (-17)
    )
playerKickCheckL =
    ( H.Vector (- characterSide) (-17)
    , H.Vector (- 2.2 * characterSide) (-17)
    )

mummySideCheckUR, mummySideCheckLR, mummySideCheckUL, mummySideCheckLL :: H.Vector
mummySideCheckUR = H.Vector characterSide (-10)
mummySideCheckLR = H.Vector characterSide 10
mummySideCheckUL = H.Vector (-characterSide) (-10)
mummySideCheckLL = H.Vector (-characterSide) 10

makeCharacter :: Ptr H.Space -> IO CharacterPhysicsRefs
makeCharacter space = do
    characterBody <- H.newBody characterMass $ 1/0
    H.spaceAddBody space characterBody
    characterFeetShape <- H.newShape characterBody characterFeetShapeType
    characterBodyShape <- H.newShape characterBody characterBodyShapeType
    H.spaceAddShape space characterFeetShape
    H.spaceAddShape space characterBodyShape
    H.friction characterFeetShape $= characterFeetFriction
    H.friction characterBodyShape $= 0

    return (characterFeetShape, characterBodyShape)

groundCheckFilter :: H.ShapeFilter
groundCheckFilter = H.ShapeFilter
    { H.shapeFilterGroup = H.noGroup
    , H.shapeFilterCategories = H.allCategories
    , H.shapeFilterMask = H.allCategories
    }

visibleByAiBit :: H.CpBitmask
visibleByAiBit = bit 31

aiVisibleFilter :: H.ShapeFilter
aiVisibleFilter = H.ShapeFilter
    { H.shapeFilterGroup = H.noGroup
    , H.shapeFilterMask = visibleByAiBit
    , H.shapeFilterCategories = visibleByAiBit
    }

aiInvisibleFilter :: H.ShapeFilter
aiInvisibleFilter = H.ShapeFilter
    { H.shapeFilterGroup = H.noGroup
    , H.shapeFilterMask = complement visibleByAiBit
    , H.shapeFilterCategories = complement visibleByAiBit
    }


extend :: CDouble -> (H.Vector, H.Vector) -> (H.Vector, H.Vector)
extend amount (start, end) =
    let H.Vector x1 y1 = start
        H.Vector x2 y2 = end
        dx = x2 - x1
        dy = y2 - y1
        len = H.len (H.Vector dx dy)
        extension = H.Vector (dx / len * amount) (dy / len * amount)
    in (start, end + extension)

limit :: (Num a, Ord a, Reflex t) => Behavior t a -> Behavior t a
limit = fmap limf where
    limf x
        | x < -1 = -1
        | x > 1 = 1
        | otherwise = x

debugRenderCharacter :: Reflex t => Behavior t DebugRenderSettings -> [Renderable] -> [Renderable] -> Behavior t [Renderable]
debugRenderCharacter debugSettings renderFeetAndBody renderSideChecks = do
    debugRender <- debugRenderAtAll <$> debugSettings
    if not debugRender then
        return []
    else do
        dbgRenderChrs <- debugRenderCharacterShapes <$> debugSettings
        dbgRenderCol <- debugRenderCharacterChecks <$> debugSettings
        return $ case (dbgRenderChrs, dbgRenderCol) of
             (False, False) -> []
             (True, False) -> renderFeetAndBody
             (False, True) -> renderSideChecks
             (True, True) -> renderFeetAndBody <> renderSideChecks

attackEffect :: Ptr H.Space -> H.Vector -> Direction -> H.CpFloat -> (Ptr H.Shape -> Ptr H.Body -> IO (Maybe (Ptr H.Body))) -> IO (Maybe (Ptr H.Body))
attackEffect space pos dir imp onHit = do
    let (pkc1, pkc2) = case dir of
            DRight -> playerKickCheckR
            DLeft -> playerKickCheckL
        attackVec = case dir of
            DRight -> H.Vector imp (-imp)
            DLeft -> H.Vector (-imp) (-imp)

    hitShapeInfo <- queryLineSeg space (pos + pkc1, pos + pkc2) groundCheckFilter

    let hitShape = H.segQueryInfoShape hitShapeInfo
    if hitShape /= nullPtr then do
        hitBody <- get $ H.shapeBody hitShape
        H.applyImpulse hitBody attackVec H.zero
        onHit hitShape hitBody
    else
        return Nothing

aiCharacterNetwork :: forall t m. MonadGame t m =>
    Ptr H.Space ->
    Ptr H.Body ->
    Event t TickInfo ->
    Event t () ->
    Event t (Ptr H.Body, Int) ->
    Dynamic t Time.NominalDiffTime ->
    Behavior t H.Vector ->
    Behavior t DebugRenderSettings ->
    (Ptr H.Shape, Ptr H.Shape) ->
    Ptr Spriter.CEntityInstance ->
    m (CharacterOutput t)
aiCharacterNetwork space playerBody eAiTick eSamplePhysics eWasHit gameTime playerPosition debugSettings (feetShape, bodyShape) sprite = do
    let speedForAnim "Run" = playerSpeed / 3
        speedForAnim _ = playerSpeed / 6

        mummySpeed AiRight anim = H.Vector (- speedForAnim anim) 0
        mummySpeed AiLeft anim = H.Vector (speedForAnim anim) 0
        mummySpeed AiStay _ = H.zero

        rightSideSegment currentPos =
            ( currentPos + mummySideCheckUR
            , currentPos + mummySideCheckLR
            )
        leftSideSegment currentPos =
            ( currentPos + mummySideCheckUL
            , currentPos + mummySideCheckLL
            )

        playerBodyPosition = playerPosition - pure (H.Vector 0 15)

        checkSide col =
            let shape = H.segQueryInfoShape col
            in if shape == nullPtr then
                   return Nothing
               else
                   (\b -> if b == playerBody then Just () else Nothing) <$> get (H.shapeBody shape)

        mummyPunchEffect :: H.Vector -> Direction -> Performable m (Maybe (Ptr H.Body))
        mummyPunchEffect position dir =
            liftIO $ attackEffect space position dir 500
                (\_ hitBody ->
                     if hitBody == playerBody then
                         putStrLn "punched player" >> return (Just playerBody)
                     else
                         return Nothing)

    body <- get $ H.shapeBody feetShape

    ePolledPos <- performEvent $ get (H.position body) <$ eSamplePhysics
    pos <- hold H.zero ePolledPos

    rec
        let mummyEyePos = pos + pure (H.Vector 0 (-20))

            mummyVisionLine = extend 20 <$> ((,) <$> mummyEyePos <*> playerBodyPosition)

            checkForPlayer dir visionLine@(H.Vector mx my, H.Vector px py) = liftIO $ do

                playerSearchHitInfo <- queryLineSeg space visionLine aiVisibleFilter
                let hitShape = H.segQueryInfoShape playerSearchHitInfo
                    playerInFront = case dir of
                        DRight -> px > mx
                        DLeft -> px < mx
                    distToPlayer = H.len $ H.Vector (px - mx) (py - my)
                    playerAngleCos = abs (px - mx) / distToPlayer
                    playerAngleAcceptable = playerAngleCos > cos (pi / 4)
                playerNotBlocked <- if hitShape /= nullPtr
                    then do
                        hitBody <- get $ H.shapeBody hitShape
                        return (hitBody == playerBody)
                    else
                        return False

                return $ distToPlayer < 200 && playerInFront && playerAngleAcceptable && playerNotBlocked

            doAiCollisionChecks mp = liftIO $ do
                let (pkc1r, pkc2r) = playerKickCheckR
                    (pkc1l, pkc2l) = playerKickCheckL
                    segsToCheck =
                        [ rightSideSegment mp
                        , leftSideSegment mp
                        , (pkc1r + mp, pkc2r + mp)
                        , (pkc1l + mp, pkc2l + mp)
                        ]

                [cgr, cgl, cwr, cwl] <- forM segsToCheck $ \seg ->
                    queryLineSeg space seg groundCheckFilter
                return (cgr, cgl, cwr, cwl)

        eCanSeePlayer <-
            performEvent $ checkForPlayer <$> mummyDisplayDir <*> mummyVisionLine <@ eAiTick

        mummySeesPlayer <- hold False eCanSeePlayer

        let runAiLogic (colGroundRight, colGroundLeft, colWallRight, colWallLeft) (currentDir, _currentAnim) = do
                canSeePlayer <- sample mummySeesPlayer
                dir <- sample mummyDisplayDir

                let canGoLeft = H.segQueryInfoShape colGroundLeft /= nullPtr
                        && H.segQueryInfoShape colWallLeft == nullPtr
                    canGoRight = H.segQueryInfoShape colGroundRight /= nullPtr
                        && H.segQueryInfoShape colWallRight == nullPtr

                return $ case currentDir of
                    AiLeft
                        | canGoLeft ->
                              if not canSeePlayer then (AiLeft, "Walk") else (AiLeft, "Run")
                        | canGoRight ->
                              if canSeePlayer then (AiStay, "Idle") else (AiRight, "Walk")
                        | otherwise -> (AiStay, "Idle")
                    AiRight
                        | canGoRight ->
                              if not canSeePlayer then (AiRight, "Walk") else (AiRight, "Run")
                        | canGoLeft ->
                              if canSeePlayer then (AiStay, "Idle") else (AiLeft, "Walk")
                        | otherwise -> (AiStay, "Idle")
                    AiStay
                        | canSeePlayer && dir == DRight && not canGoRight -> (AiStay, "Idle")
                        | canSeePlayer && dir == DLeft && not canGoLeft -> (AiStay, "Idle")
                        | canGoRight -> (AiRight, "Walk")
                        | canGoLeft -> (AiLeft, "Walk")
                        | otherwise -> (AiStay, "Idle")

        colResults <- performEvent $ liftIO <$> (doAiCollisionChecks <$> pos <@ eAiTick)

        let checkForHitPossibility canSeePlayer dir (_, _, colRight, colLeft) = liftIO $
                if not canSeePlayer then
                    return Nothing
                else
                    case dir of
                        DLeft -> checkSide colLeft
                        DRight -> checkSide colRight

        eHitChecks <- performEvent $
            checkForHitPossibility <$> mummySeesPlayer <*> mummyDisplayDir <@> colResults

        rec
            let eWantsToHitPlayer = fmapMaybe id eHitChecks
                eStartPunch = gate (not <$> isPunching) eWantsToHitPlayer

            performEvent_ $
                liftIO (Spriter.setEntityInstanceCurrentTime sprite 0) <$ eStartPunch

            let punchDelay = 0.25
                punchDuration = 0.6

            latestMummyPunch <- hold Nothing $ Just <$> current gameTime <@ eStartPunch
            eDelayedMummyPunch <- delayInDynTime gameTime punchDelay eStartPunch

            eHitPlayer <- performEvent $
                mummyPunchEffect <$> pos <*> mummyDisplayDir <@ eDelayedMummyPunch

            let isPunching = do
                    t <- current gameTime
                    mLatestPunchStart <- latestMummyPunch
                    return $ case mLatestPunchStart of
                        Nothing -> False
                        Just latestPunchStart -> t < latestPunchStart + punchDuration

        mummyWalkingState <- foldDynM runAiLogic (AiStay, "Idle") colResults
        let mummyAiState = do
                currentlyPunching <- isPunching
                return $
                    if currentlyPunching then
                        pure (AiStay, "Punch")
                    else
                        mummyWalkingState

        let mummyWalkDirectionDyn = fmap fst <$> mummyAiState
            eMummyWalkDirection = switch $ updated <$> mummyWalkDirectionDyn
            mummyWalkDirection = join $ current <$> mummyWalkDirectionDyn

        mummyDisplayDir <- hold DRight $ fmapMaybe aiDirToDirection eMummyWalkDirection

    let mummyAnimation = join $ fmap current $ fmap snd <$> mummyAiState
        mummySurfaceVelocity = mummySpeed <$> mummyWalkDirection <*> mummyAnimation

    let aiAnimation = do
            aiCurrentPosition <- pos
            aiCurrentAnimation <- mummyAnimation
            aiCurrentDirection <- mummyDisplayDir
            return
                [ AnimatedSprite
                    sprite
                    aiCurrentAnimation
                    (H.toV2 aiCurrentPosition)
                    aiCurrentDirection
                ]

        renderColDebug = do
            currentAiPosition <- pos
            (eyePos, playerBodyPos) <- mummyVisionLine
            seesPlayer <- mummySeesPlayer

            let renderFeetAndBody =
                    [ Shape feetShape characterFeetShapeType
                    , Shape bodyShape characterBodyShapeType
                    ]
                chrLine chrP (p1, p2) =
                    Line (V4 255 0 0 255) (H.toV2 $ p1 + chrP) (H.toV2 $ p2 + chrP)
                mummyLines mummyP =
                    [ chrLine mummyP (mummySideCheckUL, mummySideCheckLL)
                    , chrLine mummyP (mummySideCheckUR, mummySideCheckLR)
                    , chrLine mummyP playerKickCheckR
                    , chrLine mummyP playerKickCheckL
                    ]
                renderEnemyVision =
                    let color = if seesPlayer then V4 0 255 0 255 else V4 255 0 0 255
                    in Line color (H.toV2 eyePos) (H.toV2 playerBodyPos)
                renderSideChecks =
                     mummyLines currentAiPosition ++ [ renderEnemyVision ]

            debugRenderCharacter debugSettings renderFeetAndBody renderSideChecks

    health <- foldDyn (+) 100 $ negate <$>
        fmapMaybe (\(b, d) -> if b == body then Just d else Nothing) eWasHit

    return CharacterOutput
        { characterSurfaceVelocity = mummySurfaceVelocity
        , characterForce = pure H.zero
        , characterDirection = mummyDisplayDir
        , characterAnimation = mummyAnimation
        , characterRendering = aiAnimation <> renderColDebug
        , characterAttack = (playerBody, 20) <$ fmapMaybe id eHitPlayer
        , characterHealth = health
        , characterRefsAndSprite = ((feetShape, bodyShape), sprite)
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

playerNetwork :: forall t m. MonadGame t m =>
    Time.UTCTime ->
    Ptr H.Space ->
    EventSelector t SfmlEventTag ->
    Event t Int ->
    Dynamic t Time.NominalDiffTime ->
    Behavior t Bool ->
    Behavior t Bool ->
    Behavior t (SFML.KeyCode -> Bool) ->
    (Behavior t H.Vector, Behavior t H.Vector) ->
    Behavior t Bool ->
    Behavior t [(Ptr H.Shape, Ptr H.Shape)] ->
    Behavior t DebugRenderSettings ->
    Maybe JoystickID ->
    Ptr H.Body ->
    (Ptr H.Shape, Ptr H.Shape) ->
    Ptr Spriter.CEntityInstance ->
    (SFML.Sprite, SFML.Sprite) ->
    m (CharacterOutput t)
playerNetwork startTime space sfmlEventFan eWasHit gameTime hasJetpack notEditing pressedKeys (pos, velocity) onGround aiShapes debugSettings mGamepad body (feetShape, bodyShape) sprite (jetpackTex, particleTex) = do
    let pressEvent kc = gate notEditing $ ffilter (isKeyPressed kc) $ select sfmlEventFan KeyEvent
        eAPressed = pressEvent SFML.KeyA
        eDPressed = pressEvent SFML.KeyD
        eWPressed = pressEvent SFML.KeyW
        eKPressed = pressEvent SFML.KeyK
        ePadAxisMove = gate notEditing $ select sfmlEventFan (JoyAxisEvent 0)
        padFilterButtonPress b = ffilter (wasButtonPressed b) $ gate notEditing $
            select sfmlEventFan (JoyButtonEvent 0)
        ePadAPressed = padFilterButtonPress padButtonA
        ePadXPressed = padFilterButtonPress padButtonX
        ePadNotCenter = ffilter
            (\(SFML.SFEvtJoystickMoved _ a v) ->
                 a == SFML.JoystickY &&
                abs v > 0.15)
            ePadAxisMove
        ePadChangeDir = (\(SFML.SFEvtJoystickMoved _ _ v) -> if v > 0 then DRight else DLeft)
            <$> ePadNotCenter

        ePlayerWantsToJump = mconcat [() <$ eWPressed, () <$ ePadAPressed]
        ePlayerWantsToKick = mconcat [() <$ eKPressed, () <$ ePadXPressed]

    rec
        let ePlayerKick = gate (not <$> current playerIsKicking) ePlayerWantsToKick
            eJump = (-1000) <$ gate playerCanJump ePlayerWantsToJump
            kickDuration = 0.6

        latestPlayerKick <- holdDyn Nothing $ Just <$> current gameTime <@ ePlayerKick

        let playerIsKicking = do
                currentTime <- gameTime
                lastKickTime <- latestPlayerKick
                return $ case lastKickTime of
                    Just t -> currentTime - t < kickDuration
                    Nothing -> False

            playerCanJump = (&&) <$> onGround <*> (not <$> current playerIsKicking)

    let jump imp = liftIO $ H.applyImpulse body (H.Vector 0 imp) H.zero

        playerKickEffect :: H.Vector -> Direction -> [(Ptr H.Shape, Ptr H.Shape)] -> Performable m (Maybe (Ptr H.Body))
        playerKickEffect playerP playerD currentAiShapes = do
            currentAiBodies <- mapM (get . H.shapeBody) $ fst <$> currentAiShapes
            liftIO $ attackEffect space playerP playerD 700
                (\_ hitBody -> if hitBody `elem` currentAiBodies then
                        putStrLn "Kicked mummy" >> return (Just hitBody)
                    else
                        return Nothing)

    ePollInput <- tickLossy (1/15) startTime
    eCurrentInput <- performEvent $ pollInput mGamepad <$ gate notEditing ePollInput
    gamepadInput <- hold initialInput eCurrentInput

    let aPressed = ($ SFML.KeyA) <$> pressedKeys
        dPressed = ($ SFML.KeyD) <$> pressedKeys
        iPressed = ($ SFML.KeyI) <$> pressedKeys
        playerKeyMovement = controlVx 1 <$> aPressed <*> dPressed
        playerAxisMovement = leftXAxis <$> gamepadInput
        yHeld = yPressed <$> gamepadInput
        playerMovement = limit $ (+) <$> playerKeyMovement <*> playerAxisMovement
        playerAirForce = (\d -> H.Vector (d * 1000) 0) <$> playerMovement
        playerAcc = H.Vector <$> fmap (* playerSpeed) playerMovement <*> pure 0

        dontUseTheForce = (&&) <$> onGround <*> fmap not (current playerIsKicking)
        playerSurfaceVelocity = do
            notUsingForce <- dontUseTheForce
            if notUsingForce
                then playerAcc
                else pure H.zero

        playerMoving = (\(H.Vector vx _) -> abs vx > 0) <$> playerSurfaceVelocity
        kickDelay = 0.4

    eDelayedPlayerKick <- delayInDynTime gameTime kickDelay ePlayerKick

    playerDir <- hold DRight $ gate (not <$> current playerIsKicking) $ gate notEditing $ leftmost
        [ DLeft <$ eAPressed
        , DRight <$ eDPressed
        , ePadChangeDir
        ]

    performEvent_ $ liftIO (Spriter.setEntityInstanceCurrentTime sprite 0) <$ ePlayerKick
    eHitEnemy <- performEvent $
        playerKickEffect <$> pos <*> playerDir <*> aiShapes <@ eDelayedPlayerKick

    performEvent_ $ jump <$> eJump

    stdGen <- liftIO newStdGen

    let particleXVelocities = randomRs (-20, 20) stdGen
        jetpackPosition = do
            p <- pos
            dir <- playerDir
            let jetpackOffset = case dir of
                    DLeft -> V2 3 (-25)
                    DRight -> V2 (-8) (-25)
            return $ H.toV2 p + jetpackOffset
        jetpackOn = (\jp i y -> jp && (i || y)) <$> hasJetpack <*> iPressed <*> yHeld

    eSpawnParticleTick <- gate notEditing <$> tickLossy particleSpawnInterval startTime
    eParXVel <- zipListWithEvent const particleXVelocities $
        gate jetpackOn eSpawnParticleTick

    let spawnParticle xv = do
            playerV <- sample velocity
            jetpos <- sample jetpackPosition
            t <- sample $ current gameTime
            return Particle
                { particleVelocity = H.toV2 playerV + V2 xv 200
                , particleAngularVel = xv
                , particleStartTime = t
                , particleLifetime = realToFrac $ 0.5 + 0.05 * xv
                , particleStartPos = jetpos
                }
        eNewParticle = pushAlways spawnParticle eParXVel

    rec
        particles <- hold [] $ flip (:) <$> aliveParticles <@> eNewParticle
        let aliveParticles = (filter . particleAlive) <$> current gameTime <*> particles

    let invincibleDuration = 2
        blinkInterval = 0.1

    eTimeDiffs <- mapAccum_ (\t t' -> (t', t' - t)) 0 $ updated gameTime
    eToggleBlink <- mapAccumMaybe_
        (\acc dt -> let acc' = acc + dt in
                if acc' >= blinkInterval then
                    (Just $ acc' - blinkInterval, Just ())
                else
                    (Just acc', Nothing)) 0 eTimeDiffs
    blinkShowing <- toggle True eToggleBlink

    rec
        latestHurtTime <- holdDyn Nothing $ Just <$> current gameTime <@ eWasHit

        let isInvincible = do
                mLatestDamage <- latestHurtTime
                t <- gameTime
                return $ case mLatestDamage of
                    Just latestDamage -> t - latestDamage < invincibleDuration
                    Nothing -> False

            eTakeDamage = gate (not <$> current isInvincible) eWasHit

    health <- foldDyn (+) 100 $ negate <$> eTakeDamage

    let pickAnimation moving onGroundNow isKicking
            | isKicking = "Kick"
            | not onGroundNow = "Falling"
            | moving = "Run"
            | otherwise = "Idle"

        playerAnimation =
            pickAnimation <$> playerMoving <*> onGround <*> current playerIsKicking
        horizontalForce = bool <$> playerAirForce <*> pure H.zero <*> dontUseTheForce
        verticalForce = bool H.zero (H.Vector 0 $ -6000) <$> jetpackOn
        playerForce = horizontalForce + verticalForce

        renderCharacter = do
            anim <- playerAnimation
            p <- fmap H.toV2 pos
            d <- playerDir
            blink <- current blinkShowing
            invincible <- current isInvincible
            return [ AnimatedSprite sprite anim p d | not invincible || blink ]

        renderColDebug = do
            playerP <- pos
            let renderFeetAndBody =
                    [ Shape feetShape characterFeetShapeType
                    , Shape bodyShape characterBodyShapeType
                    ]
                chrLine chrP (p1, p2) =
                    Line (V4 255 0 0 255) (H.toV2 $ p1 + chrP) (H.toV2 $ p2 + chrP)
                renderSideChecks =
                    [ chrLine playerP playerKickCheckR
                    , chrLine playerP playerKickCheckL
                    ]

            debugRenderCharacter debugSettings renderFeetAndBody renderSideChecks

        renderJetpack = do
            hasJet <- hasJetpack
            if not hasJet
                then return []
                else do
                jetpos <- jetpackPosition
                anim <- playerAnimation
                dir <- playerDir
                let angle =
                        if anim == "Run" then
                            if dir == DRight then 10 else -10
                        else
                            0
                return [ StaticSprite jetpackTex jetpos angle 1 ]

        renderParticles = do
            pars <- aliveParticles
            currentTime <- current gameTime
            forM (particleState currentTime <$> pars) $ \(p, angle) ->
                return $ StaticSprite particleTex p angle 1

    return CharacterOutput
        { characterSurfaceVelocity = playerSurfaceVelocity
        , characterForce = playerForce
        , characterAnimation = playerAnimation
        , characterDirection = playerDir
        , characterRendering =
                renderParticles
                <> renderJetpack
                <> renderCharacter
                <> renderColDebug
        , characterHealth = health
        , characterAttack = (\b -> (b, 40)) <$> fmapMaybe id eHitEnemy
        , characterRefsAndSprite = ((feetShape, bodyShape), sprite)
        }
