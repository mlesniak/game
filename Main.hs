-- | Simulation of balls using the Chipmunk library.
--
-- Regarding Chipmunk, ball and border are the important functions; and probably
-- the call to step in line 63.
--

module Main where

import Control.Concurrent
import Control.Monad
import Graphics.UI.GLUT hiding (position, Position)
import Physics.Hipmunk hiding (Position)
import Statistics
import System.Random (randomRIO)
import Unsafe.Coerce (unsafeCoerce)
import Window


main :: IO ()
main = do
    -- Physics intialization.
    initChipmunk
    space <- newSpace
    gravity space $= Vector 0 (-1)

    -- Enabling performance information display.
    fpsStat <- newFPS
  
    -- Create outside borders.
    let borders = [ ((0.01, 0.00), (1.29, 0.00)),
                    ((0.01, 0.00), (0.01, 1.00)),
                    ((1.29, 0.00), (1.29, 1.00)) ]
    mapM_ (border space) borders

    -- Drag'n'Drop support. We store the object (actually, its shape) clicked
    -- here until the user is finished (by releasing the mouse button).
    drag <- newMVar Nothing

    -- Container for all balls.
    let numNew = 10
    balls <- newMVar []
    
    -- The WindowConfig type is special for the underlying graphics system and
    -- provides handlers for frame drawing, key and mouse events. It's not
    -- important for the physic simulation.
    let wc = WindowConfig {
        -- The FrameHandler is called for every frame, i.e. fps times per
        -- second.
        frameHandler = FrameHandler $ do
            -- Handle Drag'n'Drop. I'm pretty sure this is not the best
            -- solution, but it works for now.
            v <- readMVar drag
            case v of
                Nothing    -> return ()
                Just (b,p) -> do
                    -- Reset velocity. Should we reset to the known velocity
                    -- before the DnD?
                    velocity b $= Vector 0 (0.0)
                    position b $= p

            -- Draw borders in red. 
            color $ Color3 1 0 (0 :: GLdouble)
            renderPrimitive Lines $ 
                forM_ borders $ \(s, e) -> do
                    toVertex s
                    toVertex e

            -- Draw balls with their particular color.
            bs <- readMVar balls
            forM_ bs $ \(b, c, r) -> do
                Vector x y <- get $ position (body b)
                circle (x, y) r c
            
            -- Draw fps statistics, if enabled and calculate new position.
            drawFPS fpsStat (Just $ "Objects: " ++ show (length bs))

            -- Physics-wise, this is the important step in the simulation which
            -- recalculates the new positions of our objects.
            step space (1.0 / (fps wc))

        -- Key handling.
      , keyHandler = Just $ KeyHandler $ \key state _ -> 
            when (state == Down) $
            case key of
                Char 'c'   -> clearSpace space balls
                Char 'b'   -> replicateM_ numNew (newBall space balls)
                Char 'v'   -> newBigBall space balls
                Char 'f'   -> toggleFPS fpsStat
                Char '\27' -> leaveMainLoop
                _          -> return ()

      , mouseHandler  = Just $ MouseHandler (clickBall space drag)
      , motionHandler = Just $ MotionHandler (moveBall space drag)
      , title         = "Ball simulation with Chipmunk in Haskell"
      , size          = Size 640 480
      , fps           = 30
    }
    windowLoop wc


-- Object creation and removal, the important physics stuff                   --
-- Storing the balls, i.e. their shape, color and radius.
type Balls = MVar [(Shape, Color3 GLdouble, Double)]

-- | Create an unmovable and blocking border.
border :: Space -> ((Double, Double), (Double, Double)) -> IO ()
border space ((x1,y1), (x2,y2)) = do
    ground <- newBody infinity infinity
    gshape <- newShape ground (LineSegment (Vector x1 y1) (Vector x2 y2) 0.01) 
               (Vector 0.0 0.0)
    position ground   $= Vector 0.0 0.00
    elasticity gshape $= 0.5
    friction gshape   $= 0.8
    spaceAdd space (Static gshape)


-- | Create a new ball with random velocity, position around 0.6 on the x-axis
newBall :: Space -> Balls -> IO () 
newBall space balls = do
        x         <- randomRIO (0.5, 0.7) :: IO CpFloat
        maxRadius <- randomRIO (0.01, 0.03) :: IO Double
        rad       <- randomRIO (0.001, maxRadius) :: IO Double
        vel       <- randomPoint (-0.8, 0.8)

        ball space balls 100 (x, 0.7) vel rad   


-- | Creates a new heavy ball.
newBigBall :: Space -> Balls -> IO () 
newBigBall space balls = ball space balls 100000 (0.65, 0.95) (0,0) 0.05


-- | Creates a new ball with the given properties and add it to the space.
--
-- Physics-wise, this (and border above) are the important functions for
-- simulating physically correct ball behaviour.
ball :: Space -> Balls -> Mass -> (CpFloat, CpFloat) -> (CpFloat, CpFloat)
  -> Double -> IO ()
ball space balls m pos vel rad = do
    -- Body.
    c <- randomColor
    b <- newBody m infinity
    position b $= uncurry Vector pos
    velocity b $= uncurry Vector vel
    spaceAdd space b
    
    -- Shape.
    bshape <- newShape b (Circle $ unsafeCoerce rad) (Vector 0 0)
    elasticity bshape    $= 0.9
    friction bshape      $= 0.1
    spaceAdd space bshape

    modifyMVar_ balls (\k -> return $ (bshape, c, rad):k)


-- | Empties the space.
clearSpace :: Space -> MVar [(Shape, t, t1)] -> IO ()
clearSpace space balls = do
    as <- takeMVar balls
    mapM_ (\(k,_,_) -> spaceRemove space k) as
    mapM_ (\(sh,_,_) -> spaceRemove space (body sh)) as
    putMVar balls []


-- Drag'n'Drop support                                                        --
moveBall :: Space -> MVar (Maybe (Body, Vector)) -> Position -> IO ()
moveBall space mv (Position pos) = do
    v <- takeMVar mv
    case v of
        Nothing    -> putMVar mv Nothing
        Just (b,_) -> do
            putMVar mv $ Just (b, p2v pos)
  where p2v (x,y) = Vector (unsafeCoerce x) (unsafeCoerce y)


clickBall :: Space -> MVar (Maybe (Body, Vector)) -> MouseButton -> Position 
  -> IO ()
clickBall space mv b (Position pos) = do
    v <- takeMVar mv
    case v of
        Nothing  -> do
            shapes <- spaceQueryList space (p2v pos) (-1) 0
            if null shapes 
                then putMVar mv Nothing
                else do
                    let s = head shapes
                        b = body s
            
                    putMVar mv (Just (b, p2v pos))
        -- Something is already dragged, user has released the button.
        Just (b,_) -> putMVar mv Nothing
  where p2v (x,y) = Vector (unsafeCoerce x) (unsafeCoerce y)


-- Helper functions                                                           --
-- | Draws a circle in the given color with a black border.
circle :: (CpFloat, CpFloat) -> CpFloat -> Color3 GLdouble -> IO ()
circle (x,y) r c = preservingMatrix $ do
    let poly = 24
        ang  = \p -> p * 2 * pi / poly
        pos  = map (\p -> (x+cos(ang p)*r, y + sin(ang p)*r)) [1,2..poly]
    color c
    renderPrimitive Graphics.UI.GLUT.Polygon $
        mapM_ (toVertex . conv) pos
    color $ Color3 0 0 (0 :: GLdouble)
    renderPrimitive Graphics.UI.GLUT.LineLoop $
        mapM_ (toVertex . conv) pos
  where conv :: (CpFloat, CpFloat) -> (GLdouble, GLdouble)
        conv (a,b) = (unsafeCoerce a, unsafeCoerce b)


-- | Returns a random point in the given range.
--
-- Returns the point in Chipmunk coordinates.
randomPoint :: (Double, Double) -> IO (CpFloat, CpFloat)
randomPoint (l, r) = do
    -- Double == CpFloat in Chipmunk.
    x1 <- unsafeCoerce `liftM` (randomRIO (l, r) :: IO Double)
    x2 <- unsafeCoerce `liftM` (randomRIO (l, r) :: IO Double)
    return (x1, x2)


-- | Returns a random color.
randomColor :: IO (Color3 GLdouble)
randomColor = do
    c1 <- rndC
    c2 <- rndC
    c3 <- rndC
    return (Color3 c1 c2 c3)
  where rndC :: IO (GLdouble)
        rndC = unsafeCoerce `liftM` (randomRIO (0, 1) :: IO Double)




