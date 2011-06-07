-- | An OpenGL based game loop.
--

module Window (
    window
  , windowLoop
  , FrameHandler(..)
  , KeyHandler(..)
  , Key(..)
  , KeyState
  , Modifiers
  , WindowConfig(..)
  , MouseButton
  , MouseHandler(..)
  , MotionHandler(..)
  , Position(..)
) where

import Control.Concurrent
import Control.Monad
import Data.Time.Clock.POSIX (getPOSIXTime)
import Graphics.UI.GLUT hiding (Position)
import qualified Graphics.UI.GLUT as GLUT


newtype FrameHandler  = FrameHandler { getFrame :: IO () }

newtype KeyHandler    = KeyHandler { 
    getKeyHandler :: Key -> KeyState -> Modifiers -> IO () 
}

newtype MouseHandler  = MouseHandler { 
    getMouseHandler :: MouseButton -> Position -> IO () 
}

newtype MotionHandler = MotionHandler { 
    getMotionHandler :: Position -> IO () 
}

data Position = Position { 
    getPosition :: (GLdouble, GLdouble) 
} deriving (Show, Eq)


data WindowConfig = WindowConfig {
    frameHandler  :: FrameHandler
  , keyHandler    :: Maybe KeyHandler
  , mouseHandler  :: Maybe MouseHandler
  , motionHandler :: Maybe MotionHandler
  , title         :: String
  , size          :: Size
  , fps           :: Double
}


-- | Creates a new output window using the specified config.
--
window :: WindowConfig -> IO ()
window wc = forkOS (windowMain wc) >> return ()


windowLoop :: WindowConfig -> IO ()
windowLoop wc = do
    windowMain wc
    forever (return ())


-- | Shows the main window and initializes the fps-based loop.
windowMain :: WindowConfig -> IO ()
windowMain wc = do
    getArgsAndInitialize
    createWindow (title wc)
    perWindowKeyRepeat    $= PerWindowKeyRepeatOff
    initialDisplayMode    $= [DoubleBuffered]
    clearColor            $= Color4 1 1 (1 :: GLclampf) 1
    windowSize            $= size wc
    actionOnWindowClose   $= Exit
    keyboardMouseCallback $= Just (eventHandler wc)
    motionCallback        $= Just (motionEventHandler wc)

    -- For antialiasing.
    lineSmooth            $= Enabled
    lineWidth             $= 3.5
    hint LineSmooth       $= Nicest

    displayCallback $= do
        clear [ColorBuffer]
        loadIdentity
        translate $ Vector3 (-1.0) (-1.0) (0.0 :: GLfloat) 
        scale (2/1.3) (2/1.3) (1.0 :: GLfloat)
        getFrame (frameHandler wc)
        swapBuffers

    gameLoop (fps wc)
    mainLoop


gameLoop :: Double -> IO ()
gameLoop fps_ = loop 
  where loop = do
            t1 <- getTime
            postRedisplay Nothing
            t2 <- getTime
            let twait  = t2 - t1
                tdelay = fromEnum $ (1/fps_ - twait) * 1000
            if twait < 1/fps_ 
                then addTimerCallback tdelay loop
                else loop
        getTime :: IO Double
        getTime = (fromRational . toRational) `fmap` getPOSIXTime


eventHandler :: WindowConfig -> Key -> KeyState -> Modifiers -> GLUT.Position 
  -> IO ()
eventHandler wc key state mods pos =
    case key of
       MouseButton b ->
           case mouseHandler wc of
               Nothing -> return ()
               Just ha -> do
                   getMouseHandler ha b (windowToReal wc pos)
       _             -> 
           case keyHandler wc of
               Nothing -> return ()
               Just h  -> getKeyHandler h key state mods


motionEventHandler :: WindowConfig -> GLUT.Position -> IO ()
motionEventHandler wc pos = 
   case motionHandler wc of
       Nothing -> return ()
       Just h  -> getMotionHandler h (windowToReal wc pos)


windowToReal :: WindowConfig -> GLUT.Position -> Position
windowToReal wc pos =
    let GLUT.Position i j = pos
        Size w h = size wc
        x        = 1.3 / fromIntegral w * fromIntegral i
        y        = 1.0 - 1.0 / fromIntegral h * fromIntegral j in
    Position (x,y)

