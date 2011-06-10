-- | Display information about current and past FPS.
--
-- By proving an additional parameter to drawFPS, user-defined strings can be
-- shown in the FPS window, too. Currently the maximum FPS is fixed to 60 as a
-- constant.

module Statistics (
    FPS
  , newFPS
  , drawFPS
  , toggleFPS
) where

import Control.Monad
import Data.IORef
import Graphics.UI.GLUT hiding (position)
import Unsafe.Coerce (unsafeCoerce)
import Window


data FPS = FPS {
    fpsTime  :: IORef Double
  , fpsShown :: IORef Bool
  , fpsLast  :: IORef [GLdouble]
}


-- | Creates a new measuring object.
newFPS :: IO FPS
newFPS = do
    t <- newIORef =<< getTime
    s <- newIORef True
    l <- newIORef []
    return FPS {
        fpsTime  = t
      , fpsShown = s
      , fpsLast  = l
    }



-- | Shows the fps (if toggled) in a graph.
--
-- The additional string in add is shown directly after the FPS.
drawFPS :: FPS -> Maybe String -> IO ()
drawFPS (FPS t s fpss) add = do
    -- Should we display the graph at all?
    toShow <- readIORef s
    when toShow $ do

    -- Calculate difference between runs and update list of differences.
    told <- readIORef t
    tcur <- getTime
    let tdiff = tcur - told
    writeIORef t tcur
    runs <- readIORef fpss
    let f' = take 59 runs
    writeIORef fpss $ unsafeCoerce (1/tdiff) : f'

    preservingMatrix $ do
    -- Move to upper left corner.
    translate $ Vector3 (0.02 :: GLdouble) 0.89 0.0
    Graphics.UI.GLUT.scale 0.5 0.1 (0.1 :: GLdouble)

    -- Draw black border.
    color $ Color3 0 0 (0 :: GLdouble)
    renderPrimitive LineLoop $ do
        toVertex (0.0, 1 :: Double)
        toVertex (1.0, 1 :: Double)
        toVertex (1.0, 0 :: Double)
        toVertex (0.0, 0 :: Double)

    -- Show values.
    values <- readIORef fpss
    unless (null values) $ do
    let steps  = 59.0
        dsteps = 1.0 / steps
    color $ Color3 1 0 (0 :: GLdouble)
    renderPrimitive LineStrip $ 
        forM_ (zip [1.0,(1.0-dsteps)..0] values) $ \(x,y) -> do
            let b = (x, min (y/steps) 0.99)
            toVertex b

    -- Display textual information
    color $ Color3 0 0 (0 :: GLdouble)
    let str = take 5 $ show (head values) ++ maybe [] (" " ++) add
    text (0.04, 0.75) [str]


-- | Toggles display of fps information.
toggleFPS :: FPS -> IO ()
toggleFPS f = modifyIORef (fpsShown f) not
