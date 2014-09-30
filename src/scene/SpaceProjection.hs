import Control.Monad ( when )
import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess), exitFailure )

import Graphics.UI.GLUT

import Cube
import Star
import Grid
import StarCluster
import Fighter
import Pyramid
import Station
import Sphere

import GLUtils


----------------------------------------------------------------------------------------------------------------
-- Global State
type View = (GLfloat, GLfloat, GLfloat)

data Zoom = In | Out
data Mod = Increase | Decrease
zoomDelta = 5e-4

data State = State {
   frames  :: IORef Int,
   t0      :: IORef Int,
   ph'     :: IORef GLfloat,
   th'     :: IORef GLfloat,
   gr'      :: IORef GLfloat,
   info    :: IORef (String,String)
 }

makeState :: IO State
makeState = do
  f  <- newIORef 0
  t  <- newIORef 0
  ph <- newIORef 0
  th <- newIORef 0
  gr <- newIORef 0
  i  <- newIORef ("","")
  return $ State {  frames = f, t0 = t, ph' = ph, th' = th, gr' = gr, info = i }

----------------------------------------------------------------------------------------------------------------
-- Timer 
timerFrequencyMillis :: Timeout
timerFrequencyMillis = 20

timer :: State -> TimerCallback
timer state = do
  addTimerCallback timerFrequencyMillis (timer state)

----------------------------------------------------------------------------------------------------------------
-- Key Binding
keyboard :: State -> KeyboardMouseCallback
keyboard state (SpecialKey KeyUp)   _ _ _ = modRotate state KeyUp
keyboard state (SpecialKey KeyDown) _ _ _ = modRotate state KeyDown
keyboard state (SpecialKey KeyLeft) _ _ _ = modRotate state KeyLeft
keyboard state (SpecialKey KeyRight)_ _ _ = modRotate state KeyRight
keyboard _     (Char '\27')         _ _ _ = exitWith ExitSuccess
keyboard _     _                    _ _ _ = return ()


modRotate :: State -> SpecialKey -> IO ()
modRotate state KeyDown = do
  ph' state $~! (\x -> x - 5)
  postRedisplay Nothing
modRotate state KeyUp  = do
  ph' state $~! (+5)
  postRedisplay Nothing
modRotate state KeyRight = do
  th' state $~! (\x -> x - 5)
  postRedisplay Nothing
modRotate state KeyLeft = do
  th' state $~! (+5)
  postRedisplay Nothing


----------------------------------------------------------------------------------------------------------------
-- Misc sate modifiers
idle :: State -> IdleCallback
idle state = do

  ph <- get (ph' state)
  th <- get (th' state)
  gr <- get (gr' state)

  if gr > 360
    then gr' state $~! (\x -> 0)
    else gr' state $~! (+2)
  
  if ((-360) > ph || ph > 360)
    then ph' state $~! (\x -> 0)
    else postRedisplay Nothing

  if ((-360) > th || th > 360)
    then th' state $~! (\x -> 0)
    else postRedisplay Nothing

visible :: State -> Visibility -> IO ()
visible state Visible    = idleCallback $= Just (idle state)
visible _     NotVisible = idleCallback $= Nothing

reshape :: ReshapeCallback
reshape s@(Size width height) = do
  let wf = fromIntegral width
      hf = fromIntegral height

  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity  

  if width <= height
    then ortho (-1) 1 (-1) (hf/wf) (-500) (500:: GLdouble)
    else ortho (-1) (wf/hf) (-1) 1 (-500) (500:: GLdouble)
  matrixMode $= Modelview 0

  loadIdentity


----------------------------------------------------------------------------------------------------------------
-- Debug info
updateInfo :: State -> IO ()
updateInfo state = do 
  frames state $~! (+1)
  t0' <- get (t0 state)
  t <- get elapsedTime
  when (t - t0' >= 1000) $ do
    f <- get (frames state)
    ph <- get (ph' state)
    th <- get (th' state)
    gr <- get (gr' state)
    let seconds = fromIntegral (t - t0') / 1000 :: GLfloat
        fps = fromIntegral f / seconds
        result = ("[ph " ++ round2GL ph ++ "] [th " ++ round2GL th ++ "] [gr " ++ round2GL gr ++ "]", "")
    info state $= result
    t0 state $= t
    frames state $= 0



draw :: State -> IO ()
draw state = do
    
  clear [ ColorBuffer, DepthBuffer ]

  ph <- get (ph' state)
  th <- get (th' state)
  gr <- get (gr' state)
  info <- get (info state)
  
  loadIdentity

  scale 0.5 0.5 (0.5::GLfloat)

  rotate ph (Vector3 1 0 0)
  rotate th (Vector3 0 1 0)

  -- Set up perspective
  lookAt (Vertex3 0.1 0.0 0.1) (Vertex3 0 0 0) (Vector3 0 1 0)
  
  drawGrid 5
  
  drawStar 0.5 (0, 1.5, 0)

  drawStarCluster (10, 1, 3)
  drawStarCluster (10, 10, 1)
  drawStarCluster (1, 10, 10)

  drawStation 0.0 0.5 (1,0,0) (0,1,0)
  drawStation gr 0.35 ((-2),0,0) (0,0,1)

  drawFighter 0.5 (0.55, 0, 0)  (0,1,0)  ((-1), 0,0)
  drawFighter 0.7 (1, 0.7, 0)  (1,0,0)  (0,1,0)
  drawFighter 0.5 (0,1,1) (1,1,1) (0,1,0)

  preservingMatrix $ do
    glWindowPos 5 30
    renderString Helvetica18 $ (fst info)
    glWindowPos 5 5
    renderString Helvetica18 $ (snd info)

  swapBuffers
  updateInfo state
  reportErrors
  

myInit :: [String] -> State -> IO ()
myInit args state = do
  depthFunc $= Just Less

main :: IO ()
main = do
    initialWindowSize $= Size 800 800
    (_progName, args) <- getArgsAndInitialize
    initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]
    
    initialWindowPosition $= Position 500 500
    _window <- createWindow "Space Scene"

    state <- makeState
    myInit args state

    displayCallback $= draw state
    reshapeCallback $= Just reshape
    
    keyboardMouseCallback $= Just (keyboard state)
    visibilityCallback $= Just (visible state)
    mainLoop
  


