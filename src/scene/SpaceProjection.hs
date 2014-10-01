import Control.Monad ( when )
import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess), exitFailure )

import Graphics.UI.GLUT

import GLUtils
import Cube
import Star
import Grid
import StarCluster
import Fighter
import Pyramid
import Station
import Sphere

----------------------------------------------------------------------------------------------------------------
-- Global State
--type View = (GLfloat, GLfloat, GLfloat)

data Zoom = In | Out deriving (Show)
data ModDirection = Increase | Decrease deriving (Show)

data ProjectionView = PerspectiveView | OrthogonalView | FirstPersonView deriving (Show, Eq)

zoomDelta = 5e-4

data State = State {
   frames  :: IORef Int,
   t0      :: IORef Int,
   ph'     :: IORef Float,
   th'     :: IORef Float,
   gr'     :: IORef Float,
   asp     :: IORef Float,
   fov     :: IORef Float,
   dim     :: IORef Float,
   proj    :: IORef ProjectionView,
   info    :: IORef (String,String)
 }

makeState :: IO State
makeState = do
  f  <- newIORef 0
  t  <- newIORef 0
  ph <- newIORef 0
  th <- newIORef 0
  gr <- newIORef 0
  fv <- newIORef 100
  as <- newIORef 1
  di <- newIORef 2
  pr <- newIORef OrthogonalView
  --pr <- newIORef PerspectiveView
  i  <- newIORef ("","")
  return $ State {  frames = f, t0 = t, ph' = ph, th' = th, gr' = gr, asp = as, fov = fv, dim = di, proj = pr, info = i }

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
keyboard state (Char 'd')           _ _ _ = modDim state Decrease
keyboard state (Char 'D')           _ _ _ = modDim state Increase
keyboard state (Char '1')           _ _ _ = modProjection state PerspectiveView
keyboard state (Char '2')           _ _ _ = modProjection state OrthogonalView
keyboard state (Char '3')           _ _ _ = modProjection state FirstPersonView
keyboard _     (Char '\27')         _ _ _ = exitWith ExitSuccess
keyboard _     _                    _ _ _ = return ()


modDim :: State -> ModDirection -> IO ()
modDim state Decrease = do
  dim state $~! (\x -> x - 0.1)
  postRedisplay Nothing
modDim state Increase = do
  dim state $~! (+0.1)
  postRedisplay Nothing  


modProjection :: State -> ProjectionView -> IO ()
modProjection state proj' = do
  -- Update PerspectiveView state
 
  proj state $~! (\x -> proj')
  -- Render new state
  projectView state proj'

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

  ph  <- get (ph' state)
  th  <- get (th' state)
  gr  <- get (gr' state)
  dim' <- get (dim state)

  if dim' < 1
    then dim state $~! (\x -> 1)
    else postRedisplay Nothing

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

projectView :: State -> ProjectionView -> IO ()
projectView state OrthogonalView  = do
  dim <- get (dim state)
  asp <- get (asp state)
  setOrtho ((-asp)*dim) (asp*dim) (-dim) dim (-dim) dim
  putStrLn $ show OrthogonalView

projectView state PerspectiveView = do
  fov <- get (fov state)
  asp <- get (asp state)
  dim <- get (dim state)
  setPerspective fov asp (dim/4) (dim*4)

  --(Size width height) <- get windowSize
  --perspective 100 (fromIntegral width / fromIntegral height) 1 500
  putStrLn $ show PerspectiveView



  
  

reshape :: State -> ReshapeCallback
reshape state s@(Size width height) = do

  viewport   $= (Position 0 0, s)

  matrixMode $= Projection
  loadIdentity

  -- Update Projection
  proj' <- get (proj state)
  projectView state proj'

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
    asp <- get (asp state)
    fov <- get (fov state)
    dim <- get (dim state)
    proj <- get (proj state)
    let seconds = fromIntegral (t - t0') / 1000 :: GLfloat
        fps = fromIntegral f / seconds
        result = ("[ph " ++ round2 ph ++ "] [th " ++ round2 th ++ "] [gr " ++ round2 gr ++ "]",
                  "[proj " ++ show proj ++ "] [asp " ++ show asp ++  "] [fov " ++ show fov ++  "] [ dim" ++ show dim ++  "] ")
    info state $= result
    t0 state $= t
    frames state $= 0



draw :: State -> IO ()
draw state = do
    
  clear [ ColorBuffer, DepthBuffer ]

  ph <- get (ph' state)
  th <- get (th' state)
  gr <- get (gr' state)
  dim <- get (dim state)
  proj' <- get (proj state)
  info <- get (info state)



  loadIdentity

  projectView state proj'

  --scale 0.5 0.5 (0.5::GLfloat)


  -- Set up perspective
  if proj' == PerspectiveView
    then do
      let ex = (-2)*dim*sin(th)*cos(ph)
          ey =    2*dim        *sin(ph)
          ez =    2*dim*cos(th)*cos(ph)
      setLookAt (ex,ey,ez) (0,0,0) (0,cos(ph),0)
      putStrLn $ show ex ++ " " ++ show ey ++ " " ++ show ez
    else do
      rotate (fToGL(ph)) (Vector3 1 0 0)
      rotate (fToGL(th)) (Vector3 0 1 0)
  
  drawGrid 5
  
  drawStar 0.5 (0, 1.5, 0)

  drawStarCluster (10, 1, 3)
  drawStarCluster (10, 10, 1)
  drawStarCluster (1, 10, 10)

  drawStation 0.0 0.5 (1,0,0) (0,1,0)
  drawStation (fToGL(gr)) 0.35 ((-2),0,0) (0,0,1)

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
    _window <- createWindow "Space Scene Projection - Adam Cardenas"

    state <- makeState
    myInit args state

    displayCallback $= draw state
    --reshapeCallback $= Just (reshape state)
    reshapeCallback $= Just (reshape state)
    
    keyboardMouseCallback $= Just (keyboard state)
    visibilityCallback $= Just (visible state)
    mainLoop
  


