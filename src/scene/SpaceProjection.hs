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

data ChangeDirection = Increase | Decrease deriving (Show)

data ProjectionView = PerspectiveView | OrthogonalView | FirstPersonView deriving (Show, Eq)

data Direction = UpDirection | DownDirection | LeftDirection | RightDirection deriving (Show, Eq)

fraction  = 0.1

data State = State {
   frames  :: IORef Int,
   t0      :: IORef Int,
   ph'     :: IORef Float,
   th'     :: IORef Float,
   gr'     :: IORef Float,
   asp     :: IORef Float,
   fov     :: IORef Float,
   dim     :: IORef Float,
   angle   :: IORef Float,
   lx      :: IORef Float,
   lz      :: IORef Float,
   vx      :: IORef Float,
   vz      :: IORef Float,
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
  fv <- newIORef 55
  as <- newIORef 1
  di <- newIORef 2
  an <- newIORef 0
  xx <- newIORef 0
  zz <- newIORef (-1.0)
  xxx <- newIORef 0
  zzz <- newIORef 5
  pr <- newIORef FirstPersonView
  i  <- newIORef ("","")
  return $ State {  
    frames = f, t0 = t, ph' = ph, th' = th, gr' = gr, asp = as, fov = fv, dim = di, 
    angle = an, lx = xx, lz = zz, vx = xxx, vz = zzz,
    proj = pr, info = i
  }

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

keyboard state (Char 'f')           _ _ _ = modFov state Decrease
keyboard state (Char 'F')           _ _ _ = modFov state Increase

keyboard state (Char '1')           _ _ _ = modProjection state FirstPersonView
keyboard state (Char '2')           _ _ _ = modProjection state PerspectiveView
keyboard state (Char '3')           _ _ _ = modProjection state OrthogonalView
keyboard _     (Char '\27')         _ _ _ = exitWith ExitSuccess
keyboard _     _                    _ _ _ = return ()


modRotate :: State -> SpecialKey -> IO ()
modRotate state KeyDown = do
  proj' <- get (proj state)
  if proj' == FirstPersonView
    then do
      lx' <- get (lx state)
      lz' <- get (lz state)
      vx state $~! (\x -> x - (lx'*fraction))
      vz state $~! (\x -> x - (lz'*fraction))
    else  ph' state $~! (\x -> x - 5)
modRotate state KeyUp  = do
  proj' <- get (proj state)
  if proj' == FirstPersonView
    then do
      lx' <- get (lx state)
      lz' <- get (lz state)
      vx state $~! (+(lx'*fraction))
      vz state $~! (+(lz'*fraction))
    else ph' state $~! (+5)
modRotate state KeyRight = do
  proj' <- get (proj state)
  if proj' == FirstPersonView
    then do
      angle state $~! (+0.05)
      angle' <- get (angle state)
      lx state $~! (\x -> sin(angle'))
      lz state $~! (\x -> (-cos(angle')))
    else th' state $~! (\x -> x - 5)
modRotate state KeyLeft = do
  proj' <- get (proj state)
  if proj' == FirstPersonView
    then do
      angle state $~! (\x -> x - 0.05)
      angle' <- get (angle state)
      lx state $~! (\x -> sin(angle'))
      lz state $~! (\x -> (-cos(angle')))
    else th' state $~! (+5)


modFov :: State -> ChangeDirection -> IO ()
modFov state Decrease = do
  fov state $~! (\x -> x - 2)
  postRedisplay Nothing
modFov state Increase = do
  fov state $~! (+2)
  postRedisplay Nothing  


modDim :: State -> ChangeDirection -> IO ()
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
  -- Redraw scene
  s <- get windowSize
  reshape state s




idle :: State -> IdleCallback
idle state = do

  ph  <- get (ph' state)
  th  <- get (th' state)
  gr  <- get (gr' state)
  dim' <- get (dim state)
  fov' <- get (fov state)

  if fov' < 55
    then fov state $~! (\x -> 55)
    else postRedisplay Nothing

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
projectView state PerspectiveView = do
  fov <- get (fov state)
  asp <- get (asp state)
  dim <- get (dim state)
  setPerspective fov asp (dim/4) (dim*4)
projectView state FirstPersonView = do
  fov <- get (fov state)
  asp <- get (asp state)
  dim <- get (dim state)
  setPerspective fov asp (dim/4) (dim*4)
  
reshape :: State -> ReshapeCallback
reshape state s@(Size width height) = do

  viewport   $= (Position 0 0, s)

  matrixMode $= Projection
  loadIdentity

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


setProjectionView :: State -> ProjectionView -> IO ()
setProjectionView state PerspectiveView = do
  ph <- get (ph' state)
  th <- get (th' state)
  dim <- get (dim state)
  let ex = (-2)*dim*sin(toDeg(th))*cos(toDeg(ph))
      ey =    2*dim               *sin(toDeg(ph))
      ez =    2*dim*cos(toDeg(th))*cos(toDeg(ph))
  setLookAt (ex,ey,ez) (0,0,0) (0,cos(toDeg(ph)),0)
setProjectionView state OrthogonalView = do
  ph <- get (ph' state)
  th <- get (th' state)
  rotate (fToGL(ph)) (Vector3 1 0 0)
  rotate (fToGL(th)) (Vector3 0 1 0)
setProjectionView state FirstPersonView = do
  vx' <- get (vx state)
  vz' <- get (vz state)
  lx' <- get (lx state)
  lz' <- get (lz state)
  setLookAt (vx',0,vz') ((vx'+lx'),0,(vz'+lz')) (0,1,0)

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

  setProjectionView state proj'
  
  drawGrid 5
  
  drawStar 0.5 (0, 1.5, 0)

  drawStarCluster (5, 1, 3)
  drawStarCluster (5, 5, 1)
  drawStarCluster (1, 5, 5)

  drawStation 0.0 0.5 (1,0,0) (0,1,0)
  drawStation (fToGL(gr)) 0.35 ((-1),0,0) (0,0,1)

  drawFighter 0.5 (0.55, 0, 0) (0,1,0) ((-1), 0,0)
  drawFighter 0.7 (1, 0.7, 0) (1,0,0) (0,1,0)
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
    
    --initialWindowPosition $= Position 500 500
    _window <- createWindow "Space Scene Projection - Adam Cardenas"

    state <- makeState
    myInit args state

    displayCallback $= draw state
    reshapeCallback $= Just (reshape state)
    
    keyboardMouseCallback $= Just (keyboard state)
    visibilityCallback $= Just (visible state)
    mainLoop
  


