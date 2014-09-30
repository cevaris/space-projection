module Grid (drawGrid) where 
 
import Graphics.UI.GLUT

import GLUtils


----------------------------------------------------------------------------------------------------------------
-- Grid Object
gridPoints :: [(Float, Float, Float)]
gridPoints = [(0,0,0),(1,0,0), (0,0,0),(0,1,0), (0,0,0),(0,0,1)]

drawGrid :: GLfloat -> IO ()
drawGrid w = do
  objectList <- grid
  preservingMatrix $ do

    preservingAttrib [AllServerAttributes] $ do
      scale 1.0 1.0 (1.0::GLfloat)

      currentRasterPosition $= vertex4f 1 0 0 1
      renderString Helvetica18 $ "X"
      currentRasterPosition $= vertex4f 0 1 0 1
      renderString Helvetica18 $ "Y"
      currentRasterPosition $= vertex4f 0 0 1 1
      renderString Helvetica18 $ "Z"
      currentRasterPosition $= vertex4f 0 0 0 1

      preservingAttrib [AllServerAttributes] $ do
        lineWidth $= 2
        color3f 0 0 1
        callList objectList
    
 
grid :: IO DisplayList
grid = do
  objectList <- defineNewList Compile $ do
    renderPrimitive Lines $ do
      mapM_ (\(x, y, z) -> drawVertex3f x y z ) gridPoints
  return objectList