module Star (drawStar) where 
 
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Objects

import GLUtils

import Sphere

-- Draw solid pyramid
--  scale (s)
--  at (x,y,z)
drawStar :: Float-> (Float, Float, Float) -> IO ()
drawStar s (x, y, z) = do
  let radius = 1.0
      slices = 100
      stacks = 100

  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do    
      color3f 1 1 0
    
      translate $ vector3f x y z
      scale3f s s s
    
      drawSphere s 0.5 (0,0,0)