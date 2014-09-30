module Station (drawStation) where 
 
import Graphics.UI.GLUT

import GLUtils

import Cube
import Pyramid
  
drawStation :: GLfloat ->
               Float->
               (Float, Float, Float) ->
               (Float, Float, Float) -> IO ()
drawStation a s (x, y, z) (ux, uy, uz) = do

    let ws = s*0.6
        wd = 0.5
        cs = s*3
    
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do

        translate $ vector3f x y z
        scale3f s s s
        rotate a (Vector3 0 1 0)

        drawCube cs (0,0,0)
        -- Up
        drawPyramid s (0,(-0.75),0) (1,0,0) (0,1,0)
        -- Down
        drawPyramid s (0,0.75,0) (1,0,0) (0,(-1),0)
        
        -- Front
        drawPyramid ws (0,0,wd) (1,0,0) (0,0,(-1))
        -- Back
        drawPyramid ws (0,0,(-wd)) (1,0,0) (0,0,1)
        -- Right
        drawPyramid ws (wd,0,0) (0,1,0) ((-1),0,0)
        ---- Front
        drawPyramid ws ((-wd),0,0) (0,1,0) (1,0,0)
        
            
      