module Pyramid (drawPyramid) where 
 
import Graphics.UI.GLUT

import GLUtils

-- Draw solid pyramid
--  scale (s)
--  at (x,y,z)
--  nose towards (dx,dy,dz)
--  up towards (ux,uy,uz)
drawPyramid :: Float -> 
               (Float, Float, Float) -> 
               (Float, Float, Float) ->
               (Float, Float, Float) -> IO ()
drawPyramid s (x, y, z) (dx, dy, dz) (ux, uy, uz) = do

  let d0 = sqrt(dx*dx+dy*dy+dz*dz)
      x0 = dx/d0
      y0 = dy/d0
      z0 = dz/d0
      --  Unit vector in "up" direction
      d1 = sqrt(ux*ux+uy*uy+uz*uz);
      x1 = ux/d1
      y1 = uy/d1
      z1 = uz/d1
      -- Cross product gives the third vector
      x2 = y0*z1-y1*z0
      y2 = z0*x1-z1*x0
      z2 = x0*y1-x1*y0
  mat <- newMatrix RowMajor $ listf [x0, x1,  x2, 0,
                                     y0, y1,  y2, 0,
                                     z0, z1,  z2, 0,
                                     0,  0,   0,  1]


  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do

      translate $ vector3f x y z
      scale3f s s s
      multMatrix (mat :: GLmatrix GLfloat)
      
      renderPrimitive Triangles $ do

        --color3f (105/255) (105/255) (105/255)
        color3f (192/255) (192/255) (192/255)
        -- Front
        -- color3f 1 0 0
        drawVertex3f  0 1 0
        drawVertex3f (-1) (-1) 1
        drawVertex3f 1 (-1) 1
   
        -- Right
        -- color3f 0 0 1
        drawVertex3f 0 1 0
        drawVertex3f 1 (-1) 1
        drawVertex3f 1 (-1) (-1)
   
        -- Back
        -- color3f 0 1 0
        drawVertex3f 0 1 0
        drawVertex3f 1 (-1) (-1)
        drawVertex3f (-1) (-1) (-1)
   
        -- Left
        -- color3f 1 0 1
        drawVertex3f  0 1 0
        drawVertex3f (-1)(-1)(-1)
        drawVertex3f (-1) (-1) 1

      renderPrimitive Quads $ do

        color3f 0 0 1
        --color3f (105/255) (105/255) (105/255)
        drawVertex3f 1 (-1) 1
        drawVertex3f 1 (-1) (-1)
        drawVertex3f (-1) (-1) (-1)
        drawVertex3f (-1) (-1) 1

        
      
            
      