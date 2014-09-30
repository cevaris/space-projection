module Cube (drawCube) where 
 
import Graphics.UI.GLUT

import GLUtils


drawCube :: Float -> (Float, Float, Float) -> IO ()
drawCube s (x, y, z) = do
    cubeDisplayList <- (cube 0.25)
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        translate $ vector3f x y z
        scale3f s s s
        callList cubeDisplayList
 
cube :: GLfloat -> IO DisplayList
cube w = do

  cubeDisplayList <- defineNewList Compile $ do
    renderPrimitive Quads $ do

      --color3f (224/255) (223/255) (219/255)
      color3f (105/255) (105/255) (105/255)
      --color3f 0 0 1
      vertex $ Vertex3 w w w
      vertex $ Vertex3 w w (-w)
      vertex $ Vertex3 w (-w) (-w)
      vertex $ Vertex3 w (-w) w

      --color3f 0 1 0
      vertex $ Vertex3 w w w
      vertex $ Vertex3 w w (-w)
      vertex $ Vertex3 (-w) w (-w)
      vertex $ Vertex3 (-w) w w
      
      --color3f 1 0 0
      vertex $ Vertex3 w w w
      vertex $ Vertex3 w (-w) w
      vertex $ Vertex3 (-w) (-w) w
      vertex $ Vertex3 (-w) w w
      
      --color3f 1 0 1
      vertex $ Vertex3 (-w) w w
      vertex $ Vertex3 (-w) w (-w)
      vertex $ Vertex3 (-w) (-w) (-w)
      vertex $ Vertex3 (-w) (-w) w
      
      --color3f 0 1 1
      vertex $ Vertex3 w (-w) w
      vertex $ Vertex3 w (-w) (-w)
      vertex $ Vertex3 (-w) (-w) (-w)
      vertex $ Vertex3 (-w) (-w) w
      
      --color3f 1 1 0
      vertex $ Vertex3 w w (-w)
      vertex $ Vertex3 w (-w) (-w)
      vertex $ Vertex3 (-w) (-w) (-w)
      vertex $ Vertex3 (-w) w (-w)

  return cubeDisplayList