module Sphere (drawSphere) where 
 
import Graphics.UI.GLUT
import Data.Fixed

import GLUtils

spherePh:: Float -> [Float]
spherePh d = [ ph | ph <- [(-90.0)..90.0], ((mod' ph d) == 0 && ph < 90)]

sphereTh:: Float -> [Float]
sphereTh d = [th | th <- [0.0..360.0], (mod' th d) == 0]


drawLatBand :: Float -> (Float,Float) -> IO ()
drawLatBand d (ph, th) =  do

  drawVertex3f ((sin th)*(cos ph)) (sin ph) ((cos th)*(cos ph))
  drawVertex3f ((sin th)*(cos (ph+d))) (sin (ph+d)) ((cos th)*(cos (ph+d)))
  --print $ "[ph " ++ show ph ++ "] [th " ++ show th ++ "]" 


--Draw solid sphere
--  scale (s)
--  quality (q)
--  at (x,y,z)
drawSphere :: Float-> Float-> (Float, Float, Float) -> IO ()
drawSphere s q (x, y, z) = do

  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do

      translate $ vector3f x y z
      scale3f s s s
      color3f 1 (69/255) 0

      mapM_ (\ph -> do
        renderPrimitive QuadStrip $do
          mapM_ (\th -> drawLatBand q (ph, th)) (sphereTh q)
        ) (spherePh q)

        
            
      