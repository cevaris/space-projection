module GLUtils where

import Numeric

import Graphics.Rendering.OpenGL.Raw.ARB.WindowPos
import Graphics.UI.GLUT

type Point3 = (Float, Float, Float)

--toGfloat :: Float -> GLfloat
--toGfloat f = (realToFrac f)::GLfloat

fToGL :: Float -> GLfloat
fToGL f = (realToFrac f)::GLfloat

--glCos :: Float -> Float
--glCos x = cos(3.1415927/180*x)

--glSin :: Float -> Float
--glSin x = sin(3.1415927/180*x)

toDeg :: Float -> Float
toDeg x = x*(3.1415927/180)

drawVertex3f :: Float -> Float -> Float -> IO ()
drawVertex3f x y z = vertex $ vertex3f x y z

vertex3f :: Float -> Float -> Float -> Vertex3 GLfloat
vertex3f x y z = Vertex3 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat)

vertex3d :: Float -> Float -> Float -> Vertex3 GLdouble
vertex3d x y z = Vertex3 ((realToFrac x)::GLdouble) ((realToFrac y)::GLdouble) ((realToFrac z)::GLdouble)

vertex4f :: Float -> Float -> Float -> Float -> Vertex4 GLfloat
vertex4f x y z w = Vertex4 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat) ((realToFrac w)::GLfloat)

vector3f :: Float -> Float -> Float -> Vector3 GLfloat
vector3f x y z = Vector3 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat)

vector3d :: Float -> Float -> Float -> Vector3 GLdouble
vector3d x y z = Vector3 ((realToFrac x)::GLdouble) ((realToFrac y)::GLdouble) ((realToFrac z)::GLdouble)

scale3f :: Float -> Float -> Float -> IO ()
scale3f x y z = scale ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat)

color3f :: Float -> Float -> Float -> IO ()
color3f x y z = color (Color3 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat))

setPerspective :: Float -> Float -> Float -> Float -> IO ()
setPerspective fov aspect zNear zFar = perspective ((realToFrac fov)::GLdouble) ((realToFrac aspect)::GLdouble) ((realToFrac zNear)::GLdouble) ((realToFrac zFar)::GLdouble)

setOrtho :: Float -> Float -> Float -> Float -> Float -> Float -> IO ()
setOrtho left right bottom top nearVal farVal = ortho ((realToFrac left)::GLdouble) ((realToFrac right)::GLdouble) ((realToFrac bottom)::GLdouble) ((realToFrac top)::GLdouble) ((realToFrac nearVal)::GLdouble) ((realToFrac farVal)::GLdouble)

setLookAt :: Point3 -> Point3 -> Point3 -> IO ()
setLookAt (ex,ey,ez) (cx,cy,cz) (ux,uy,uz) = lookAt (vertex3d ex ey ez) (vertex3d cx cy cz) (vector3d ux uy uz)


glWindowPos :: GLfloat -> GLfloat -> IO ()
glWindowPos x y = glWindowPos2f x y

round2 :: Float -> String
round2 x = showFFloat (Just 2) x ""

round2GL :: GLfloat -> String
round2GL x = showGFloat (Just 2) x ""

listf :: [Float] -> [GLfloat]
listf ls = map (\x -> ((realToFrac x)::GLfloat)) ls