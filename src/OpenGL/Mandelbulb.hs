module OpenGL.Mandelbulb
  ( main
  ) where
import           Colors.ColorRamp
import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( when )
import qualified Data.ByteString               as B
import           Data.IORef
import           Data.Vector.Unboxed            ( (!)
                                                , Vector
                                                )
import qualified Data.Vector.Unboxed           as V
import           Graphics.Rendering.OpenGL.Capture
                                                ( capturePPM )
import           Graphics.Rendering.OpenGL.GL
                                         hiding ( Color )
import           Graphics.UI.GLUT        hiding ( Color )
import           MarchingCubes
import           System.Directory               ( doesDirectoryExist )
import           System.IO.Unsafe
import           Text.Printf

white, black :: Color4 GLfloat
white = Color4 1 1 1 1
black = Color4 0 0 0 1

data Context = Context
  { contextRot1 :: IORef GLfloat
  , contextRot2 :: IORef GLfloat
  , contextRot3 :: IORef GLfloat
  , contextZoom :: IORef Double
  }

type F = Float
type Color = Color4 GLfloat

fun :: XYZ F -> F
fun p0@(x0,y0,z0) = go 10 p0 (ssq p0) 1
  where
  ssq (x,y,z) = x*x + y*y + z*z
  go :: Int -> XYZ F -> F -> F -> F
  go n (x,y,z) r2 dr =
    if r2 > 4
      then sqrt r2 * log r2 / dr
      else
        let theta = 8 * atan2 (sqrt(x*x+y*y)) z in
        let phi = 8 * atan2 y x in
        let r = sqrt r2 in
        let r7 = r2*r2*r2*r in 
        let dr' = 8 * r7 * dr + 1 in
        let r8 = r7*r in
        let xyz = ( r8 * cos phi * sin theta + x0
                  , r8 * sin phi * sin theta + y0
                  , r8 * cos theta + z0) in
        let r2' = ssq xyz in
        if n>1 then go (n-1) xyz r2' dr' else sqrt r2' * log r2' / dr'

voxel :: Voxel F
voxel = makeVoxel fun ((-1.2, 1.2), (-1.2, 1.2), (-1.2, 1.2)) (300, 300, 300)

mandelbulb :: Mesh F
mandelbulb = makeMesh voxel 0.01

vertices :: Vector (XYZ F)
vertices = _vertices mandelbulb

faces :: [(Int, Int, Int)]
faces = _faces mandelbulb

normals :: Vector (XYZ F)
normals = _normals mandelbulb

funColor :: F -> F -> F -> Color
funColor dmin dmax d = clrs !! j
 where
  clrs = colorRamp "plasma" 256
  j    = floor ((d - dmin) * 255 / (dmax - dmin))

colors :: Vector Color
colors = V.map (funColor dmin dmax) ds
 where
  ds   = V.map (\(x, y, z) -> x * x + y * y + z * z) vertices
  dmin = V.minimum ds
  dmax = V.maximum ds

triangle
  :: (Int, Int, Int)
  -> ((XYZ F, XYZ F, XYZ F), (XYZ F, XYZ F, XYZ F), (Color, Color, Color))
triangle face =
  ( (vertices ! i, vertices ! j, vertices ! k)
  , (normals ! i , normals ! j , normals ! k)
  , (colors ! i  , colors ! j  , colors ! k)
  )
 where
  (i, j, k) = face

triangles
  :: [((XYZ F, XYZ F, XYZ F), (XYZ F, XYZ F, XYZ F), (Color, Color, Color))]
triangles = map triangle faces

display :: Context -> DisplayCallback
display context = do
  clear [ColorBuffer, DepthBuffer]
  r1        <- get (contextRot1 context)
  r2        <- get (contextRot2 context)
  r3        <- get (contextRot3 context)
  zoom      <- get (contextZoom context)
  (_, size) <- get viewport
  loadIdentity
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Triangles $ mapM_ drawTriangle triangles
  swapBuffers
 where
  drawTriangle ((v1, v2, v3), (n1, n2, n3), (c1, c2, c3)) = do
    normal (toNormal n1)
    materialDiffuse Front $= c1
    vertex (toVertex v1)
    normal (toNormal n2)
    materialDiffuse Front $= c2
    vertex (toVertex v2)
    normal (toNormal n3)
    materialDiffuse Front $= c3
    vertex (toVertex v3)
   where
    toNormal (x, y, z) = Normal3 x y z
    toVertex (x, y, z) = Vertex3 x y z

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (realToFrac w / realToFrac h) 1.0 100.0
  lookAt (Vertex3 0 (-3 + zoom) 0) (Vertex3 0 0 0) (Vector3 0 0 1)
  matrixMode $= Modelview 0

keyboard
  :: IORef GLfloat
  -> IORef GLfloat
  -> IORef GLfloat -- rotations
  -> IORef Double  -- zoom
  -> IORef Bool    -- animation
  -> IORef Int     -- animation delay
  -> IORef Bool    -- save animation
  -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom anim delay save c _ = do
  case c of
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+ 2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+ 2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+ 2)
    'm' -> zoom $~! (+ 0.25)
    'l' -> zoom $~! subtract 0.25
    'a' -> anim $~! not
    'o' -> delay $~! (+ 10000)
    'p' -> delay $~! (\d -> if d == 0 then 0 else d - 10000)
    's' -> save $~! not
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing

ppmExists :: Bool
{-# NOINLINE ppmExists #-}
ppmExists = unsafePerformIO $ doesDirectoryExist "./ppm"

idle
  :: IORef Bool
  -> IORef Int
  -> IORef Bool
  -> IORef Int
  -> IORef GLfloat
  -> IdleCallback
idle anim delay save snapshots rot3 = do
  a        <- get anim
  snapshot <- get snapshots
  s        <- get save
  when a $ do
    d <- get delay
    when (s && ppmExists && snapshot < 180) $ do
      let ppm = printf "ppm/pic%04d.ppm" snapshot
      (>>=) capturePPM (B.writeFile ppm)
      print snapshot
      snapshots $~! (+ 1)
    rot3 $~! (+ 2)
    _ <- threadDelay d
    postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Mandelbulb"
  windowSize $= Size 512 512
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient Front $= black
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 (-100) 0 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
  cullFace $= Just Back
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  displayCallback $= display Context { contextRot1 = rot1
                                     , contextRot2 = rot2
                                     , contextRot3 = rot3
                                     , contextZoom = zoom
                                     }
  reshapeCallback $= Just (resize 0)
  anim      <- newIORef False
  delay     <- newIORef 0
  save      <- newIORef False
  snapshots <- newIORef 0
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom anim delay save)
  idleCallback $= Just (idle anim delay save snapshots rot3)
  putStrLn
    "*** Mandelbulb ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Animation: a\n\
        \    Animation speed: o, p\n\
        \    Save animation: s\n\
        \"
  mainLoop
