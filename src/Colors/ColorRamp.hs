{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Colors.ColorRamp
  (colorRamp)
  where
import           Colors.Palettes
import           Data.Map.Strict              ((!))
import           Data.Vector.Unboxed          (fromList, Unbox)
import           Data.Vector.Unboxed.Deriving
import           Graphics.Rendering.OpenGL.GL (Color4 (..), GLfloat)
import           Numeric.Tools.Interpolation  (at, cubicSpline, tabulate)
import           Numeric.Tools.Mesh           (uniformMesh)

derivingUnbox "Color4"
    [t| forall a . (Unbox a) => Color4 a -> (a, a, a, a) |]
    [| \(Color4 r g b alpha) -> (r, g, b, alpha) |]
    [| \(r, g, b, alpha) -> Color4 r g b alpha |]

colorRamp' :: String -> Int -> Palette
colorRamp' paletteName n =
  map (\x -> [tbl_r `at` x, tbl_g `at` x, tbl_b `at` x]) ats
  where
    palette = palettes ! paletteName
    umesh = uniformMesh (0,1) (length palette)
    r = fromList $ map head palette
    g = fromList $ map (!! 1) palette
    b = fromList $ map (!! 2) palette
    tab_r = tabulate umesh r
    tab_g = tabulate umesh g
    tab_b = tabulate umesh b
    ats = [frac i (n-1) | i <- [0 .. n-1]]
      where
        frac p q = realToFrac p / realToFrac q
    tbl_r = cubicSpline tab_r
    tbl_g = cubicSpline tab_g
    tbl_b = cubicSpline tab_b

rgbToColor4 :: [Double] -> Color4 GLfloat
rgbToColor4 x =
  Color4 (realToFrac (x!!0)) (realToFrac (x!!1)) (realToFrac (x!!2)) 1

colorRamp :: String -> Int -> [Color4 GLfloat]
colorRamp paletteName n = map rgbToColor4 (colorRamp' paletteName n)
