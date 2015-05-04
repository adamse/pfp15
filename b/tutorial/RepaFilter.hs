{-# LANGUAGE RankNTypes #-}
-- |

module RepaFilter where

import Prelude hiding (map, unzip3, zip3)
import Data.Word (Word8)

-- from repa
import Data.Array.Repa
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import Data.Array.Repa.Repr.Unboxed
-- from repa-io
import Data.Array.Repa.IO.BMP

type Image = Array U DIM2 (Double,Double,Double)
type Filter = forall m. Monad m => Image -> m Image

-- | convert a rgb in [0, 255] to [0, 1]
bmpToDouble :: Monad m
            => Array U DIM2 (Word8,Word8,Word8)
            -> m Image
bmpToDouble image =
  computeP (map (\(r,g,b) ->
                   (toDouble r,toDouble g,toDouble b))
                image)
  where toDouble c =
          fromRational (toRational c / 255)

-- | convert from [0,1] to [0,255]
doubleToBmp :: Monad m
            => Image
            -> m (Array U DIM2 (Word8,Word8,Word8))
doubleToBmp image =
  computeP (map (\(r,g,b) ->
                   (toWord r,toWord g,toWord b))
                image)
  where toWord c = round (c * 255)

-- check if index is inside radius
inside :: Int -> DIM2 -> Bool
inside r (Z :. x :. y) = abs x <= r && abs y <= y
{-# INLINE inside #-}

-- calulcate the extent from a radius
rToExtent :: Int -> DIM2
rToExtent r = ix2 rr rr
  where rr = 2 * r + 1
{-# INLINE rToExtent #-}


applyFilter :: Filter -> FilePath -> FilePath -> IO ()
applyFilter filter inFile outFile =
  do im <-
       fmap (either (error . show) id)
            (readImageFromBMP inFile)
     im' <- bmpToDouble im
     filtered <- filter im'
     outIm <- doubleToBmp filtered
     writeImageToBMP outFile outIm

-- | using luminance preserving greyscale conversion, a simple filter
greyscale :: Filter
greyscale image =
  do im <-
       computeP (map (\(r,g,b) -> 0.2126 * r + 0.7152 * g + 7.22e-2 * b) image)
     return (zip3 im im im)

runGreyscale :: IO ()
runGreyscale =
  applyFilter greyscale "FLAG_B24.BMP" "FLAG_B24_GREY.BMP"

-- * Gaussian blur

{- a simple blur -}

uniformStencil :: Int -> Stencil DIM2 Double
uniformStencil r =
  makeStencil
    (rToExtent r)
    (\ix ->
       if inside r ix
          then Just coeff
          else Nothing)
  where coeff = recip (fromIntegral (2*r+1) ** 2)


runUniform :: IO ()
runUniform =
  applyFilter (applyStencil (uniformStencil 3))
    "XING_B24.BMP"
    "XING_B24_UNIFORM.BMP"

{-
apply a gaussian blur to an image we use a Repa construct called a `Stencil`, a
-}

applyStencil :: Monad m
             => Stencil DIM2 Double -> Image -> m Image
applyStencil stencil image =
  do let (r,g,b) = unzip3 image
     [r',g',b'] <-
       mapM (computeP . appSt)
            [r,g,b]
     return (zip3 r' g' b')
  where appSt = mapStencil2 BoundClamp stencil


-- | creates a gaussian kernel with radius and sigma, radius should be odd.
gaussianStencil :: Double -> Int -> Stencil DIM2 Double
gaussianStencil sigma r =
  makeStencil
    (rToExtent r)
    (\ix@(Z :. x :. y) ->
       if inside r ix
          then Just (gaussian2d sigma x y)
          else Nothing)

gaussian2d :: Double -> Int -> Int -> Double
gaussian2d sigma x y =
  (recip (pi * s2) *
   exp (-(fromIntegral x ** 2 + fromIntegral y ** 2) / s2))
  where s2 = 2 * sigma ** 2
{-# INLINE gaussian2d #-}

runGaussian :: IO ()
runGaussian =
  applyFilter (applyStencil (gaussianStencil 0.83 3))
    "XING_B24.BMP"
    "XING_B24_BLUR.BMP"
