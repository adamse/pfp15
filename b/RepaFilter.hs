{-# LANGUAGE RankNTypes #-}
-- |

module RepaFilter where

import           Prelude hiding (map)

import Control.Applicative

-- from repa
import           Data.Array.Repa
import           Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import           Data.Array.Repa.Specialised.Dim2
import qualified Data.Array.Repa.Repr.Unboxed as U


-- from repa-io
import           Data.Array.Repa.IO.BMP
import           Data.Word (Word8)


main :: IO ()
main = do undefined
          undefined

-- | convert a rgb in [0, 255] to [0, 1]
bmpToDouble :: Monad m
            => Array U DIM2 (Word8,Word8,Word8)
            -> m (Array U DIM2 (Double,Double,Double))
bmpToDouble image =
  computeP (map (\(r,g,b) ->
                   (toDouble r,toDouble g,toDouble b))
                image)
  where toDouble c =
          fromRational (toRational c / 255)

-- | convert from [0,1] to [0,255]
doubleToBmp :: Monad m
            => Array U DIM2 (Double,Double,Double)
            -> m (Array U DIM2 (Word8,Word8,Word8))
doubleToBmp image =
  computeP (map (\(r,g,b) ->
                   (toWord r,toWord g,toWord b))
                image)
  where toWord c = round (c * 255)

inside :: Int -> DIM2 -> Bool
inside r (Z :. x :. y) = abs x <= r && abs y <= y
{-# INLINE inside #-}

rToExtent :: Int -> DIM2
rToExtent r = ix2 rr rr
  where rr = 2 * r + 1
{-# INLINE rToExtent #-}

-- applyFilter :: (forall m. Monad m => Array U DIM2 (Double,Double,Double) -> m (Array U DIM2 (Double,Double,Double)))
--             -> FilePath
--             -> FilePath
--             -> IO ()
applyFilter filter inFile outFile =
  do im <-
       fmap (either (error . show) id)
            (readImageFromBMP inFile)
     im' <- bmpToDouble im
     filtered <- filter im'
     outIm <- doubleToBmp filtered
     writeImageToBMP outFile outIm

-- | using luminance preserving greyscale conversion, a simple filter
greyscale :: Monad m => Array U DIM2 (Double,Double,Double) -> m (Array U DIM2 Double)
greyscale image =
  computeP (map (\(r,g,b) -> 0.2126 * r + 0.7152 * g + 7.22e-2 * b) image)

applyGreyscale :: IO ()
applyGreyscale =
  applyFilter
    (\im ->
       do gim <- greyscale im
          return (U.zip3 gim gim gim))
    "FLAG_B24.BMP"
    "FLAG_B24_GREY.BMP"

-- * Gaussian blur

{- a simple blur -}

simpleBlurStencil :: Int -> Stencil DIM2 Double
simpleBlurStencil r =
  makeStencil
    (rToExtent r)
    (\ix ->
       if inside r ix
          then Just coeff
          else Nothing)
  where coeff = recip (fromIntegral r ** 2)


{-
apply a gaussian blur to an image we use a Repa construct called a `Stencil`, a
-}


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

gaussianFilter :: Monad m
               => Stencil DIM2 Double
               -> Array U DIM2 Double
               -> m (Array U DIM2 Double)
gaussianFilter stencil im = computeP (mapStencil2 BoundClamp stencil im)

applyGaussian :: IO ()
applyGaussian =
  applyFilter
    (\im ->
       do let (r,g,b) = U.unzip3 im
          U.zip3 <$>
            gaussianFilter st r <*>
            gaussianFilter st g <*>
            gaussianFilter st b)
    "XING_B24.BMP"
    "XING_B24_BLUR.BMP"
  where st = gaussianStencil 0.84 3

-- applyGreyscale :: IO ()
-- applyGreyscale =
--   applyFilter
--     (\im ->
--        do gim <- greyscale im
--           return (U.zip3 gim gim gim))
--     "FLAG_B24.BMP"
--     "FLAG_B24_GREY.BMP"
