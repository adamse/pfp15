-- |

module RepaFilter where

import Prelude hiding (map)

-- from repa
import Data.Array.Repa
import Data.Array.Repa.Stencil
import Data.Array.Repa.Specialised.Dim2

-- from repa-io
import Data.Array.Repa.IO.BMP
import Data.Word (Word8)


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

-- | using luminance preserving greyscale conversion, a simple filter
greyscale :: Monad m => Array U DIM2 (Double,Double,Double) -> m (Array U DIM2 Double)
greyscale image =
  computeP (map (\(r,g,b) -> 0.2126 * r + 0.7152 * g + 7.22e-2 * b) image)


-- * Gaussian blur

{- a simple blur -}

simpleBlurStencil :: Int -> Stencil DIM2 Double
simpleBlurStencil r =
  makeStencil
    extent
    (\ix ->
       if isInside2 extent ix
          then Just coeff
          else Nothing)
  where coeff = recip (fromIntegral r ** 2)
        extent = ix2 r r


{-
apply a gaussian blur to an image we use a Repa construct called a `Stencil`, a
-}


-- | creates a gaussian kernel with radius and sigma, radius should be odd.
gaussianStencil :: Double -> Int -> Stencil DIM2 Double
gaussianStencil sigma r =
  makeStencil
    ext
    (\ix@(Z :. x :. y) ->
       if isInside2 ext ix
          then Just (recip (pi * s2) *
                     exp (-(fromIntegral x ** 2 + fromIntegral y ** 2) /
                           s2))
          else Nothing)
  where s2 = 2 * sigma ** 2
        ext = ix2 r r
