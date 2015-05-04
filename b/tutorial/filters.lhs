\documentclass[14pt]{article}

%include polycode.fmt
%format . = "."

\usepackage{graphicx}
\usepackage{fullpage}
\usepackage{amssymb,amsmath}
\usepackage[unicode=true]{hyperref}
\usepackage{fixltx2e}
\usepackage[T1]{fontenc}
\usepackage[utf8]{inputenc}

\title{Image filtering in Repa 3}
\author{Adam Sandberg Eriksson \and Andreas Svanstr\"om}

\begin{document}

\maketitle

\input{intro.tex}

\section{Image filters with Repa}
%if False
\begin{code}
{-# LANGUAGE RankNTypes #-}
\end{code}
%endif

We start with some imports. First we need to hide some prelude
imports:
\begin{code}
import Prelude hiding (map, zip3, unzip3)
import Data.Word (Word8)
\end{code}
And then we need some imports from the \texttt{repa} and \texttt{repa-io} packages
\begin{code}
-- from repa
import Data.Array.Repa
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import Data.Array.Repa.Repr.Unboxed
-- from repa-io
import Data.Array.Repa.IO.BMP
\end{code}

\texttt{repa-io} will read \texttt{BMP} files into a triple of
\texttt{Word8}'s but mostly we wish to work with numbers in the range
$[0, 1]$, so we create some convenience functions \texttt{bmpToDouble}
and \texttt{doubleToBmp} to help us.

\begin{code}
type Image = Array U DIM2 (Double,Double,Double)
type Filter = forall m. Monad m => Image -> m Image

-- convert a rgb in [0, 255] to [0, 1]
bmpToDouble :: Monad m
            => Array U DIM2 (Word8,Word8,Word8)
            -> m Image
bmpToDouble image =
  computeP (map (\(r,g,b) ->
                   (toDouble r,toDouble g,toDouble b))
                image)
  where toDouble c =
          fromRational (toRational c / 255)

-- convert from [0,1] to [0,255]
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
\end{code}

We define \texttt{applyFilter} to read a \texttt{BMP} file, apply a
filter and write to a new file:

\begin{code}
applyFilter :: Filter -> FilePath -> FilePath -> IO ()
applyFilter filter inFile outFile =
  do im <-
       fmap (either (error . show) id)
            (readImageFromBMP inFile)
     im' <- bmpToDouble im
     filtered <- filter im'
     outIm <- doubleToBmp filtered
     writeImageToBMP outFile outIm
\end{code}

We begin with a simple filter: converting an image to grey scale. A
nice way to translate colour into grey is to use a linear combination
of the red, green and blue but deemphasising the red and the blue
slightly, we copy the resulting grey scale to all three channels. In
Repa we write:
\begin{code}
greyscale :: Filter
greyscale image =
  do im <-
       computeP (map (\(r,g,b) -> 0.2126 * r + 0.7152 * g + 7.22e-2 * b) image)
     return (zip3 im im im)
\end{code}

The filter has the effect we are looking for:

\begin{figure}[h!]
  \centering
  \includegraphics{FLAG_B24.png}\quad\includegraphics{FLAG_B24_GREY.png}
  \caption{Image before and after greyscale conversion}
\end{figure}

\newpage

To make more interesting filters we will use a Repa feature called
\texttt{Stencil}s, which are used to apply a matrix of coefficients to
a pixel and its neighbours.

Such filters that are used to decrease the noise in images are called
blur filters, first we implement a uniform blur filter that sums a
pixel and its neighbours with equal weight:
\begin{code}
simpleBlurStencil :: Int -> Stencil DIM2 Double
simpleBlurStencil r =
  makeStencil
    (rToExtent r)
    (\ix ->
       if inside r ix
          then Just coeff
          else Nothing)
  where coeff = recip (fromIntegral (2*r+1) ** 2)
\end{code}
To simplify the application of the stencil to the three channels of
the image we define
\begin{code}
applyStencil :: Monad m
             => Stencil DIM2 Double -> Image -> m Image
applyStencil stencil image =
  do let (r,g,b) = unzip3 image
     [r',g',b'] <-
       mapM (computeP . appSt)
            [r,g,b]
     return (zip3 r' g' b')
  where appSt = mapStencil2 BoundClamp stencil
\end{code}

Finally we apply the uniform blur filter to an image
\begin{figure}[h!]
  \centering
  \includegraphics[width=0.45\textwidth]{XING_B24.png}\quad\includegraphics[width=0.45\textwidth]{XING_B24_UNIFORM.png}
  \caption{Image before and after an uniform blur}
\end{figure}

As we can see the image is very blurry and not very nice. A better
blur filter is a Gaussian blur filter, it uses a 2 dimensional
Gaussian to compute the stencil, this way pixels that are
further away from the origin pixel get less weight and we blur the
image in a nicer way.

We start with the Gaussian
\begin{code}
gaussian2d :: Double -> Int -> Int -> Double
gaussian2d sigma x y =
  (recip (pi * s2) *
   exp (-(fromIntegral x ** 2 + fromIntegral y ** 2) / s2))
  where s2 = 2 * sigma ** 2
{-# INLINE gaussian2d #-}
\end{code}
and for the stencil we compute the the Gaussian at each coordinate
\begin{code}
gaussianStencil :: Double -> Int -> Stencil DIM2 Double
gaussianStencil sigma r =
  makeStencil
    (rToExtent r)
    (\ix@(Z :. x :. y) ->
       if inside r ix
          then Just (gaussian2d sigma x y)
          else Nothing)
\end{code}

Using this blur stencil we get a much nicer effect:
\begin{figure}[h!]
  \centering
  \includegraphics[width=0.45\textwidth]{XING_B24.png}\quad\includegraphics[width=0.45\textwidth]{XING_B24_BLUR.png}
  \caption{Image before and after a Gaussian blur}
\end{figure}

\end{document}
