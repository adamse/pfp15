---
title: Image manipulation with Repa 3
author:
    - Adam Sandberg Eriksson
    - Andreas Svanström
...

# Introduction to Repa

## What is Repa

Repa is a library for parellelising computations using regular arrays,
the name REPA is an acronym for REgular PArallel arrays.

The cool thing with Repa is that as long as you run a program, written
using the repa combinators, with the flags +RTS -Nx (where x is the
number of threads), the program will automatically run the
combination computations in parallel.

The Repa arrays can be multidimensional and this is defined by
different shapes,
they also have the two options of being stored as Delayed or Unboxed
manifest. Delayed means that lazy evaluation will be used (i.e. the
array is built up of functions that are computed when the respective
values are needed), Unboxed manifest on the other hand means that the
actual values are computed and stored in the array. This means that
an array where a majority of the values will be accessed multiple
times probably should be stored as Unboxed manifest to improve
performance.


## How to use Repa

A repa array is defined as
    Array r sh a
where r can be either U or D, defining if the array is Unboxed
manifest or Delayed. The sh parameter defines the shape of the array,
i.e. the number of dimensions it has, and possibly their sizes. Lastly
the a parameter defines which element type the array will contain, so
if one would want to create a three-dimensional 2x3x3 delayed array
with Integer elements, one could define it as
    Array D (Z :. 2 :. 3 :. 3) Integer
If one wouldn't know yet how big each dimension should be, and would
want to create an unboxed three-dimensional array of doubles, it could
be defined as
    Array U DIM3 Double

To get the element from a certain index in an array, one uses the `!`
operator together with a shape, let's say that we have a two-dimensional
array called nums, and that we wanted the element at `(5, 0)`, then
we would get it by
    nums ! (Z :. 5 :. 0)
similar to
    nums !! 5 !! 0
would it be a normal Haskell list.

There are functions for easy creation of Repa arrays from Haskell
lists or functions. For example if we dould like to create a
one-dimensional unboxed array with the numbers 0-10, we would use the
`fromListUnboxed` function, which takes a shape and a list as arguments,
like this
    zeroToTen = fromListUnboxed (Z :. 11) [0..10]
If we instead would like this array as a delayed one, we would use the
`fromFunction` function, which takes a shape and a function, like this
    zeroToTen = fromFunction (Z :. 11) (\n -> (size n))

The Repa library contains its own versions of some common functions,
like `map` and `zipWith`, which enables normal "list operations" on
Repa arrays. Since these often, as in the two cases mentioned, have
the same names as the list functions in the Prelude, it is necessary
to either import the Repa modules qualified or hide the functions
from the Prelude.


## Image manipulation with Repa
The `repa-io` library has built-in support for reading and writing
bitmap images with the functions `readImageFromBMP` and
`writeImageToBMP`. The Repa representation of bitmaps is a
two-dimensional unboxed array of three-tuples of 8-bit words, meaning
one element for each pixel, consisting of the RGB-value tuple. To
manipulate an image, one simply changes the values of these Word8
colour values.

Because of the excellent parallelism scaling that Repa has when
working on the individual elements in a pixel, Repa is very good for
writing image manipulation functions for filters etc. that can operate
on each pixel independently.


# Image filters

Image manipulation using filters

Inspiration: http://www.html5rocks.com/en/tutorials/canvas/imagefilters/

we start with a simple greyscale filter:
...

Start with greyscale

## Gaussian blur

- What is
- Compute blur matrix
    - f sigma radius :: Array r DIM2 Double
- Apply to image
- greyscale vs colour


# Generalise to other linear convolutions

- edge detection
- other fun stuff
