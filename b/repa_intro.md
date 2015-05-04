# Introduction to Repa

## What is Repa

Repa is a library for parellelising computations using regular arrays,
the name REPA is an acronym for REgular PArallel arrays.

The cool thing with Repa is that as long as you run a program, written
using the repa combinators, with the flags +RTS -Nx (where x is the
number of threads), the program will automatically run the
combination computations in parallel.

The Repa arrays can be multidimensional and have different shapes,
they also have the two options of being stored as Delayed or Unboxed
manifest. Delayed means that lazy evaluation will be used (i.e. the
array is built up of functions that are computed when the respective
values are needed), Unboxed manifest on the other hand means that the
actual values are computed and stored in the array. This means that
an array where a majority of the values will be accessed multiple
times probably should be stored as Unboxed manifest to improve
performance.


## How to use Repa

- Image manipulation (reading, writing) repa-io

