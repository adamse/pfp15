---
title: Image manipulation with Repa 3
author:
    - Adam Sandberg Eriksson
    - Andreas Svanström
...

# Introduction to Repa

- What is
- How use
- Image manipulation (reading, writing) repa-io


# Gaussian blur

- What is
- Compute blur matrix
    - f sigma radius :: Array r DIM2 Double
- Apply to image
- greyscale vs colour


# Generalise to other linear convolutions

- edge detection
- other fun stuff


# Animations?

Time permitting.

- f :: Time -> Array r DIM2 (R,G,B)
  or f :: Array r DIM3 (R,G,B) ??
- use ffmpeg to stitch images together to animation
