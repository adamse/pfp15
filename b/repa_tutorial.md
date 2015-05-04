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
