# ARD

[![GitHub License](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/crazymaik/ard-haskell/master/LICENSE.txt)
[![Build Status](https://travis-ci.org/crazymaik/ard-haskell.svg?branch=master)](https://travis-ci.org/crazymaik/ard-haskell)

A simple ray tracer written in Haskell.
The implementation is based on the book "Ray Tracing from the Group Up" by Kevin Suffern.

## Features

* Geometric Objects
  * Box
  * Plane
  * Sphere
* Lights
  * Ambient
  * Ambient Occlusion
  * Directional
  * Point
* Viewing
  * Orthographic
  * Perspective
* BRDF/Materials
  * Matte
  * Phong
* Sampling for antialiasing
* Hard shadows
* Soft shadows via Ambient Occlusion
* Scene definition in file
* Parallelization of pixel tracing

## Examples

![Ambient Occlusion](/samples/ambient_occlusion.png)

![Simple](/samples/simple.png)

## License

    Copyright (c) 2016 Michael Zoech

    Permission is hereby granted, free of charge, to any person obtaining
    a copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
    permit persons to whom the Software is furnished to do so, subject to
    the following conditions:

    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

