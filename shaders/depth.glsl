/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#ifndef _DEPTH___GLSL_
#define _DEPTH___GLSL_

// Declare a global that all shaders can read, `depthNear`. It is used for reversed Z depth.
// The depth of a fragment is stored as depthNear / w, where w is the
// distance along the view axis. This gives the same relative precision at all distances
// in a floating point depth buffer. The binding is provided by the shader preprocessor
layout(std140, binding = #{depthDataBinding}) uniform DepthData {
  // The distance, in view space, to the near plane. This is a fixed distance in world
  // space multiplied with the view scale of the camera
  float depthNear;
};

// The depth is 0 at infinite distance. Clamping to this avoids dividing by zero when
// decoding. It is on the order of the smallest normal float (about 1.2e-38)
const float MinReversedDepth = 1e-38;

// The largest distance that a decoded depth can give. Also keeps the result finite
const float MaxViewDistance = 1e30;

#endif // _DEPTH___GLSL_
