/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef TEXTURETILE_HGLSL
#define TEXTURETILE_HGLSL

struct TileDepthTransform {
  float depthScale;
  float depthOffset;
};

struct TileUvTransform {
  vec2 uvOffset;
  vec2 uvScale;
};

struct ChunkTile {
  sampler2D textureSampler;
  TileUvTransform uvTransform;
};

struct ChunkTilePile {
  ChunkTile chunkTile0;
  ChunkTile chunkTile1;
  ChunkTile chunkTile2;
};

struct LayerSettings {
  float opacity;
  float gamma;
  float multiplier;
  float offset;
  float valueBlending;
};

struct LayerAdjustment {
  vec3 chromaKeyColor;
  float chromaKeyTolerance;
};

struct Layer {
  ChunkTilePile pile;
  TileDepthTransform depthTransform;
  LayerSettings settings;
  LayerAdjustment adjustment;

  // Other layer type properties stuff
  vec3 color;
};

#endif // TEXTURETILE_HGLSL
