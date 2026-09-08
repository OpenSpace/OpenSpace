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

uniform float maxStepSize#{id} = 0.02;
uniform sampler3D volumeTexture_#{id};
uniform sampler1D transferFunction_#{id};
uniform int gridType_#{id} = 0;

uniform int nClips_#{id};
uniform vec3 clipNormals_#{id}[8];
uniform vec2 clipOffsets_#{id}[8];

uniform vec2 valueRange_#{id};
uniform bool hideOutsideRange_#{id};

uniform float brightness_#{id} = 1.0;
// unitless factor that multiplies with the brightness [0,1] to achieve desired visuals.
const float SamplingIntervalReferenceFactor = 500.0;

// If true, skip any sample whose 8 trilinear-interpolation neighbors include a cell
// equal to nonValue. This prevents blending with empty cells and produces a sharp border.
uniform bool sharpBorder_#{id} = false;

// The data value that represents 'no data'. Cells equal to this value are masked out
// during border-path trilinear interpolation.
uniform float nonValue_#{id} = 0.0;

// Precomputed per-cell mask: 1.0 if all 8 corners of the interpolation cell are
// non-zero (safe to use fast hardware trilinear), 0.0 otherwise.
// Stored as GL_R8 normalized (uint8 255 = 1.0 exactly per OpenGL spec).
uniform sampler3D safeMask_#{id};

// Normalization factor x for radius r [0, 1].
// value *= 1/(r^x)
// only working for volumes given in spherical coordianates.
// Applied after any linear value remapping.
uniform float rNormalization_#{id} = 0.0;

uniform float rUpperBound_#{id} = 1.0;


void sample#{id}(vec3 samplePos, vec3 dir, inout vec3 accumulatedColor,
                 inout vec3 accumulatedAlpha, inout float stepSize)
{
  vec3 pos = samplePos;
  if (gridType_#{id} == 1) {
    pos = cartesianToSpherical(samplePos);
    if (abs(pos.r) > 1.0) {
      return;
    }
  }

  float clipAlpha = 1.0;
  vec3 centerToPos = pos - vec3(0.5);

  for (int i = 0; i < nClips_#{id} && i < 8; i++) {
    vec3 clipNormal = clipNormals_#{id}[i];
    float clipBegin = clipOffsets_#{id}[i].x;
    float clipEnd = clipBegin + clipOffsets_#{id}[i].y;
    clipAlpha *= smoothstep(clipBegin, clipEnd, dot(centerToPos, clipNormal));
  }

  clipAlpha *= 1.0 - smoothstep(rUpperBound_#{id} - 0.01, rUpperBound_#{id} + 0.01, pos.x);

  if (clipAlpha > 0) {
    float val;
    if (sharpBorder_#{id}) {
      // Value stored by the CPU-side precomputation for a fully non-zero cell.
      // GL_R8 normalized guarantees uint8 255 maps to exactly 1.0.
      const float SAFE_CELL = 1.0;

      if (texture(safeMask_#{id}, pos).r == SAFE_CELL) {
        // Fast path: all 8 corners are non-zero. Use hardware trilinear directly;
        // no integer coordinate math needed here.
        val = texture(volumeTexture_#{id}, pos).r;
      }
      else {
        // Border path: at least one of the 8 interpolation corners is nonValue.
        ivec3 texSize = textureSize(volumeTexture_#{id}, 0);
        vec3 texelPos = pos * vec3(texSize) - 0.5;

        // Step 1: check whether the current sample position is itself in valid data.
        // The nearest voxel is the best proxy for the "cell" the sample belongs to.
        // If it equals nonValue the sample is in empty space -> discard immediately.
        ivec3 nearest = clamp(ivec3(round(texelPos)), ivec3(0), texSize - 1);
        if (texelFetch(volumeTexture_#{id}, nearest, 0).r == nonValue_#{id}) {
          return;
        }

        // Step 2: current position is inside valid data but neighbours may not be.
        // Perform masked trilinear: fetch all 8 corners, zero-weight the nonValue
        // ones and renormalise. This blends only among valid neighbours so the
        // border stays sharp without discarding the outermost layer of real data.
        ivec3 i0 = clamp(ivec3(floor(texelPos)), ivec3(0), texSize - 1);
        ivec3 i1 = clamp(i0 + ivec3(1), ivec3(0), texSize - 1);
        vec3 f = fract(texelPos);

        float v000 = texelFetch(volumeTexture_#{id}, ivec3(i0.x, i0.y, i0.z), 0).r;
        float v100 = texelFetch(volumeTexture_#{id}, ivec3(i1.x, i0.y, i0.z), 0).r;
        float v010 = texelFetch(volumeTexture_#{id}, ivec3(i0.x, i1.y, i0.z), 0).r;
        float v110 = texelFetch(volumeTexture_#{id}, ivec3(i1.x, i1.y, i0.z), 0).r;
        float v001 = texelFetch(volumeTexture_#{id}, ivec3(i0.x, i0.y, i1.z), 0).r;
        float v101 = texelFetch(volumeTexture_#{id}, ivec3(i1.x, i0.y, i1.z), 0).r;
        float v011 = texelFetch(volumeTexture_#{id}, ivec3(i0.x, i1.y, i1.z), 0).r;
        float v111 = texelFetch(volumeTexture_#{id}, ivec3(i1.x, i1.y, i1.z), 0).r;

        float w000 = (1.0-f.x) * (1.0-f.y) * (1.0-f.z) * float(v000 != nonValue_#{id});
        float w100 =      f.x  * (1.0-f.y) * (1.0-f.z) * float(v100 != nonValue_#{id});
        float w010 = (1.0-f.x) *      f.y  * (1.0-f.z) * float(v010 != nonValue_#{id});
        float w110 =      f.x  *      f.y  * (1.0-f.z) * float(v110 != nonValue_#{id});
        float w001 = (1.0-f.x) * (1.0-f.y) *      f.z  * float(v001 != nonValue_#{id});
        float w101 =      f.x  * (1.0-f.y) *      f.z  * float(v101 != nonValue_#{id});
        float w011 = (1.0-f.x) *      f.y  *      f.z  * float(v011 != nonValue_#{id});
        float w111 =      f.x  *      f.y  *      f.z  * float(v111 != nonValue_#{id});

        float totalWeight = w000+w100+w010+w110+w001+w101+w011+w111;
        if (totalWeight == 0.0) {
          return;
        }

        val = (w000*v000 + w100*v100 + w010*v010 + w110*v110 +
               w001*v001 + w101*v101 + w011*v011 + w111*v111) / totalWeight;
      }
    }
    else {
      val = texture(volumeTexture_#{id}, pos).r;
    }

    float minVal = valueRange_#{id}.x;
    float diff = valueRange_#{id}.y - valueRange_#{id}.x;

    val = (val - minVal) / diff;

    if (rNormalization_#{id} > 0 && gridType_#{id} == 1) {
      val *= pow(pos.x, rNormalization_#{id});
    }

    vec4 color = texture(transferFunction_#{id}, val);

    // Apply an alpha falloff using eq: e^(-((x*2-1)^(p)+(y*2-1)^(p)) * p), a large value
    // of p results in a square shape, a value of 2 resulsts in a circle
    if(hideOutsideRange_#{id}) {
      const float e = 2.71828;
      float p = 400;
      float scale = 1.05; // Scaling the cube further to reduce the impact on the edges
      float x  = (pos.x * 2 - 1) * scale;
      float y = (pos.y * 2 - 1) * scale;
      float z = (pos.z * 2 -1) * scale;

      float v = pow(e, -(pow(abs(x), p) + pow(abs(y), p) + pow(abs(z), p)) * p);
      color.a *= v;
      // color = vec4(v, 0, 0, 0.5);
    }

    vec3 backColor = color.rgb;
    vec3 backAlpha = color.aaa;

    backColor *=
      stepSize * brightness_#{id} * SamplingIntervalReferenceFactor * clipAlpha;
    backAlpha *=
      stepSize * brightness_#{id} * SamplingIntervalReferenceFactor * clipAlpha;

    backColor = clamp(backColor, 0.0, 1.0);
    backAlpha = clamp(backAlpha, 0.0, 1.0);

    vec3 oneMinusFrontAlpha = vec3(1.0) - accumulatedAlpha;
    accumulatedColor += oneMinusFrontAlpha * backColor;
    accumulatedAlpha += oneMinusFrontAlpha * backAlpha;
  }

  stepSize = maxStepSize#{id};
}

float stepSize#{id}(vec3 samplePos, vec3 dir) {
    return maxStepSize#{id};
}
