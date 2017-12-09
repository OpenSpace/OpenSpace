/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

#version __CONTEXT__

// Textures and buffers
uniform sampler2D cubeFront;
uniform sampler2D cubeBack;
uniform sampler3D textureAtlas;
uniform sampler1D transferFunction;
//layout (r32i, binding = 2) writeonly uniform iimage2D reqList;
//layout (r32i, binding = 2) readonly uniform iimage2D brickList;
layout (rgba32f, binding = 3) writeonly uniform image2D out_image;

// TSP settings
uniform int     gridType          = 0;
uniform float   stepSize          = 0.002;
uniform int     numTimesteps      = 0;
uniform float   temporalTolerance = -1.0;
uniform float   spatialTolerance  = -1.0;
uniform int   	numBoxesPerAxis   = 0;
uniform int     paddedBrickDim    = 0;
uniform int     rootLevel    	  = 0;

in vec4 vs_position;
in vec4 vs_color;

layout( std140, binding=1 ) buffer Bricks
{
	int brickList[ ];
};

#include "helpers_cs.hglsl"

// Converts a global coordinate [0..1] to a box coordinate [0..boxesPerAxis]
ivec3 BoxCoords(vec3 _globalCoords, int _boxesPerAxis) {
  ivec3 boxCoords = ivec3(_globalCoords * float(_boxesPerAxis));
  return clamp(boxCoords, ivec3(0, 0, 0), ivec3(_boxesPerAxis-1));
}

// Fetch atlas box coordinates from brick list
ivec3 AtlasBoxCoords(int _brickIndex) {
  int x = brickList[uint(3*_brickIndex+0)];
  int y = brickList[uint(3*_brickIndex+1)];
  int z = brickList[uint(3*_brickIndex+2)];
  return ivec3(x, y, z);
}

// Convert a global coordinate to a local in-box coordinate, given
// the number of boxes (of this size) per axis and the box coordinates
vec3 InBoxCoords(vec3 _globalCoords, ivec3 _boxCoords, 
                   int _boxesPerAxis, int _paddedBrickDim) {
  // Calculate [0.0 1.0] box coordinates
  vec3 inbox = (_globalCoords - vec3(_boxCoords)/float(_boxesPerAxis)) 
         * float(_boxesPerAxis);
  // Map to padding range 
  float low = 1.0/float(_paddedBrickDim);
  float high = float(_paddedBrickDim-1)/float(_paddedBrickDim);
  return vec3(low) + inbox * (vec3(high)-vec3(low));
}

vec3 AtlasCoords(vec3 _globalCoords, int _brickIndex, int _boxesPerAxis,
                   int _paddedBrickDim, int _level) {

  // Use current octree level to calculate dividing factor for coordinates
  int divisor = int(pow(2.0, _level));
 
  // Calculate box coordinates, taking current subdivision level into account
  ivec3 boxCoords = BoxCoords(_globalCoords, _boxesPerAxis/divisor);

  // Calculate local in-box coordinates for the point
  vec3 inBoxCoords = InBoxCoords(_globalCoords, boxCoords, 
                                   _boxesPerAxis/divisor,
                                   _paddedBrickDim*divisor);

  // Fetch atlas box coordinates
  ivec3 atlasBoxCoords = AtlasBoxCoords(_brickIndex);

  // Transform coordinates to atlas coordinates
  return inBoxCoords/float(_boxesPerAxis) +
         vec3(atlasBoxCoords)/float(_boxesPerAxis);
}



// Sample atlas
void SampleAtlas(inout vec4 _color, vec3 _coords, int _brickIndex, int _level) {

  // Find the texture atlas coordinates for the point
  vec3 atlasCoords = AtlasCoords(_coords, _brickIndex, 
                                   numBoxesPerAxis, paddedBrickDim,
                                   _level);

  ivec3 boxCoords = BoxCoords(_coords, numBoxesPerAxis);
  
  vec3 a4 = vec3(atlasCoords.x, atlasCoords.y, atlasCoords.z);
  // Sample the atlas
  float intensity = texture(textureAtlas, a4).x;
  // Composition
  vec4 tf = texture(transferFunction, intensity);
  _color += (1.0f - _color.a)*tf;

}


bool TraverseBST(int _otNodeIndex, inout int _brickIndex) {
  // Start att the root of the current BST
  int bstNodeIndex = _otNodeIndex;
  bool bstRoot = true;
  int timespanStart = 0;
  int timespanEnd = numTimesteps;

  for(int i = 0; i < TraverseBSTLimit; ++i) {
    _brickIndex = BrickIndex(bstNodeIndex);

    // Check temporal error
    if (TemporalError(bstNodeIndex) <= temporalTolerance) {

      // If the OT node is a leaf, we cannot do any better spatially
      if (IsOctreeLeaf(_otNodeIndex)) {
        return true;

      } else if (SpatialError(bstNodeIndex) <= spatialTolerance) {
        return true;

      } else if (IsBSTLeaf(bstNodeIndex, bstRoot)) {
        return false;

      } else {

        // Keep traversing
        bstNodeIndex = ChildNodeIndex(bstNodeIndex, 
        							  timespanStart,
                                      timespanEnd, 
                                      bstRoot);
      }

    } else if (IsBSTLeaf(bstNodeIndex, bstRoot)) {
      return false;

    } else {

      // Keep traversing
      bstNodeIndex = ChildNodeIndex(bstNodeIndex, 
      								timespanStart,
                                    timespanEnd,
                                    bstRoot);
    }

    bstRoot = false;
  }
}


vec4 TraverseOctree(vec3 _rayO, vec3 _rayD, float _maxDist) {

  float stepsize = stepSize;

  // Sample point
  vec3 cartesianP = _rayO;

  // Keep track of distance traveled along ray
  float traversed = 0;

  // Cumulative color for ray to return
  vec4 color = vec4(0,0,0,0);


  // Traverse until sample point is outside of volume
  while (traversed < _maxDist) {

    // Reset octree traversal variables
    vec3 offset = vec3(0.0, 0.0, 0.0);
    float boxDim = 1.0;
    bool foundBrick = false;
    int child;
    int level = rootLevel;

    int otNodeIndex = 0;

    // Rely on finding a leaf for loop termination
    for(int i = 0; i < TraverseOctreeLimit; ++i) {

      // Traverse BST to get a brick index, and see if the found brick
      // is good enough
      int brickIndex;
      bool bstSuccess = TraverseBST(otNodeIndex, brickIndex);

        // Convert to spherical if needed
        vec3 sampleP;
        if (gridType == 0) { // cartesian
          sampleP = cartesianP;
        } else { // spherical ( == 1)
          sampleP = CartesianToSpherical(cartesianP);
        }

      if (bstSuccess || 
          IsOctreeLeaf(otNodeIndex)) {
        
        //float s = 0.008*SpatialError(brickIndex, 4, _tsp);
        //color += (float4)(s);


        // Sample the brick
        SampleAtlas(color, sampleP, brickIndex, level); 
        break;

      } else {
           
        // Keep traversing the octree
        
        // Next box dimension
        boxDim /= 2.0f;
        
        // Current mid point
        float boxMid = boxDim;

        // Check which child encloses the sample point
        child = EnclosingChild(sampleP, boxMid, offset);
       
        // Update offset for next level
        UpdateOffset(offset, boxDim, child);

        // Update index to new node
        otNodeIndex = OTChildIndex(otNodeIndex, child);
        
        level--;

      }

    } // while traversing

    // Update sample point
    traversed += stepsize;
    cartesianP += stepsize * _rayD;

  } // while (traversed < maxDist)

  return color;
}

void main() {

  // Get coordinated
  const vec3 startPos = vs_color.xyz;
  const vec3 endPos = texture(cubeBack, gl_FragCoord.xy).xyz;

  // Calculate direction of raycasting
  vec3 dir = startPos - endPos;
  const float maxDist = length(dir);
  dir = normalize(dir);

  // Early termination
  if(maxDist == 0.0)
    discard;

  // Early termination
  if(maxDist == 0.0)
    return;

  vec4 color = TraverseOctree(startPos, dir, maxDist); 
  //color += vec4(1, 0, 0, 1);
  //vec4 color = vec4(dir,1);
  //vec4 color = vec4(dir,1);

  // Store result
  imageStore(out_image, ivec2(gl_FragCoord.xy), color);
  discard;
}