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

// TSP settings
uniform int     gridType          = 0;
uniform float   stepSize          = 0.002;
uniform int     numTimesteps      = 0;
uniform float   temporalTolerance = -1.0;
uniform float   spatialTolerance  = -1.0;

layout( std140, binding=1 ) buffer reqBuffer
{
	int reqBrick[ ];
};


#include "helpers_cs.hglsl"


in vec4 vs_position;
in vec4 vs_color;

//#include <${SHADERS}/ABuffer/abufferStruct.hglsl>
//#include <${SHADERS}/ABuffer/abufferAddToBuffer.hglsl>
//#include <${SHADERS}/PowerScaling/powerScaling_fs.hglsl>


// Increment the count for a brick in the request list
void AddToList(int _brickIndex) {
    //atomic_inc(&_reqList[_brickIndex]);

    // only used to flag requested brick, no need for concurrency
    reqBrick[_brickIndex] = 1;
    //imageStore(reqList, coord, ivec4(1,0,0,0));
}

// Given an octree node index, traverse the corresponding BST tree and look
// for a useful brick. 
bool TraverseBST(int _otNodeIndex, inout int _brickIndex) {

  // Start at the root of the current BST
  int bstNodeIndex = _otNodeIndex;
  bool bstRoot = true;
  int timespanStart = 0;
  int timespanEnd = numTimesteps;

   // Rely on structure for termination
   for(int i = 0; i < TraverseBSTLimit; ++i) {
   //while (true) {
  
    // Update brick index (regardless if we use it or not)
    _brickIndex = BrickIndex(bstNodeIndex);

    // If temporal error is ok
    // TODO float and <= errors
    if (TemporalError(bstNodeIndex) <= temporalTolerance) 
    {
      
      // If the ot node is a leaf, we can't do any better spatially so we 
      // return the current brick
      if (IsOctreeLeaf(_otNodeIndex)) {
        return true;

      // All is well!
      } else if (SpatialError(bstNodeIndex) <= spatialTolerance) {
        return true;
         
      // If spatial failed and the BST node is a leaf
      // The traversal will continue in the octree (we know that
      // the octree node is not a leaf)
      } else if (IsBSTLeaf(bstNodeIndex, bstRoot)) {
        return false;
      
      // Keep traversing BST
      } else {
        bstNodeIndex = ChildNodeIndex(bstNodeIndex,
                                      timespanStart,
                                      timespanEnd,
                                      bstRoot);
      }

    // If temporal error is too big and the node is a leaf
    // Return false to traverse OT
    } else if (IsBSTLeaf(bstNodeIndex, bstRoot)) {
      return false;
    
    // If temporal error is too big and we can continue
    } else {
      bstNodeIndex = ChildNodeIndex(bstNodeIndex,
                                    timespanStart,
                                    timespanEnd,
                                    bstRoot);
    }

    bstRoot = false;
  }
  

  // Hopefully we never go here
  return false;
}

void TraverseOctree(const vec3 _rayO, const vec3 _rayD, const float _maxDist) {

  vec3 P = _rayO;
  // Keep traversing until the sample point goes outside the unit cube
  float traversed = 0.0f;
  
  while (traversed < _maxDist) {
    
    // Reset traversal variables
    vec3 offset = vec3(0.0, 0.0, 0.0);
    float boxDim = 1.0f;
    int child;

    // Init the octree node index to the root
    int otNodeIndex = OctreeRootNodeIndex();
    
    // Start traversing octree
    // Rely on finding a leaf for loop termination 
    for(int i = 0; i < TraverseOctreeLimit; ++i) {
    //while (true) {
      // See if the BST tree is good enough
      int brickIndex = 0;
      bool bstSuccess = TraverseBST(otNodeIndex, brickIndex);


      if (bstSuccess) {

        // Add the found brick to brick list
        AddToList(brickIndex);
        // We are now done with this node, so go to next
        break;

      // If the BST lookup failed but the octree node is a leaf, 
      // add the brick anyway (it is the BST leaf)
      } else if (IsOctreeLeaf(otNodeIndex)) {
        AddToList(brickIndex);
        // We are now done with this node, so go to next
        break;

      // If the BST lookup failed and we can traverse the octree,
      // visit the child that encloses the point
      } else {
        
        // Next box dimension
        boxDim = boxDim/2.0f;

        // Current mid point
        float boxMid = boxDim;

        // Check which child encloses P
        
        if (gridType == 0) { // Cartesian
          child = EnclosingChild(P, boxMid, offset);
        } else { // Spherical (==1)
          child = EnclosingChild(CartesianToSpherical(P), boxMid, offset);
        }

        // Update offset
        UpdateOffset(offset, boxDim, child);

        // Update node index to new node
        //int oldIndex = otNodeIndex;
        otNodeIndex = OTChildIndex(otNodeIndex, child);
      }

    } // while traversing

    // Update 
    traversed = traversed + stepSize;
    P = P + stepSize * _rayD;

  } // while (traversed < maxDist)
  
  
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

	TraverseOctree(startPos, dir, maxDist);
	discard;
  //imageStore(out_image, coord, vec4(dir, 1));

}