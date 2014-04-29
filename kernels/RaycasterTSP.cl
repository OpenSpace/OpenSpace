struct KernelConstants {
  int gridType_;
  float stepsize_;
  float intensity_;
  int numTimesteps_;
  int numValuesPerNode_;
  int numOTNodes_;
  int numBoxesPerAxis_;
  float temporalTolerance_;
  float spatialTolerance_;
  int rootLevel_;
  int paddedBrickDim_;
};

float3 CartesianToSpherical(float3 _cartesian);
float Lerp(float _v0, float _v1, float _d);
int LeftBST(int _bstNodeIndex, int _numValuesPerNode, int _numOTNodes,
            bool _bstRoot, __global __read_only int *_tsp);
int RightBST(int _bstNodeIndex, int _numValuesPerNode, int _numOTNodes,
             bool _bstRoot, __global __read_only int *_tsp);
int ChildNodeIndex(int _bstNodeIndex, 
                   int *_timespanStart,
                   int *_timespanEnd,
                   int _timestep,
                   int _numValuesPerNode,
                   int _numOTNodes,
                   bool _bstRoot,
                   __global __read_only int *_tsp);
int BrickIndex(int _bstNodeIndex, int _numValuesPerNode, 
               __global __read_only int *_tsp);
bool IsBSTLeaf(int _bstNodeIndex, int _numValuesPerNode, 
               bool _bstRoot, __global __read_only int *_tsp);
bool IsOctreeLeaf(int _otNodeIndex, int _numValuesPerNode, 
                  __global __read_only int *_tsp);
int OTChildIndex(int _otNodeIndex, int _numValuesPerNode,
                 int _child, 
                 __global __read_only int *_tsp) ;
float TemporalError(int _bstNodeIndex, int _numValuesPerNode, 
                    __global __read_only int *_tsp) ; 
float SpatialError(int _bstNodeIndex, int _numValuesPerNode, 
                  __global __read_only int *_tsp);
int3 BoxCoords(float3 _globalCoords, int _boxesPerAxis) ;
int3 AtlasBoxCoords(int _brickIndex, 
                    __global __read_only int *_brickList);
float3 AtlasCoords(float3 _globalCoords, int _brickIndex, int _boxesPerAxis,
                   int _paddedBrickDim, int _level, 
                   __global __read_only int *_brickList) ;
void SampleAtlas(float4 *_color, float3 _coords, int _brickIndex,
                 int _boxesPerAxis, int _paddedBrickDim, int _level,
                 const sampler_t _atlasSampler,
                 __read_only image3d_t _textureAtlas,
                 __read_only image2d_t _transferFunction,
                 const sampler_t _tfSampler,
                 __global __read_only int *_brickList);
bool TraverseBST(int _otNodeIndex, int *_brickIndex,
                 __constant __read_only struct KernelConstants *_constants,
                 __global __read_only int *_tsp, const int _timestep);
int EnclosingChild(float3 _P, float _boxMid, float3 _offset) ;
void UpdateOffset(float3 *_offset, float _boxDim, int _child) ;
float4 TraverseOctree(float3 _rayO, float3 _rayD, float _maxDist,
                      __read_only image3d_t _textureAtlas,
                      __constant struct KernelConstants *_constants,
                      __read_only image2d_t _transferFunction,
                      __global __read_only int *_tsp,
                      __global __read_only int *_brickList,
                      const int _timestep);
__kernel void RaycasterTSP(__read_only image2d_t _cubeFront,
                           __read_only image2d_t _cubeBack,
                           __write_only image2d_t _output,
                           __read_only image3d_t _textureAtlas,
                           __constant struct KernelConstants *_constants,
                           __read_only image2d_t _transferFunction,
                           //__global __read_only float *_transferFunction,
                           __global __read_only int *_tsp,
                           __global __read_only int *_brickList,
                           const int _timestep);



// Turn normalized [0..1] cartesian coordinates 
// to normalized spherical [0..1] coordinates
float3 CartesianToSpherical(float3 _cartesian) {
  // Put cartesian in [-1..1] range first
  _cartesian = (float3)(-1.0) + 2.0f* _cartesian;
  float r = length(_cartesian);
  float theta, phi;
  if (r == 0.0) {
    theta = phi = 0.0;
  } else {
    theta = acospi(_cartesian.z/r);
    phi = (M_PI + atan2(_cartesian.y, _cartesian.x)) / (2.0*M_PI);
  }
  r = r / native_sqrt(3.0f);
  // Sampler ignores w component
  return (float3)(r, theta, phi);
}

// Linearly interpolate between two values. Distance
// is assumed to be normalized.
float Lerp(float _v0, float _v1, float _d) {
  return _v0*(1.0 - _d) + _v1*_d;
}

/*
float4 TransferFunction(__global __read_only image2d_t _tf, 
                        const sampler_t _tfSampler, 
                        float _i) {
  // TODO remove  hard-coded value and change to 1D texture
  int i0 = (int)floor(1023.0*_i);
  int i1 = (i0 < 1023) ? i0+1 : i0;
  float di = _i - floor(_i);
  
  float tfr0 = _tf[i0*4+0];
  float tfr1 = _tf[i1*4+0];
  float tfg0 = _tf[i0*4+1];
  float tfg1 = _tf[i1*4+1];
  float tfb0 = _tf[i0*4+2];
  float tfb1 = _tf[i1*4+2];
  float tfa0 = _tf[i0*4+3];
  float tfa1 = _tf[i1*4+3];

  float tfr = Lerp(tfr0, tfr1, di);
  float tfg = Lerp(tfg0, tfg1, di);
  float tfb = Lerp(tfb0, tfb1, di);
  float tfa = Lerp(tfa0, tfa1, di);

  return clamp((float4)(tfr, tfg, tfb, tfa), (float4)(0.0), (float4)(1.0));
  //return (float4)(tfr, tfg, tfb, tfa);
  float2 sampleCoords = (float2)(1.0, _i);
  return read_imagef(_tf, _tfSampler, sampleCoords);
}
*/

/*
// Return index to the octree root (same index as BST root at that OT node)
int OctreeRootNodeIndex() {
  return 0;
}
*/

// Return index to left BST child (low timespan)
int LeftBST(int _bstNodeIndex, int _numValuesPerNode, int _numOTNodes,
            bool _bstRoot, __global __read_only int *_tsp) {
  // If the BST node is a root, the child pointer is used for the OT. 
  // The child index is next to the root.
  // If not root, look up in TSP structure.
  if (_bstRoot) {
    return _bstNodeIndex + _numOTNodes;
  } else {
    return _tsp[_bstNodeIndex*_numValuesPerNode + 1];
  }
}

// Return index to right BST child (high timespan)
int RightBST(int _bstNodeIndex, int _numValuesPerNode, int _numOTNodes,
             bool _bstRoot, __global __read_only int *_tsp) {
  if (_bstRoot) {
    return _bstNodeIndex + _numOTNodes*2;
  } else {
    return _tsp[_bstNodeIndex*_numValuesPerNode + 1] + _numOTNodes;
  }
}

// Return child node index given a BST node, a time span and a timestep
// Updates timespan
int ChildNodeIndex(int _bstNodeIndex, 
                   int *_timespanStart,
                   int *_timespanEnd,
                   int _timestep,
                   int _numValuesPerNode,
                   int _numOTNodes,
                   bool _bstRoot,
                   __global __read_only int *_tsp) {
  // Choose left or right child
  int middle = *_timespanStart + (*_timespanEnd - *_timespanStart)/2; 
  if (_timestep <= middle) {
    // Left subtree
    *_timespanEnd = middle;
    return LeftBST(_bstNodeIndex, _numValuesPerNode, _numOTNodes, 
                   _bstRoot, _tsp);
  } else {
    // Right subtree
    *_timespanStart = middle+1;
    return RightBST(_bstNodeIndex, _numValuesPerNode, _numOTNodes,
                    _bstRoot, _tsp);
  }
}

// Return the brick index that a BST node represents
int BrickIndex(int _bstNodeIndex, int _numValuesPerNode, 
               __global __read_only int *_tsp) {
  return _tsp[_bstNodeIndex*_numValuesPerNode + 0];
}

// Checks if a BST node is a leaf ot not
bool IsBSTLeaf(int _bstNodeIndex, int _numValuesPerNode, 
               bool _bstRoot, __global __read_only int *_tsp) {
  if (_bstRoot) return false;
  return (_tsp[_bstNodeIndex*_numValuesPerNode + 1] == -1);
}

// Checks if an OT node is a leaf or not
bool IsOctreeLeaf(int _otNodeIndex, int _numValuesPerNode, 
                  __global __read_only int *_tsp) {
  // CHILD_INDEX is at offset 1, and -1 represents leaf
  return (_tsp[_otNodeIndex*_numValuesPerNode + 1] == -1);
}

// Return OT child index given current node and child number (0-7)
int OTChildIndex(int _otNodeIndex, int _numValuesPerNode,
                 int _child, 
                 __global __read_only int *_tsp) {
  int firstChild = _tsp[_otNodeIndex*_numValuesPerNode + 1];
  return firstChild + _child;
}

float TemporalError(int _bstNodeIndex, int _numValuesPerNode, 

                    __global __read_only int *_tsp) {
  return as_float(_tsp[_bstNodeIndex*_numValuesPerNode + 3]);
}

float SpatialError(int _bstNodeIndex, int _numValuesPerNode, 
                  __global __read_only int *_tsp) {
  return as_float(_tsp[_bstNodeIndex*_numValuesPerNode + 2]);
}

// Converts a global coordinate [0..1] to a box coordinate [0..boxesPerAxis]
int3 BoxCoords(float3 _globalCoords, int _boxesPerAxis) {
  int3 boxCoords = convert_int3((_globalCoords * (float)_boxesPerAxis));
  return clamp(boxCoords, (int3)(0, 0, 0), (int3)(_boxesPerAxis-1));
}

// Fetch atlas box coordinates from brick list
int3 AtlasBoxCoords(int _brickIndex, 
                    __global __read_only int *_brickList) {
  int x = _brickList[3*_brickIndex+0];
  int y = _brickList[3*_brickIndex+1];
  int z = _brickList[3*_brickIndex+2];
  return (int3)(x, y, z);
}

// Convert a global coordinate to a local in-box coordinate, given
// the number of boxes (of this size) per axis and the box coordinates
float3 InBoxCoords(float3 _globalCoords, int3 _boxCoords, 
                   int _boxesPerAxis, int _paddedBrickDim) {
  // Calculate [0.0 1.0] box coordinates
  float3 inbox = (_globalCoords - convert_float3(_boxCoords)/(float)_boxesPerAxis) 
         * (float)_boxesPerAxis;
  // Map to padding range 
  float low = 1.0/(float)_paddedBrickDim;
  float high = (float)(_paddedBrickDim-1)/(float)_paddedBrickDim;
  return (float3)(low) + inbox * ((float3)(high)-(float3)(low));
}

float3 AtlasCoords(float3 _globalCoords, int _brickIndex, int _boxesPerAxis,
                   int _paddedBrickDim, int _level, 
                   __global __read_only int *_brickList) {

  // Use current octree level to calculate dividing factor for coordinates
  int divisor = (int)pow(2.0, _level);
 
  // Calculate box coordinates, taking current subdivision level into account
  int3 boxCoords = BoxCoords(_globalCoords, _boxesPerAxis/divisor);

  // Calculate local in-box coordinates for the point
  float3 inBoxCoords = InBoxCoords(_globalCoords, boxCoords, 
                                   _boxesPerAxis/divisor,
                                   _paddedBrickDim*divisor);

  // Fetch atlas box coordinates
  int3 atlasBoxCoords = AtlasBoxCoords(_brickIndex, _brickList);

  // Transform coordinates to atlas coordinates
  return inBoxCoords/(float)_boxesPerAxis +
         convert_float3(atlasBoxCoords)/(float)_boxesPerAxis;
}



// Sample atlas
void SampleAtlas(float4 *_color, float3 _coords, int _brickIndex,
                 int _boxesPerAxis, int _paddedBrickDim, int _level,
                 const sampler_t _atlasSampler,
                 __read_only image3d_t _textureAtlas,
                 __read_only image2d_t _transferFunction,
                 const sampler_t _tfSampler,
                 __global __read_only int *_brickList) {

  // Find the texture atlas coordinates for the point
  float3 atlasCoords = AtlasCoords(_coords, _brickIndex, 
                                   _boxesPerAxis, _paddedBrickDim,
                                   _level, _brickList);

  int3 boxCoords = BoxCoords(_coords, _boxesPerAxis);
  
  float4 a4 = (float4)(atlasCoords.x, atlasCoords.y, atlasCoords.z, 1.0);
  // Sample the atlas
  float sample = read_imagef(_textureAtlas, _atlasSampler, a4).x;
  // Composition
  float4 tf = read_imagef(_transferFunction, _tfSampler, (float2)(sample, 0.0));
  *_color += (1.0f - _color->w)*tf;

}


bool TraverseBST(int _otNodeIndex, int *_brickIndex,
                 __constant __read_only struct KernelConstants *_constants,
                 __global __read_only int *_tsp, const int _timestep) {
  // Start att the root of the current BST
  int bstNodeIndex = _otNodeIndex;
  bool bstRoot = true;
  int timespanStart = 0;
  int timespanEnd = _constants->numTimesteps_;

  while (true) {
    *_brickIndex = BrickIndex(bstNodeIndex, 
                              _constants->numValuesPerNode_,
                              _tsp);

    // Check temporal error
    if (TemporalError(bstNodeIndex, _constants->numValuesPerNode_, _tsp) <=
        _constants->temporalTolerance_) {

      // If the OT node is a leaf, we cannot do any better spatially
      if (IsOctreeLeaf(_otNodeIndex, _constants->numValuesPerNode_, _tsp)) {
        return true;

      } else if (SpatialError(bstNodeIndex, _constants->numValuesPerNode_,
                              _tsp) <= _constants->spatialTolerance_) {
        return true;

      } else if (IsBSTLeaf(bstNodeIndex, _constants->numValuesPerNode_,
                           bstRoot, _tsp)) {
        return false;

      } else {

        // Keep traversing
        bstNodeIndex = ChildNodeIndex(bstNodeIndex, &timespanStart,
                                      &timespanEnd, 
                                      _timestep,
                                      _constants->numValuesPerNode_,
                                      _constants->numOTNodes_,
                                      bstRoot, _tsp);
      }

    } else if (IsBSTLeaf(bstNodeIndex, _constants->numValuesPerNode_,
                         bstRoot, _tsp)) {
      return false;

    } else {

      // Keep traversing
      bstNodeIndex = ChildNodeIndex(bstNodeIndex, &timespanStart,
                                    &timespanEnd,
                                    _timestep,
                                    _constants->numValuesPerNode_,
                                    _constants->numOTNodes_,
                                    bstRoot, _tsp);
    }

    bstRoot = false;
  }
}


// Given a point, a box mid value and an offset, return enclosing child
int EnclosingChild(float3 _P, float _boxMid, float3 _offset) {
  if (_P.x < _boxMid+_offset.x) {
    if (_P.y < _boxMid+_offset.y) {
      if (_P.z < _boxMid+_offset.z) {
        return 0;
      } else {
        return 4;
      }
    } else {
      if (_P.z < _boxMid+_offset.z) {
        return 2;
      } else {
        return 6;
      }
    }
  } else {
    if (_P.y < _boxMid+_offset.y) {
      if (_P.z < _boxMid+_offset.z) {
        return 1;
      } else {
        return 5;
      }
    } else {
      if (_P.z < _boxMid+_offset.z) {
        return 3;
      } else {
        return 7;
      }
    }
  }
}

void UpdateOffset(float3 *_offset, float _boxDim, int _child) {
  if (_child == 0) {
    // do nothing
  } else if (_child == 1) {
    _offset->x += _boxDim;
  } else if (_child == 2) {
    _offset->y += _boxDim;
  } else if (_child == 3) {
    _offset->x += _boxDim;
    _offset->y += _boxDim;
  } else if (_child == 4) {
    _offset->z += _boxDim;
  } else if (_child == 5) {
    _offset->x += _boxDim;
    _offset->z += _boxDim;
  } else if (_child == 6) {
    _offset->y += _boxDim;
    _offset->z += _boxDim;
  } else if (_child == 7) {
    *_offset += (float3)(_boxDim);
  }
}

float4 TraverseOctree(float3 _rayO, float3 _rayD, float _maxDist,
                      __read_only image3d_t _textureAtlas,
                      __constant struct KernelConstants *_constants,
                      __read_only image2d_t _transferFunction,
                      __global __read_only int *_tsp,
                      __global __read_only int *_brickList,
                      const int _timestep) {

  float stepsize = _constants->stepsize_;
  // Sample point
  float3 cartesianP = _rayO;
  // Keep track of distance traveled along ray
  float traversed = 0;
  // Cumulative color for ray to return
  float4 color = (float4)(0.0);

  // Sampler for texture atlas
  const sampler_t atlasSampler = CLK_FILTER_LINEAR |
                                 CLK_NORMALIZED_COORDS_TRUE |
                                 CLK_ADDRESS_CLAMP_TO_EDGE;

                                
  // Sampler for transfer function texture 
  const sampler_t tfSampler = CLK_FILTER_LINEAR |
                              CLK_NORMALIZED_COORDS_TRUE |
                              CLK_ADDRESS_CLAMP_TO_EDGE;

  // Traverse until sample point is outside of volume
  while (traversed < _maxDist) {

    // Reset octree traversal variables
    float3 offset = (float3)(0.0);
    float boxDim = 1.0;
    bool foundBrick = false;
    int child;
    int level = _constants->rootLevel_;

    int otNodeIndex = 0;

    // Rely on finding a leaf for loop termination
    while (true) {

      // Traverse BST to get a brick index, and see if the found brick
      // is good enough
      int brickIndex;
      bool bstSuccess = TraverseBST(otNodeIndex,
                                    &brickIndex,
                                    _constants,
                                    _tsp,
                                    _timestep);

        // Convert to spherical if needed
        float3 sampleP;
        if (_constants->gridType_ == 0) { // cartesian
          sampleP = cartesianP;
        } else { // spherical ( == 1)
          sampleP = CartesianToSpherical(cartesianP);
        }

      if (bstSuccess || 
          IsOctreeLeaf(otNodeIndex, _constants->numValuesPerNode_, _tsp)) {
        
        //float s = 0.008*SpatialError(brickIndex, 4, _tsp);
        //color += (float4)(s);


        // Sample the brick
        SampleAtlas(&color, sampleP, brickIndex, 
                    _constants->numBoxesPerAxis_, 
                    _constants->paddedBrickDim_,
                    level, 
                    atlasSampler, _textureAtlas,
                    _transferFunction,
                    tfSampler, _brickList); 
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
        UpdateOffset(&offset, boxDim, child);

        // Update index to new node
        otNodeIndex = OTChildIndex(otNodeIndex, _constants->numValuesPerNode_,
                                   child, _tsp);
        
        level--;

      }

    } // while traversing

    // Update sample point
    traversed += stepsize;
    cartesianP += stepsize * _rayD;

  } // while (traversed < maxDist)

  return color;
}


__kernel void RaycasterTSP(__read_only image2d_t _cubeFront,
                           __read_only image2d_t _cubeBack,
                           __write_only image2d_t _output,
                           __read_only image3d_t _textureAtlas,
                           __constant struct KernelConstants *_constants,
                           __read_only image2d_t _transferFunction,
                           //__global __read_only float *_transferFunction,
                           __global __read_only int *_tsp,
                           __global __read_only int *_brickList,
                           const int _timestep) {

  // Kernel should be launched in 2D with one work item per pixel
  int2 intCoords = (int2)(get_global_id(0), get_global_id(1));

  // Sampler for color cube reading
  const sampler_t sampler = CLK_NORMALIZED_COORDS_FALSE;

  // Read from color cube textures
  float4 cubeFrontColor = read_imagef(_cubeFront, sampler, intCoords);
  float4 cubeBackColor = read_imagef(_cubeBack, sampler, intCoords);
  // Figure out ray direction and distance to traverse
  float3 direction = cubeBackColor.xyz - cubeFrontColor.xyz;
  
  float maxDist = length(direction);
  
  direction = normalize(direction);

  float4 color = TraverseOctree(cubeFrontColor.xyz, // ray origin
                                direction,          // direction
                                maxDist,            // distance to traverse
                                _textureAtlas,      // voxel data atlas
                                _constants,         // kernel constants
                                _transferFunction,  // transfer function
                                _tsp,               // TSP tree struct
                                _brickList,
                                _timestep); 
                                
  //color = 0.0001*color + cubeFrontColor;

  write_imagef(_output, intCoords, _constants->intensity_*color);
  
  return;
}
                                  
