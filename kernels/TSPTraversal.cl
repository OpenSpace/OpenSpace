// Mirrors struct on host side
struct TraversalConstants {
  int gridType_;
  float stepsize_;
  int numTimesteps_;
  int numValuesPerNode_;
  int numOTNodes_;
  float temporalTolerance_;
  float spatialTolerance_;
};

float3 CartesianToSpherical(float3 _cartesian);
int OctreeRootNodeIndex();
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
               __global __read_only int *_tsp) ;
bool IsBSTLeaf(int _bstNodeIndex, int _numValuesPerNode, 
               bool _bstRoot, __global __read_only int *_tsp) ;
bool IsOctreeLeaf(int _otNodeIndex, int _numValuesPerNode, 
                  __global __read_only int *_tsp) ;
int OTChildIndex(int _otNodeIndex, int _numValuesPerNode,
                 int _child, 
                 __global __read_only int *_tsp);
void AddToList(int _brickIndex, 
               __global volatile int *_reqList);
float TemporalError(int _bstNodeIndex, int _numValuesPerNode, 
                    __global __read_only int *_tsp) ;
float SpatialError(int _bstNodeIndex, int _numValuesPerNode, 
                   __global __read_only int *_tsp);
bool TraverseBST(int _otNodeIndex,
                 int *_brickIndex, 
                 int _timestep,
                 __constant struct TraversalConstants *_constants,
                 __global volatile int *_reqList,
                 __global __read_only int *_tsp);
int EnclosingChild(float3 _P, float _boxMid, float3 _offset);
void UpdateOffset(float3 *_offset, float _boxDim, int _child) ;
void TraverseOctree(float3 _rayO, 
                    float3 _rayD,
                    float _maxDist,
                    __constant struct TraversalConstants *_constants,
                    __global volatile int *_reqList,
                    __global __read_only int *_tsp,
                    const int _timestep);
__kernel void TSPTraversal(__read_only image2d_t _cubeFront,
                           __read_only image2d_t _cubeBack,
                           __constant struct TraversalConstants *_constants,
                           __global __read_only int *_tsp,
                           __global int *_reqList,
                           const int _timestep) ;





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

// Return index to the octree root (same index as BST root at that OT node)
int OctreeRootNodeIndex() {
  return 0;
}

// Return index to left BST child (low timespan)
int LeftBST(int _bstNodeIndex, int _numValuesPerNode, int _numOTNodes,
            bool _bstRoot, __global __read_only int *_tsp) {
  // If the BST node is a root, the child pointer is used for the OT. 
  // The child index is next to the root.
  // If not root, look up in TSP structure.
  if (_bstRoot) {
    return _bstNodeIndex + _numOTNodes;
    //return _bstNodeIndex + 1;
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
  int firstChild = _tsp[_otNodeIndex*_numValuesPerNode+1];
  return firstChild + _child;
}

// Increment the count for a brick in the request list
void AddToList(int _brickIndex, 
               __global volatile int *_reqList) {
  atomic_inc(&_reqList[_brickIndex]);
}

float TemporalError(int _bstNodeIndex, int _numValuesPerNode, 
                    __global __read_only int *_tsp) {
  return as_float(_tsp[_bstNodeIndex*_numValuesPerNode + 3]);
}

float SpatialError(int _bstNodeIndex, int _numValuesPerNode, 
                   __global __read_only int *_tsp) {
  return as_float(_tsp[_bstNodeIndex*_numValuesPerNode + 2]);
}


// Given an octree node index, traverse the corresponding BST tree and look
// for a useful brick. 
bool TraverseBST(int _otNodeIndex,
                 int *_brickIndex, 
                 int _timestep,
                 __constant struct TraversalConstants *_constants,
                 __global volatile int *_reqList,
                 __global __read_only int *_tsp) {

  // Start at the root of the current BST
  int bstNodeIndex = _otNodeIndex;
  bool bstRoot = true;
  int timespanStart = 0;
  int timespanEnd = _constants->numTimesteps_;

   // Rely on structure for termination
   while (true) {
  
    // Update brick index (regardless if we use it or not)
    *_brickIndex = BrickIndex(bstNodeIndex, 
                              _constants->numValuesPerNode_,
                              _tsp);

    // If temporal error is ok
    // TODO float and <= errors
    if (TemporalError(bstNodeIndex, _constants->numValuesPerNode_,
                      _tsp) <= _constants->temporalTolerance_) {
      
      // If the ot node is a leaf, we can't do any better spatially so we 
      // return the current brick
      if (IsOctreeLeaf(_otNodeIndex, _constants->numValuesPerNode_, _tsp)) {
        return true;

      // All is well!
      } else if (SpatialError(bstNodeIndex, _constants->numValuesPerNode_,
               _tsp) <= _constants->spatialTolerance_) {
        return true;
         
      // If spatial failed and the BST node is a leaf
      // The traversal will continue in the octree (we know that
      // the octree node is not a leaf)
      } else if (IsBSTLeaf(bstNodeIndex, _constants->numValuesPerNode_, 
                           bstRoot, _tsp)) {
        return false;
      
      // Keep traversing BST
      } else {
        bstNodeIndex = ChildNodeIndex(bstNodeIndex,
                                      &timespanStart,
                                      &timespanEnd,
                                      _timestep,
                                      _constants->numValuesPerNode_,
                                      _constants->numOTNodes_,
                                      bstRoot,
                                      _tsp);
      }

    // If temporal error is too big and the node is a leaf
    // Return false to traverse OT
    } else if (IsBSTLeaf(bstNodeIndex, _constants->numValuesPerNode_, 
                         bstRoot, _tsp)) {
      return false;
    
    // If temporal error is too big and we can continue
    } else {
      bstNodeIndex = ChildNodeIndex(bstNodeIndex,
                                    &timespanStart,
                                    &timespanEnd,
                                    _timestep,
                                    _constants->numValuesPerNode_,
                                    _constants->numOTNodes_,
                                    bstRoot,
                                    _tsp);
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

// Traverse one ray through the volume, build brick list
void TraverseOctree(float3 _rayO, 
                    float3 _rayD,
                    float _maxDist,
                    __constant struct TraversalConstants *_constants,
                    __global volatile int *_reqList,
                    __global __read_only int *_tsp,
                    const int _timestep) {

  // Choose a stepsize that guarantees that we don't miss any bricks
  // TODO dynamic depending on brick dimensions
  float stepsize = _constants->stepsize_;
  float3 P = _rayO;
  // Keep traversing until the sample point goes outside the unit cube
  float traversed = 0.0f;
  
  int max_iterations = 50;
  int iterations = 0;
  bool ok = stepsize > 0.0f && stepsize < fabs(_maxDist);
  //while (traversed < _maxDist && iterations < max_iterations) {
  while (traversed < _maxDist) {
    
    // Reset traversal variables
    float3 offset = (float3)(0.0f);
    float boxDim = 1.0f;
    int child;

    // Init the octree node index to the root
    int otNodeIndex = OctreeRootNodeIndex();
    
    // Start traversing octree
    // Rely on finding a leaf for loop termination 
    
    while (true) {
      // See if the BST tree is good enough
      int brickIndex = 0;
      bool bstSuccess = TraverseBST(otNodeIndex, 
                                    &brickIndex,
                                    _timestep,
                                    _constants,
                                    _reqList,
                                    _tsp);



      if (bstSuccess) {

        // Add the found brick to brick list
        AddToList(brickIndex, _reqList);
        // We are now done with this node, so go to next
        break;

      // If the BST lookup failed but the octree node is a leaf, 
      // add the brick anyway (it is the BST leaf)
      } else if (IsOctreeLeaf(otNodeIndex, 
                              _constants->numValuesPerNode_, _tsp)) {
        AddToList(brickIndex, _reqList);
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
        
        if (_constants->gridType_ == 0) { // Cartesian
          child = EnclosingChild(P, boxMid, offset);
        } else { // Spherical (==1)
          child = EnclosingChild(CartesianToSpherical(P), boxMid, offset);
        }

        // Update offset
        UpdateOffset(&offset, boxDim, child);

        // Update node index to new node
        //int oldIndex = otNodeIndex;
        otNodeIndex = OTChildIndex(otNodeIndex, _constants->numValuesPerNode_,
                                  child, _tsp);
      }
    } // while traversing

    // Update 
    traversed = traversed + stepsize;
    P = P + stepsize * _rayD;

  } // while (traversed < maxDist)
  
  
}

__kernel void TSPTraversal(__read_only image2d_t _cubeFront,
                           __read_only image2d_t _cubeBack,
                           __constant struct TraversalConstants *_constants,
                           __global __read_only int *_tsp,
                           __global int *_reqList,
                           const int _timestep) {
    
    // Kernel should be launched in 2D with one work item per pixel
    int2 intCoords = (int2)(get_global_id(0), get_global_id(1));

    // Sampler for color cube reading
    const sampler_t sampler = CLK_NORMALIZED_COORDS_FALSE;

    // Read from color cube textures
    float4 cubeFrontColor = read_imagef(_cubeFront, sampler, intCoords);
    
    if (length(cubeFrontColor.xyz) == 0.0) return;
    float4 cubeBackColor = read_imagef(_cubeBack, sampler, intCoords);

    // Figure out ray direction 
    float maxDist = length(cubeBackColor.xyz-cubeFrontColor.xyz);
    float3 direction = normalize(cubeBackColor.xyz-cubeFrontColor.xyz);
    
    // Traverse octree and fill the brick request list
    TraverseOctree(cubeFrontColor.xyz, direction, maxDist,
                   _constants,  _reqList, _tsp, _timestep);

    return;

}
