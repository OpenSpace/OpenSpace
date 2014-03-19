// Needs to mirror struct on host
struct KernelConstants {
  float stepSize;
  float intensity;
  int aDim;
  int bDim;
  int cDim;
};


// Turn normalized [0..1] coordinates into array index 
int CoordsToIndex(float3 _coordinates, 
                  int3 _dimensions) {
  // Put coords in [0 .. dim-1] range
  int x = (float)(_dimensions.x-1) * _coordinates.x;
  int y = (float)(_dimensions.y-1) * _coordinates.y;
  int z = (float)(_dimensions.z-1) * _coordinates.z;
  // Return index
  return x + y*_dimensions.x + z*_dimensions.x*_dimensions.y;
}


// Linearly interpolate between two values. Distance
// is assumed to be normalized.
float Lerp(float _v0, float _v1, float _d) {
  return _v0*(1.0 - _d) + _v1*_d;
}

// Sample a volume given spherical integer coordinates
float Sample(__global __read_only float *_data,
             int3 _coords,
             int3 _dims) {

  int r = (_coords.x < _dims.x) ? _coords.x : _dims.x-1;
  int t = (_coords.y < _dims.y) ? _coords.y : _dims.y-1;
  int p = (_coords.z < _dims.z) ? _coords.z : _dims.z-1;
  int idx = r + t*_dims.x + p*_dims.x*_dims.y;
  return _data[idx];
}

// Trilinear interpolation and sampling
float Trilerp(__global __read_only float *_data,
              float3 _spherical,
              int3 _dims) {

  // TODO fix wrap issue
  // Get coordinates in [0..dim-1] range
  float r = (float)(_dims.x-1) * _spherical.x;
  float t = (float)(_dims.y-1) * _spherical.y;
  float p = (float)(_dims.z-1) * _spherical.z;

  // Lower values
  int r0 = (int)floor(r);
  int t0 = (int)floor(t);
  int p0 = (int)floor(p);

  // Upper values
  int r1 = r0+1;
  int t1 = t0+1;
  int p1 = p0+1;

  // Interpolation values
  float dr = r - floor(r);
  float dt = t - floor(t);
  float dp = p - floor(p);

  // Sample corners
  float v000 = Sample(_data, (int3)(r0, t0, p0), _dims);
  float v100 = Sample(_data, (int3)(r1, t0, p0), _dims);
  float v010 = Sample(_data, (int3)(r0, t1, p0), _dims);
  float v110 = Sample(_data, (int3)(r1, t1, p0), _dims);
  float v001 = Sample(_data, (int3)(r0, t0, p1), _dims);
  float v101 = Sample(_data, (int3)(r1, t0, p1), _dims);
  float v011 = Sample(_data, (int3)(r0, t1, p1), _dims);
  float v111 = Sample(_data, (int3)(r1, t1, p1), _dims);

  // Interpolate
  float v00 = Lerp(v000, v100, dr);
  float v10 = Lerp(v010, v110, dr);
  float v01 = Lerp(v001, v101, dr);
  float v11 = Lerp(v011, v111, dr);
  float v0 = Lerp(v00, v10, dt);
  float v1 = Lerp(v01, v11, dt);
  float v = Lerp(v0, v1, dp);

  return min(v, 1.0);
}


// Turn normalized [0..1] cartesian coordinates 
// to normalized spherical [0..1] coordinates
float3 CartesianToSpherical(float3 _cartesian) {
  // Put cartesian in [-1..1] range first
  _cartesian = (float3)(-1.0) + 2.0* _cartesian;
  float r = length(_cartesian);
  float theta, phi;
  if (r == 0.0) {
    theta = phi = 0.0;
  } else {
    theta = acos(_cartesian.z/r) / M_PI;
    phi = (M_PI + atan2(_cartesian.y, _cartesian.x)) / (2.0*M_PI);
  }
  r = r / sqrt(3.0);
  // Sampler ignores w component
  return (float3)(r, theta, phi);
}

float4 TransferFunction(__global __read_only float *_tf, float _i) {
  // TODO remove 512 hard-coded value and change to 1D texture
  int i0 = (int)floor(511.0*_i);
  int i1 = (i0 < 511) ? i0+1 : i0;
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

  return (float4)(tfr, tfg, tfb, tfa);
}

__kernel void
Raycaster(__global __read_only image2d_t _cubeFront,
          __global __read_only image2d_t _cubeBack,
          __global __write_only image2d_t _output,
          __global __read_only float * _voxelData,
          __constant struct KernelConstants *_constants,
          __global __read_only float *_tf) {


  int3 dimensions = (int3)(_constants->aDim,
                           _constants->bDim,
                           _constants->cDim);
  
  // Kernel should be launched in 2D with one work item per pixel
  int idx = get_global_id(0);
  int idy = get_global_id(1);
  int2 intCoords = (int2)(idx, idy);

  // Sampler for texture reading
  const sampler_t sampler = CLK_NORMALIZED_COORDS_FALSE;
  const sampler_t volumeSampler = CLK_FILTER_LINEAR | 
                                  CLK_NORMALIZED_COORDS_TRUE |
                                  CLK_ADDRESS_CLAMP_TO_EDGE;
    
  // Read from textures
  float4 cubeFrontColor = read_imagef(_cubeFront, sampler, intCoords);
  float4 cubeBackColor = read_imagef(_cubeBack, sampler, intCoords);

  // Figure out the direction
  float3 direction = (cubeBackColor-cubeFrontColor).xyz;
  float maxDistance = length(direction);
  direction = normalize(direction);

  // Keep track of distance traversed
  float traversed = 0.0;

  // Sum colors
  float stepSize = _constants->stepSize;
  float3 samplePoint = cubeFrontColor.xyz;
  float3 spherical;
  float4 associatedColor;
  float4 color = (float4)(0.0, 0.0, 0.0, 0.0);
   
  while (traversed < maxDistance) {
    spherical = CartesianToSpherical(samplePoint);
    
    //int index = timestepOffset + CoordsToIndex(spherical, dimensions);
    // Get intensity from data
    //float i = _voxelData[index];
    float i = Trilerp(_voxelData, spherical, dimensions);
    
    // Texture stores intensity in R channel
    //float i = read_imagef(_voxelData, volumeSampler, spherical).x;

    // Front-to-back compositing
    float4 tf = TransferFunction(_tf, i);
    color += (1.0 - color.w)*tf;
    
    //  color += (float4)(i, i, i, 1.0);
    samplePoint += direction * stepSize;
    traversed += stepSize;
  }

  // Output
  float intensity = _constants->intensity;
  color *= intensity*stepSize;

  // Write to image
  write_imagef(_output, intCoords, color);
  
}

