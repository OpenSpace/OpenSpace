#include <volume_helpers.cl>
#include <volume_raycasting.cl>

__kernel void volumeraycaster(
          __read_only image2d_t _cubeFront,
          __read_only image2d_t _cubeBack,
          __write_only image2d_t _output,
          __read_only image3d_t _voxelData,
          __read_only RC_TF_TYPE _t1);


__kernel void volumeraycaster(
          __read_only image2d_t _cubeFront,
          __read_only image2d_t _cubeBack,
          __write_only image2d_t _output,
          __read_only image3d_t _voxelData,
          __read_only RC_TF_TYPE _t1) {
  // Kernel should be launched in 2D with one work item per pixel
  int idx = get_global_id(0);
  int idy = get_global_id(1);

  int2 intCoords = (int2)(idx, idy);

  // Sampler for texture reading
  const sampler_t sampler = CLK_NORMALIZED_COORDS_FALSE;
  const sampler_t transferSampler = CLK_ADDRESS_CLAMP_TO_EDGE |
                                    CLK_NORMALIZED_COORDS_TRUE |
                                    CLK_FILTER_LINEAR;
  const sampler_t volumeSampler = CLK_ADDRESS_CLAMP_TO_EDGE |
                                  CLK_FILTER_LINEAR |
                                  CLK_NORMALIZED_COORDS_TRUE;
  // Read from textures
  float stepSize = 0.01f;
  float3 front = read_imagef(_cubeFront, sampler, intCoords).xyz;
  float3 back = read_imagef(_cubeBack, sampler, intCoords).xyz;
  float3 direction = back - front;
  float directionLength = length(direction);
  direction = normalize(direction);
  float3 position = front;
  float intensity= 0.0f;
  float4 color = (float4)(0.0,0.0,0.0,0.0);
  float4 finalColor = (float4)(0.0,0.0,0.0,0.0);

  int4 vol_dim = get_image_dim(_voxelData);

  int iterations = directionLength / stepSize;
  float iterationsf = iterations;
  iterationsf /= 10.0f;
  for(int i = 0; i < iterations && finalColor.w < 0.95f; ++i) {
    float3 sph_pos = CartesianToSpherical(position);
    float4 sp = (float4)(sph_pos.x,sph_pos.y,sph_pos.z,0.0f);
    //float4 sp = (float4)(position.x,position.y,position.z,0.0f);
    
    intensity = read_imagef(_voxelData, volumeSampler, sp).x;
    //float4 intensityvec = read_imagef(_voxelData, volumeSampler, sp);

    color = read_imagef(_t1, transferSampler, RC_TF_MAP(intensity));
    color /= iterationsf;

    float alpha = (1.0f - finalColor.w);
    float outputRed = (finalColor.x ) + (color.x * alpha);
    float outputGreen = (finalColor.y ) + (color.y * alpha);
    float outputBlue = (finalColor.z ) + (color.z * alpha);
    float outputAlpha = finalColor.w + alpha*color.w;

    finalColor = (float4)(outputRed, outputGreen, outputBlue, outputAlpha);

    //finalColor = max(finalColor, intensityvec); 
    position = position + direction * stepSize;
  }

  //color = (float4)(color.x,color.x,color.x,1.0f);
  //write_imagef(_output, intCoords, _cubeFront);
  //finalColor = (float4)(front.x,front.y,front.z,1.0f);
  write_imagef(_output, intCoords, finalColor);

  
}


