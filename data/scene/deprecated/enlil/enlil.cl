#include <volume_helpers.cl>
#include <volume_raycasting.cl>

__kernel void volumeraycaster(
          __read_only image2d_t _cubeFront,
          __read_only image2d_t _cubeBack,
          __write_only image2d_t _output,
          __read_only image3d_t _voxelData,
          __read_only image3d_t _voxelData2,
          __read_only RC_TF_TYPE _t1);
bool float3cmp(float3 f1, float3 f2);
bool float3cmp(float3 f1, float3 f2) {
  if(f1.x != f2.x)
    return false;
  if(f1.y != f2.y)
    return false;
  if(f1.z != f2.z)
    return false;
  return true;
}

//#define MIP
#define SHOWFUNC

__kernel void volumeraycaster(
          __read_only image2d_t _cubeFront,
          __read_only image2d_t _cubeBack,
          __write_only image2d_t _output,
          __read_only image3d_t _voxelData,
          __read_only image3d_t _voxelData2,
          __read_only RC_TF_TYPE _t1) {
  // Kernel should be launched in 2D with one work item per pixel

  RC_DEFINE_TEXTUE_COORDINATES(intCoords);
  RC_DEFINE_VOLUME3D_DIMENSIONS(voxelDataDimensions,_voxelData);

  // Sampler for texture reading
  const sampler_t sampler = CLK_NORMALIZED_COORDS_FALSE;
  const sampler_t transferSampler = CLK_ADDRESS_CLAMP_TO_EDGE |
                                    CLK_NORMALIZED_COORDS_TRUE |
                                    CLK_FILTER_LINEAR;
  const sampler_t volumeSampler = CLK_ADDRESS_CLAMP_TO_EDGE |
                                  CLK_FILTER_LINEAR |
                                  CLK_NORMALIZED_COORDS_TRUE;
  // Read from textures
  float3 front = read_imagef(_cubeFront, sampler, intCoords).xyz;
  float3 back = read_imagef(_cubeBack, sampler, intCoords).xyz;
  float3 direction;
  float tIncr;
  float tEnd;

  raySetup(front, back, voxelDataDimensions, &direction, &tIncr, &tEnd);
  float4 finalColor = (float4)(0.0,0.0,0.0,0.0);

#ifdef MIP
  float intensity= 0.0f;
  RC_BEGIN_LOOP
    float3 position = front + direction * t;
    float3 sph_pos = CartesianToSpherical(position);
    float4 sp = (float4)(sph_pos.x,sph_pos.y,sph_pos.z,0.0f);
    float i = read_imagef(_voxelData, volumeSampler, sp).x;
    intensity = max(intensity, i);
  RC_END_LOOP(finalColor)
  intensity *= 10.0f;
  finalColor = (float4)(intensity, intensity,intensity,1.0f);
#else
  float4 color;
  RC_BEGIN_LOOP
    float3 position = front + direction * t;
    float3 sph_pos = CartesianToSpherical(position);
    float4 sp = (float4)(sph_pos.x,sph_pos.y,sph_pos.z,0.0f);
    float intensity1 = read_imagef(_voxelData, volumeSampler, sp).x;
    float intensity2 = read_imagef(_voxelData2, volumeSampler, sp).x;

    //float intensity = intensity1 - intensity2;
    float intensity = intensity1;
    intensity = clamp(intensity, 0.0f, 1.0f);

    color = read_imagef(_t1, transferSampler, RC_TF_MAP(intensity));
    color *= tIncr * 10.0f;

    float alpha = (1.0f - finalColor.w);
    float outputRed = (finalColor.x ) + (color.x * alpha);
    float outputGreen = (finalColor.y ) + (color.y * alpha);
    float outputBlue = (finalColor.z ) + (color.z * alpha);
    float outputAlpha = finalColor.w + alpha*color.w;
    finalColor = (float4)(outputRed, outputGreen, outputBlue, outputAlpha);

  RC_END_LOOP(finalColor)
#endif

//finalColor = (float4)(front.x,front.y,front.z,1.0f);
//finalColor = (float4)(back.x,back.y,back.z,1.0f);
write_imagef(_output, intCoords, finalColor);

#ifdef SHOWFUNC  
  int showfunc_size = 50;
  if(intCoords.y > get_global_size(1) - showfunc_size) {
    float normalizedIntensity = (float)intCoords.x / (float) get_global_size(0);

    float4 tfc = read_imagef(_t1, transferSampler, RC_TF_MAP(normalizedIntensity));
    
    float f = 1.0f;
    float4 outcolor = (float4)(tfc.x*f, tfc.y*f, tfc.z*f, tfc.w*f);
    write_imagef(_output, intCoords, outcolor);
  }
#endif
  
}


