
float3 CartesianToSpherical(float3 _cartesian);
float intensityNormalizer(float intensity, float iMin, float iMax);

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

float intensityNormalizer(float intensity, float iMin, float iMax) {
  float i = clamp(intensity, iMin, iMax);
  i = (i - iMin) / (iMax - iMin);
  return clamp(i, 0.0f, 1.0f);
}



