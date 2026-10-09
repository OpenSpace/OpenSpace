#include "fragment.glsl"

in Data {
  flat float density;
  flat float selected;
  float depth;
} in_data;

uniform vec3 fixedColor;
uniform bool useColorMap;
uniform sampler1D colorMapTexture;
uniform vec2 densityRange;
uniform bool useLogScale;
uniform float densityThreshold;
uniform bool hasSelection;
uniform vec3 selectionColor;
uniform float unselectedOpacity;
uniform float opacity;

float normalizedDensity(float density) {
  if (useLogScale) {
    // Avoid log(0) when the range starts at zero density
    float lo = max(densityRange.x, densityRange.y * 1e-6);
    float span = max(log(densityRange.y) - log(lo), 1e-30);
    return clamp((log(max(density, lo)) - log(lo)) / span, 0.0, 1.0);
  }
  float span = max(densityRange.y - densityRange.x, 1e-30);
  return clamp((density - densityRange.x) / span, 0.0, 1.0);
}

Fragment getFragment() {
  vec2 coord = gl_PointCoord * 2.0 - 1.0;
  if (dot(coord, coord) > 1.0 || in_data.density < densityThreshold) {
    discard;
  }

  vec3 color = useColorMap ?
    texture(colorMapTexture, normalizedDensity(in_data.density)).rgb :
    fixedColor;
  float alpha = opacity;

  if (hasSelection) {
    if (in_data.selected > 0.5) {
      color = selectionColor;
    }
    else {
      alpha *= unselectedOpacity;
    }
  }

  if (alpha < 0.01) {
    discard;
  }

  Fragment frag;
  frag.color = vec4(color, alpha);
  frag.depth = in_data.depth;
  frag.gNormal = vec4(0.0, 0.0, -1.0, 1.0);
  return frag;
}
