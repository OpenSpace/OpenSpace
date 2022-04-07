
#version __CONTEXT__

#include "PowerScaling/powerScaling_vs.hglsl"

in dvec4 in_position;
in dvec4 in_colormap;

out float vs_screenSpaceDepth;
out float vs_scaleFactor;
out vec4 colorMap;

uniform dmat4 modelViewProjectionTransform;
uniform float scaleFactor;

void main() {
  vec4 positionClipSpace = vec4(modelViewProjectionTransform * in_position);
  vec4 positionScreenSpace = vec4(z_normalization(positionClipSpace));

  vs_screenSpaceDepth = positionScreenSpace.w;
  vs_scaleFactor = scaleFactor;
  colorMap = vec4(in_colormap);

  gl_PointSize = scaleFactor;
  gl_Position = positionScreenSpace;
}