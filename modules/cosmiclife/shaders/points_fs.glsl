#include "fragment.glsl"

in float vs_screenSpaceDepth;

uniform vec3 color;
uniform float alphaValue;
uniform sampler2D spriteTexture;

Fragment getFragment() {
  Fragment frag;

  if (alphaValue == 0.0) {
    discard;
  }

  frag.color = texture(spriteTexture, gl_PointCoord) * vec4(color, alphaValue);
  //frag.depth = gs_screenSpaceDepth;
  frag.depth = vs_screenSpaceDepth;
  frag.blend = BLEND_MODE_ADDITIVE;

  return frag;
}