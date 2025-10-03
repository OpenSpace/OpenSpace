#include "fragment.glsl"

Fragment getFragment() {
  Fragment frag;
  frag.depth = gl_FragCoord.z;
  frag.disableLDR2HDR = true;
  frag.disableDepthNormalization = true;
  return frag;
}
