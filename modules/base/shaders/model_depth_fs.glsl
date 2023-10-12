#include "fragment.glsl"

Fragment getFragment() {
  Fragment frag;
  frag.color = vec4(0, 1, 0, 1);
  frag.depth = gl_FragCoord.z;
  // frag.disableLDR2HDR = true;
  return frag;
}
