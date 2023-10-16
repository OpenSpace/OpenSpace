#include "fragment.glsl"

Fragment getFragment() {
  Fragment frag;
  frag.depth = gl_FragCoord.z;
  return frag;
}
