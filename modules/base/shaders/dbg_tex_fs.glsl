#include "fragment.glsl"

uniform sampler2D tex;

in vec2 uv;

Fragment getFragment() {
  Fragment frag;
  frag.disableLDR2HDR = true;
  frag.color = texture(tex, uv).rrrr;
  return frag;
}
