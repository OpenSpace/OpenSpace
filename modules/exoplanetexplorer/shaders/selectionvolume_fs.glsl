#include "fragment.glsl"

in Data {
  float depth;
} in_data;

uniform vec4 color;

Fragment getFragment() {
  Fragment frag;
  frag.color = color;
  frag.depth = in_data.depth;
  return frag;
}
