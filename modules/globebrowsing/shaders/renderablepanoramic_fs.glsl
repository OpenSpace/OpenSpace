
uniform sampler2D texture1;

in vec2 vs_st;
in vec4 vs_position;
in vec3 vs_normal;

out vec4 outColor;

#include "fragment.glsl"
#include "PowerScaling/powerScaling_fs.hglsl"

Fragment getFragment() {
  Fragment frag;
  vec4 diffuse;
  vec2 texCoord = vs_st;

  if (vs_st.y < 0.01) diffuse = vec4(0,0,1,1);
  else if (vs_st.y > 0.99) diffuse = vec4(1,0,0,1);
  else diffuse = texture(texture1, texCoord);
  frag.color = diffuse;
  frag.depth = 1;
  return frag;
}
