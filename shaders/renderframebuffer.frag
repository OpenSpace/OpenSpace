#include <#{fragmentPath}>

out vec4 _out_color_;

void main() {
     Fragment f = getFragment();
     _out_color_ = f.color;
     gl_FragDepth = f.depth;
}