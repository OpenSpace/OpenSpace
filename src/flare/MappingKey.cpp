#include <openspace/flare/MappingKey.h>
#include <iostream>
#include <iomanip>

using namespace osp;

MappingKey::MappingKey() 
  : intensity_(0.f),
    r_(0),
    g_(0),
    b_(0),
    a_(0) {}

MappingKey::MappingKey(const MappingKey &_mk) 
  : intensity_(_mk.intensity_),
    r_(_mk.r_),
    g_(_mk.g_),
    b_(_mk.b_),
    a_(_mk.a_) {}

MappingKey::MappingKey(float _intensity, unsigned int _r, unsigned int _g,
                       unsigned int _b, unsigned int _a) 
  : intensity_(_intensity),
    r_(_r),
    g_(_g),
    b_(_b),
    a_(_a) {}

MappingKey::~MappingKey() {}

void MappingKey::SetValues(float _intensity, unsigned int _r, unsigned int _g,
                           unsigned int _b, unsigned int _a) {
  intensity_ = _intensity;
  r_ = _r;
  g_ = _g;
  b_ = _b;
  a_ = _a;
}

std::ostream& operator<<(std::ostream &os,
                         const MappingKey &_mk) {
  os  << _mk.Intensity() << " [" 
     << _mk.R() << " " << _mk.G() << " " << _mk.B() << " " << _mk.A() << "]";
  return os;
}

bool MappingKey::operator<(const MappingKey &_mappingKey) const {
  return intensity_ < _mappingKey.intensity_;
}

MappingKey & MappingKey::operator=(const MappingKey &_mk) {
  if (this == &_mk) return *this;
  SetValues(_mk.intensity_, _mk.r_, _mk.g_, _mk.b_, _mk.a_);
  return *this;
}
