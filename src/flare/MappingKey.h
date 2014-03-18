#ifndef MAPPINGKEY_H
#define MAPPINGKEY_H


/* 
Author: Victor Sand (victor.sand@gmail.com)
Mapping key used in transfer function. A key includes an intensity
for which the key is active, and RGBA values for that specific intensity.
*/

#include <iostream>
#include <flare/Utils.h>

namespace osp {

class MappingKey {
public:
    MappingKey();
    MappingKey(const MappingKey &_mappingKey);
    MappingKey(float _intensity, unsigned int _r, unsigned int _g,
               unsigned int _b, unsigned int _a);
    ~MappingKey();

    float Intensity() const { return intensity_; }
    unsigned int R() const { return r_; }
    unsigned int G() const { return g_; }
    unsigned int B() const { return b_; }
    unsigned int A() const { return a_; }         
    unsigned int Channel(unsigned int _channel) const { 
      switch (_channel) {
        case 0:
         return r_;
        case 1:
         return g_;
        case 2:
         return b_;
        case 3:
         return a_;
        default:
          ERROR("Invalid channel, returning 0");
          return 0;
      }
    }

    void SetValues(float _intensity, unsigned int _r, unsigned int _g,
                   unsigned int _b, unsigned int _a);
    
    // Comparsion by intensity for sorting purposes
    bool operator<(const MappingKey &_mappingKey) const;        
    MappingKey & operator=(const MappingKey &_mappingKey);

private:
    float intensity_;
    unsigned int r_;
    unsigned int g_;
    unsigned int b_;
    unsigned int a_;
};
    
}
    
std::ostream& operator<<(std::ostream &os,
                         const osp::MappingKey &_mappingKey);

#endif
