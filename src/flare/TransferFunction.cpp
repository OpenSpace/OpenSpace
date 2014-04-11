/*
 * Author: Victor Sand (victor.sand@gmail.com)
 *
 */

#include <openspace/flare/TransferFunction.h>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <openspace/flare/Utils.h>
#include <boost/tokenizer.hpp>
#include <boost/lexical_cast.hpp>
#include <math.h>

using namespace osp;

TransferFunction * TransferFunction::New() {
  return new TransferFunction();
}

TransferFunction::TransferFunction() : 
  texture_(nullptr),
  floatData_(NULL),
  width_(0),
  lower_(0.f),
  upper_(1.f),
  generatedTexture_(false),
  inFilename_("NotSet"),
  interpolation_(TransferFunction::LINEAR) {}


TransferFunction::TransferFunction(const TransferFunction &_tf) {
  if (floatData_) delete floatData_; 
}


TransferFunction::~TransferFunction() {}

bool TransferFunction::ReadFile() {
  std::ifstream in;
  in.open(inFilename_.c_str());
  if (!in.is_open()) {
    ERROR("Could not open file " << inFilename_);
    return false;
  }
  std::string line;
  
  mappingKeys_.clear();

  while (std::getline(in, line)) {
    boost::char_separator<char> sep(" ");
    boost::tokenizer<boost::char_separator<char> > tokens(line, sep);

    boost::tokenizer<boost::char_separator<char> >::iterator it=tokens.begin();
    float intensity;
    unsigned int r, g, b, a;
    if (*it == "width") {
      width_ = boost::lexical_cast<unsigned int>(*(boost::next(it)));
    } else if (*it == "lower") {
      lower_ = boost::lexical_cast<float>(*(boost::next(it)));
    } else if (*it == "upper") {
      upper_ = boost::lexical_cast<float>(*(boost::next(it)));
    } else if (*it == "mappingkey") {
      intensity = boost::lexical_cast<float>(*(++it));
      r = boost::lexical_cast<unsigned int>(*(++it));
      g = boost::lexical_cast<unsigned int>(*(++it));
      b = boost::lexical_cast<unsigned int>(*(++it));
      a = boost::lexical_cast<unsigned int>(*(++it));
      MappingKey mp(intensity, r, g, b, a);
      mappingKeys_.insert(mp);
    } else {
      ERROR("Parsing error");
      return false;
    }
  }
   
  in.close();
  return true;
}

void TransferFunction::SetInFilename(const std::string &_inFilename) {
  inFilename_ = _inFilename;
}

std::string TransferFunction::ToString() const {
  std::stringstream ss;
  ss << "Width: " << width_ << "\n";
  ss << "Range: [" << lower_ << " " << upper_ << "]\n";
  ss << "Number of mapping keys: " << mappingKeys_.size(); 
  for (std::set<MappingKey>::iterator it = mappingKeys_.begin();
       it != mappingKeys_.end();
       it++) {
    ss << "\n" << *it;
  }
  return ss.str();
}

bool TransferFunction::Sample(float &_r, float &_g, float &_b, float &_a, 
                              float _i) {

  if (!floatData_) {
    ERROR("TF Sample() - no float data");
    return false;
  }

  if (_i < 0.f || _i > 1.f) { 
    //ERROR("Invalid TF sample intensity: " << _i);
    //return false;
  }

  if (_i < 0.f) _i = 0.f;
  if (_i > 1.0) _i = 1.f;

  int i0 = static_cast<int>(floorf((width_-1)*_i));
  int i1 = (i0 < width_-1) ? i0+1 : i0;
  float di = _i - floor(_i);
  
  float tfr0 = floatData_[i0*4+0];
  float tfr1 = floatData_[i1*4+0];
  float tfg0 = floatData_[i0*4+1];
  float tfg1 = floatData_[i1*4+1];
  float tfb0 = floatData_[i0*4+2];
  float tfb1 = floatData_[i1*4+2];
  float tfa0 = floatData_[i0*4+3];
  float tfa1 = floatData_[i1*4+3];

  _r = Lerp(tfr0, tfr1, di);
  _g = Lerp(tfg0, tfg1, di);
  _b = Lerp(tfb0, tfb1, di);
  _a = Lerp(tfa0, tfa1, di);

  return true;
}

bool TransferFunction::ConstructTexture() {

  if (mappingKeys_.empty()) {
    ERROR("No mapping keys");
    return false;
  }

  if (interpolation_ == TransferFunction::LINEAR) {

    // Float values for R, G, B and A channels
    // TODO temp
    if (floatData_ == NULL) {
      floatData_ = new float[4*width_];
    }
    
    unsigned int lowerIndex = (unsigned int)floorf(lower_*(float)width_);
    unsigned int upperIndex = (unsigned int)floorf(upper_*(float)width_); 

    // Loop over channels
    for (unsigned int channel=0; channel<4; channel++) {

      // Init some mapping keys
      std::set<MappingKey>::iterator it = mappingKeys_.begin();
      MappingKey prev(lower_, 0, 0, 0, 0);
      MappingKey next = *it;
      MappingKey last(upper_, 0, 0, 0, 0);
      // First mapping key switch is a special case
      bool first = true;

      for (unsigned int i=0; i<width_; i++) {

        if (i <= lowerIndex || i > upperIndex) {
          floatData_[4*i + channel] = 0.f;
        } else {

          // See if it's time to go to next pair of mapping keys
          float pos = (float)i/width_;
          if (!first && pos > next.Intensity()) {
            prev = next;
            it++;
            if (it == mappingKeys_.end()) {
              next = last;
            } else {
              next = *it;
            }
          }

          first = false;

          // Interpolate linearly between prev and next mapping key   
          //DEBUG("i: " << i);
          //DEBUG("pos: " << pos);
          //DEBUG("prev.Intensity(): " << prev.Intensity());
          float dist = pos-prev.Intensity();
          //DEBUG("dist: " << dist);
          float weight = dist/(next.Intensity()-prev.Intensity());
          //DEBUG("weight: " << weight);
          floatData_[4*i + channel] = 
            ((float)prev.Channel(channel)*(1.f-weight)+ 
            (float)next.Channel(channel)*weight)/255.f;

        }
      }
    }

    // Create and fill the texture
    std::vector<unsigned int> dim(2);
    dim[0] = width_;
    dim[1] = 1;
    texture_ = new ghoul::opengl::Texture(floatData_, glm::size3_t(width_,1,1),ghoul::opengl::Texture::Format::RGBA, GL_RGBA, GL_FLOAT);
    texture_->uploadTexture();
    //texture_ = Texture2D::New(dim);
    //texture_->Init(&floatData_[0]);
    generatedTexture_ = true; 
    
    //delete[] values;
  } else {
    ERROR("Invalid interpolation mode");
    return false;
  }

  return true;

}

std::ostream & operator<<(std::ostream &os, const TransferFunction &_tf) {
  os << _tf.ToString(); 
  return os;
}

