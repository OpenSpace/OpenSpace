/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

// open space includes
#include <openspace/util/powerscaledscalar.h>

// std includes
#include <cstdio>
#include <cstdlib>
#include <cmath>

namespace openspace {

    namespace {
        const double k = 10.0;
    }


PowerScaledScalar::PowerScaledScalar() : _data(0.f) {

}

PowerScaledScalar::PowerScaledScalar(const glm::vec2 &v) {
    _data = std::move(v);
}

PowerScaledScalar::PowerScaledScalar(float f1, float f2) {
    _data = glm::vec2(f1, f2);
}

PowerScaledScalar PowerScaledScalar::CreatePSS(double d1) {
    char buff[30];

    // find the number with maximum number of digits
    double ad1 = std::abs(d1);

    // find out how many digits
#ifdef _MSC_VER
    sprintf_s(buff, 30, "%.0f", ad1);
#else
    sprintf(buff, "%.0f", ad1);
#endif
    size_t digits = strlen(buff)-1;

    // rescale and return
    double tp = 1.0 / pow(k, digits);
    return PowerScaledScalar(static_cast<float>(d1*tp), static_cast<float>(digits));
}

const glm::vec2& PowerScaledScalar::vec2() const {
    return _data;
}

float PowerScaledScalar::lengthf() const {
    return static_cast<float>(_data[0] * pow(k,_data[1]));
}

double PowerScaledScalar::lengthd() const {
    return static_cast<double>(_data[0]) * pow(k, static_cast<double>(_data[1]));
}

PowerScaledScalar& PowerScaledScalar::operator=(const PowerScaledScalar &rhs) {
    if (this != &rhs){
        this->_data = rhs._data;
    }
    return *this;  // Return a reference to myself.
}

PowerScaledScalar & PowerScaledScalar::operator=(const glm::vec2 &rhs) {
    this->_data = glm::vec2(rhs);
    return *this;  // Return a reference to myself.
}

PowerScaledScalar & PowerScaledScalar::operator=(float rhs) {
    this->_data = glm::vec2(rhs,0.0);
    return *this;  // Return a reference to myself.
}

PowerScaledScalar & PowerScaledScalar::operator+=(const PowerScaledScalar &rhs) {

    double ds = this->_data[1] - rhs._data[1];
    if(ds >= 0.0) {
        *this = PowerScaledScalar(
            static_cast<float>(rhs._data[0] * pow(k, -ds) + this->_data[0]), 
            this->_data[1]);
    } else {
        *this = PowerScaledScalar(
            static_cast<float>(rhs._data[0] + this->_data[0] * pow(k, ds)), 
            rhs._data[1]);
    }

    return *this;
}

const PowerScaledScalar PowerScaledScalar::operator+(const PowerScaledScalar &rhs) const {
    return PowerScaledScalar(*this) += rhs;
}

PowerScaledScalar & PowerScaledScalar::operator-=(const PowerScaledScalar &rhs) {

    double ds = this->_data[1] - rhs._data[1];
    if(ds >= 0.0) {
        *this = PowerScaledScalar(
            static_cast<float>(-rhs._data[0] * pow(k, -ds) + this->_data[0]), 
            this->_data[1]);
    } else {
        *this = PowerScaledScalar(
            static_cast<float>(-rhs._data[0] + this->_data[0] * pow(k, ds)), 
            rhs._data[1]);
    }

    return *this;
}

const PowerScaledScalar PowerScaledScalar::operator-(const PowerScaledScalar &rhs) const {
    return PowerScaledScalar(*this) -= rhs;
}

PowerScaledScalar & PowerScaledScalar::operator*=(const PowerScaledScalar &rhs) {
    double ds = this->_data[1] - rhs._data[1];
    if(ds >= 0.0) {
        *this = PowerScaledScalar(
            static_cast<float>(rhs._data[0] * pow(k, -ds) * this->_data[0]), 
            this->_data[1] + this->_data[1]);
    } else {
        *this = PowerScaledScalar(
            static_cast<float>(rhs._data[0] * this->_data[0] * pow(k, ds)), 
            rhs._data[1] + rhs._data[1]);
    }

    return *this;
}


const PowerScaledScalar PowerScaledScalar::operator*(const PowerScaledScalar &rhs) const {
    return PowerScaledScalar(*this) *= rhs;
}


PowerScaledScalar & PowerScaledScalar::operator*=(const float &rhs) {
    double ds = this->_data[1];
    if(ds >= 0) {
        *this = PowerScaledScalar(
            static_cast<float>(rhs*pow(k, -ds) * this->_data[0]), 
            this->_data[1] + this->_data[1]);
    } else {
        *this = PowerScaledScalar(
            static_cast<float>(rhs * this->_data[0] * pow(k, ds)), 0.0f);
    }

    return *this;
}

const PowerScaledScalar PowerScaledScalar::operator*(const float &rhs) const {
    return PowerScaledScalar(*this) *= rhs;
}

float& PowerScaledScalar::operator[](unsigned int idx) {
    return _data[idx];
}

float PowerScaledScalar::operator[](unsigned int idx) const {
    return _data[idx];
}

bool PowerScaledScalar::operator==(const PowerScaledScalar &other) const {
    return _data == other._data;
}

bool PowerScaledScalar::operator<(const PowerScaledScalar &other) const {
    double ds = this->_data[1] - other._data[1];
    if(ds >= 0) {
        double upscaled = other._data[0]*pow(k,-ds);
        return _data[0] < upscaled;
        /*
        bool retur =(vec_[0] < upscaled);
        std::printf("this: %f, upscaled: %f, this<upscaled: %i\n", vec_[0], upscaled, retur);
        return retur;
        */
    } else {
        double upscaled = _data[0]*pow(k,-ds);
        return other._data[0] > upscaled;
    }
}

bool PowerScaledScalar::operator>(const PowerScaledScalar &other) const {
    double ds = this->_data[1] - other._data[1];
    if(ds >= 0) {
        double upscaled = other._data[0]*pow(k,-ds);
        return _data[0] > upscaled;
    } else {
        double upscaled = _data[0]*pow(k,-ds);
        return other._data[0] < upscaled;
    }
}

bool PowerScaledScalar::operator<=(const PowerScaledScalar &other) const {
    return *this < other || *this == other;
}

bool PowerScaledScalar::operator>=(const PowerScaledScalar &other) const {
    return *this > other || *this == other;
}

bool PowerScaledScalar::operator==(double other) const {
    double ds = this->_data[1];
    if(ds >= 0) {
        double upscaled = other*pow(k,-ds);
        return _data[0] == upscaled;
    } else {
        double upscaled = _data[0]*pow(k,-ds);
        return other == upscaled;
    }
}

bool PowerScaledScalar::operator<(double other) const {
    double ds = this->_data[1];
    if(ds >= 0) {
        double upscaled = other*pow(k,-ds);
        return _data[0] < upscaled;
    } else {
        double upscaled = _data[0]*pow(k,-ds);
        return other > upscaled;
    }
}

bool PowerScaledScalar::operator>(double other) const {
    double ds = this->_data[1];
    if(ds >= 0) {
        double upscaled = other*pow(k,-ds);
        return _data[0] > upscaled;
    } else {
        double upscaled = _data[0]*pow(k,-ds);
        return other < upscaled;
    }
}

bool PowerScaledScalar::operator<=(double other) const {
    return *this < other || *this == other;
}

bool PowerScaledScalar::operator>=(double other) const {
    return *this > other || *this == other;
}

std::ostream& operator<<(::std::ostream& os, const PowerScaledScalar& rhs) {
    os << "(" << rhs[0] << ", " << rhs[1] << ")";
    return os;
}

} // namespace openspace
