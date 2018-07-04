/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <openspace/util/powerscaledcoordinate.h>

#include <openspace/util/powerscaledscalar.h>
#include <cstring>
#include <sstream>

namespace {
    constexpr const double k = 10.0;
} // namespace

namespace openspace {

PowerScaledCoordinate::PowerScaledCoordinate(PowerScaledCoordinate&& rhs) {
    _vec = std::move(rhs._vec);
}

PowerScaledCoordinate::PowerScaledCoordinate(glm::vec4 v) {
    _vec = std::move(v);
}

PowerScaledCoordinate::PowerScaledCoordinate(glm::vec3 v) {
    _vec = glm::vec4(v, 0.f);
}

PowerScaledCoordinate::PowerScaledCoordinate(float f1, float f2, float f3, float f4) {
    _vec = glm::vec4(f1, f2, f3, f4);
}

PowerScaledCoordinate::PowerScaledCoordinate(const PowerScaledCoordinate& rhs) {
    _vec = rhs._vec;
}

PowerScaledCoordinate
PowerScaledCoordinate::CreatePowerScaledCoordinate(double d1, double d2, double d3)
{
    char buff[600];

    // find the number with maximum number of digits
    double ad1 = std::abs(d1);
    double ad2 = std::abs(d2);
    double ad3 = std::abs(d3);
    double max = (ad2 > ad1) ? ad2 : (ad3 > ad1) ? ad3 : ad1;

    // find out how many digits
    // TODO: profile the next two lines and replace with something more efficient (ab)
#ifdef _MSC_VER
    sprintf_s(buff, 600, "%.0f", max);
    //sprintf(buff, "%.0f", max);
#else
    sprintf(buff, "%.0f", max);
#endif
    size_t digits = strlen(buff);

    //digits += 3;

    // rescale and return
    double tp = 1.0 / pow(k, digits);
    return PowerScaledCoordinate(
        static_cast<float>(d1 * tp),
        static_cast<float>(d2 * tp),
        static_cast<float>(d3 * tp),
        static_cast<float>(digits));
}

const glm::vec4& PowerScaledCoordinate::vec4() const
{
    return _vec;
}

glm::vec3 PowerScaledCoordinate::vec3() const
{
    return glm::vec3(_vec[0] * pow(k, _vec[3]), _vec[1] * pow(k, _vec[3]),
                     _vec[2] * pow(k, _vec[3]));
}

glm::dvec4 PowerScaledCoordinate::dvec4() const
{
    //return _vec;
    return glm::dvec4(_vec);
}

glm::dvec3 PowerScaledCoordinate::dvec3() const
{
    double p = pow(static_cast<double>(k), static_cast<double>(_vec[3]));
    return glm::dvec3( static_cast<double>(_vec[0]) * p, static_cast<double>(_vec[1]) * p,
        static_cast<double>(_vec[2]) * p);
}

PowerScaledScalar PowerScaledCoordinate::length() const
{
    return PowerScaledScalar(glm::length(glm::vec3(_vec[0], _vec[1], _vec[2])), _vec[3]);
}

glm::vec3 PowerScaledCoordinate::direction() const
{
    glm::vec3 tmp(_vec[0], _vec[1], _vec[2]);
    return glm::normalize(glm::vec3(_vec[0], _vec[1], _vec[2]));
}

PowerScaledCoordinate& PowerScaledCoordinate::operator=(const PowerScaledCoordinate& rhs)
{
    if (this != &rhs) {
        _vec = rhs._vec;
    }
    return *this;  // Return a reference to myself.
}

PowerScaledCoordinate& PowerScaledCoordinate::operator=(const glm::vec4& rhs) {
    _vec = rhs;
    return *this;  // Return a reference to myself.
}

PowerScaledCoordinate& PowerScaledCoordinate::operator=(const glm::vec3& rhs) {
    _vec = glm::vec4(rhs[0], rhs[1], rhs[2], 0.0);
    return *this;  // Return a reference to myself.
}

PowerScaledCoordinate& PowerScaledCoordinate::operator=(const glm::dvec4& rhs) {
    _vec = glm::vec4(rhs);
    return *this;  // Return a reference to myself.
}

PowerScaledCoordinate& PowerScaledCoordinate::operator=(const glm::dvec3& rhs) {
    _vec = glm::vec4(rhs[0], rhs[1], rhs[2], 0.0);
    return *this;  // Return a reference to myself.
}

PowerScaledCoordinate& PowerScaledCoordinate::operator=(PowerScaledCoordinate&& rhs) {
    _vec = std::move(rhs._vec);
    return *this;
}

PowerScaledCoordinate& PowerScaledCoordinate::operator+=(const PowerScaledCoordinate& rhs)
{
    double ds = _vec[3] - rhs._vec[3];
    if (ds >= 0.0) {
        double p = pow(k, -ds);
        *this = PowerScaledCoordinate(static_cast<float>(rhs._vec[0] * p + _vec[0]),
                                      static_cast<float>(rhs._vec[1] * p + _vec[1]),
                                      static_cast<float>(rhs._vec[2] * p + _vec[2]),
                                      _vec[3]);
    } else {
        double p = pow(k, ds);
        *this = PowerScaledCoordinate(static_cast<float>(rhs._vec[0] + _vec[0] * p),
                                      static_cast<float>(rhs._vec[1] + _vec[1] * p),
                                      static_cast<float>(rhs._vec[2] + _vec[2] * p),
                                      rhs._vec[3]);
    }

    return *this;
}

PowerScaledCoordinate PowerScaledCoordinate::
      operator+(const PowerScaledCoordinate& rhs) const
{
    return PowerScaledCoordinate(*this) += rhs;
}

PowerScaledCoordinate& PowerScaledCoordinate::operator-=(const PowerScaledCoordinate& rhs)
{
    double ds = this->_vec[3] - rhs._vec[3];
    if (ds >= 0.0) {
        double p = pow(k, -ds);
        *this = PowerScaledCoordinate(static_cast<float>(-rhs._vec[0] * p + _vec[0]),
                                      static_cast<float>(-rhs._vec[1] * p + _vec[1]),
                                      static_cast<float>(-rhs._vec[2] * p + _vec[2]),
                                      _vec[3]);
    } else {
        double p = pow(k, ds);
        *this = PowerScaledCoordinate(static_cast<float>(-rhs._vec[0] + _vec[0] * p),
                                      static_cast<float>(-rhs._vec[1] + _vec[1] * p),
                                      static_cast<float>(-rhs._vec[2] + _vec[2] * p),
                                      rhs._vec[3]);
    }

    return *this;
}

PowerScaledCoordinate PowerScaledCoordinate::operator*(const double& rhs) const
{
    return PowerScaledCoordinate(static_cast<float>(_vec[0] * rhs),
                                 static_cast<float>(_vec[1] * rhs),
                                 static_cast<float>(_vec[2] * rhs), _vec[3]);
}

PowerScaledCoordinate PowerScaledCoordinate::operator*(const float& rhs) const
{
    return PowerScaledCoordinate(_vec[0] * rhs, _vec[1] * rhs, _vec[2] * rhs, _vec[3]);
}

PowerScaledCoordinate& PowerScaledCoordinate::operator*=(const PowerScaledScalar& rhs)
{
    double ds = this->_vec[3] - rhs._data[1];
    if (ds >= 0.0) {
        double p = pow(k, -ds);
        *this = PowerScaledCoordinate(
            static_cast<float>(rhs._data[0] * p * _vec[0]),
            static_cast<float>(rhs._data[0] * p * _vec[1]),
            static_cast<float>(rhs._data[0] * p * _vec[2]), this->_vec[3] + _vec[3]);
    } else {
        double p = pow(k, ds);
        *this = PowerScaledCoordinate(
            static_cast<float>(rhs._data[0] * _vec[0] * p),
            static_cast<float>(rhs._data[0] * _vec[1] * p),
            static_cast<float>(rhs._data[0] * _vec[2] * p), rhs._data[1] + rhs._data[1]);
    }
    return *this;
}

PowerScaledCoordinate PowerScaledCoordinate::operator*(const PowerScaledScalar& rhs) const
{
    return PowerScaledCoordinate(*this) *= rhs;
}

PowerScaledCoordinate PowerScaledCoordinate::operator*(const glm::mat4& matrix) const
{
    return matrix * _vec;
}

PowerScaledCoordinate PowerScaledCoordinate::
      operator-(const PowerScaledCoordinate& rhs) const
{
    return PowerScaledCoordinate(*this) -= rhs;
}

float& PowerScaledCoordinate::operator[](unsigned int idx)
{
    return _vec[idx];
}

float PowerScaledCoordinate::operator[](unsigned int idx) const
{
    return _vec[idx];
}

double PowerScaledCoordinate::dot(const PowerScaledCoordinate& rhs) const
{
    double ds = _vec[3] - rhs._vec[3];
    if (ds >= 0.0) {
        double p = pow(k, -ds);
        glm::dvec3 uPowerScaledCoordinatealed(rhs._vec[0] * p, rhs._vec[1] * p,
                                              rhs._vec[2] * p);
        glm::dvec3 shortened(_vec[0], _vec[1], _vec[2]);
        return glm::dot(uPowerScaledCoordinatealed, shortened);
    } else {
        double p = pow(k, ds);
        glm::dvec3 uPowerScaledCoordinatealed(_vec[0] * p, _vec[1] * p, _vec[2] * p);
        glm::dvec3 shortened(rhs._vec[0], rhs._vec[1], rhs._vec[2]);
        return glm::dot(uPowerScaledCoordinatealed, shortened);
    }
}

double PowerScaledCoordinate::angle(const PowerScaledCoordinate& rhs) const
{
    glm::dvec3 uPowerScaledCoordinatealed(rhs._vec[0], rhs._vec[1], rhs._vec[2]);
    glm::dvec3 shortened(_vec[0], _vec[1], _vec[2]);
    uPowerScaledCoordinatealed = glm::normalize(uPowerScaledCoordinatealed);
    shortened = glm::normalize(shortened);

    return acos(glm::dot(uPowerScaledCoordinatealed, shortened));
}

bool PowerScaledCoordinate::operator==(const PowerScaledCoordinate& other) const
{
    return _vec == other._vec;
}

bool PowerScaledCoordinate::operator!=(const PowerScaledCoordinate& other) const
{
    return _vec != other._vec;
}

bool PowerScaledCoordinate::operator<(const PowerScaledCoordinate& other) const
{
    double ds = _vec[3] - other._vec[3];
    if (ds >= 0) {
        double p = pow(k, -ds);
        glm::dvec3 upscaled(other._vec[0] * p, other._vec[1] * p,
                                              other._vec[2] * p);
        glm::dvec3 shortened(_vec[0], _vec[1], _vec[2]);
        return glm::length(shortened) < glm::length(upscaled);
    } else {
        double p = pow(k, ds);
        glm::dvec3 upscaled(_vec[0] * p, _vec[1] * p, _vec[2] * p);
        glm::dvec3 shortened(other._vec[0], other._vec[1], other._vec[2]);
        return glm::length(shortened) < glm::length(upscaled);
    }
}

bool PowerScaledCoordinate::operator>(const PowerScaledCoordinate& other) const
{
    double ds = this->_vec[3] - other._vec[3];
    if (ds >= 0) {
        double p = pow(k, -ds);
        glm::dvec3 upscaled(other._vec[0] * p, other._vec[1] * p,
                                              other._vec[2] * p);
        glm::dvec3 shortened(_vec[0], _vec[1], _vec[2]);
        return glm::length(shortened) > glm::length(upscaled);
    } else {
        double p = pow(k, ds);
        glm::dvec3 upscaled(_vec[0] * p, _vec[1] * p, _vec[2] * p);
        glm::dvec3 shortened(other._vec[0], other._vec[1], other._vec[2]);
        return glm::length(shortened) > glm::length(upscaled);
    }
}

bool PowerScaledCoordinate::operator<=(const PowerScaledCoordinate& other) const
{
    return *this < other || *this == other;
}

bool PowerScaledCoordinate::operator>=(const PowerScaledCoordinate& other) const
{
    return *this > other || *this == other;
}

std::ostream& operator<<(::std::ostream& os, const PowerScaledCoordinate& rhs)
{
    os << "(" << rhs[0] << ", " << rhs[1] << ", " << rhs[2] << "; " << rhs[3] << ")";
    return os;
}

}  // namespace openspace
