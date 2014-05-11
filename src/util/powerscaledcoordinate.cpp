/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/powerscaledscalar.h>

// std includes
#include <cstdio>
#include <cstdlib>
#include <cmath>

namespace openspace {

namespace {
const double k = 10.0;
}

PowerScaledCoordinate::PowerScaledCoordinate()
{
}

PowerScaledCoordinate::PowerScaledCoordinate(glm::vec4 v)
{
    _vec = glm::dvec4(std::move(v));
}

PowerScaledCoordinate::PowerScaledCoordinate(glm::dvec4 v)
{
    _vec = std::move(v);
}

PowerScaledCoordinate::PowerScaledCoordinate(glm::vec3 v)
{
    _vec = glm::dvec4(v[0], v[1], v[2], 0.0);
}

PowerScaledCoordinate::PowerScaledCoordinate(glm::dvec3 v)
{
    _vec = glm::dvec4(v[0], v[1], v[2], 0.0);
}

PowerScaledCoordinate::PowerScaledCoordinate(float f1, float f2, float f3, float f4)
{
    _vec = glm::dvec4(f1, f2, f3, f4);
}

PowerScaledCoordinate::PowerScaledCoordinate(double d1, double d2, double d3, double d4)
{
    _vec = glm::dvec4(d1, d2, d3, d4);
}

PowerScaledCoordinate
      PowerScaledCoordinate::CreatePowerScaledCoordinate(double d1, double d2, double d3)
{
    char buff[30];

    // find the number with maximum number of digits
    double ad1 = abs(d1);
    double ad2 = abs(d2);
    double ad3 = abs(d3);
    double max = (ad2 > ad1) ? ad2 : (ad3 > ad1) ? ad3 : ad1;

    // find out how many digits
    sprintf(buff, "%.0f", max);
    unsigned int digits = static_cast<unsigned int>(strlen(buff)) - 1;

    // rescale and return
    double tp = 1.0 / pow(k, digits);
    return PowerScaledCoordinate(d1 * tp, d2 * tp, d3 * tp, digits);
}

const glm::dvec4& PowerScaledCoordinate::vec4() const
{
    return _vec;
}

glm::vec4 PowerScaledCoordinate::vec4f() const
{
    return  glm::vec4(_vec);
}

glm::dvec3 PowerScaledCoordinate::getVec3() const
{
    return glm::dvec3(_vec[0] * pow(k, _vec[3]), _vec[1] * pow(k, _vec[3]),
                      _vec[2] * pow(k, _vec[3]));
}

glm::vec3 PowerScaledCoordinate::getVec3f() const
{
    return glm::vec3(_vec[0] * pow(k, _vec[3]), _vec[1] * pow(k, _vec[3]),
                     _vec[2] * pow(k, _vec[3]));
}

pss PowerScaledCoordinate::length() const
{
    return pss(glm::length(glm::dvec3(_vec[0], _vec[1], _vec[2])), _vec[3]);
}

glm::dvec3 PowerScaledCoordinate::getDirection() const
{
    if (_vec[0] == 0.0 && _vec[1] == 0.0 && _vec[2] == 0.0)
        return glm::dvec3(0.0, 0.0, 1.0);
    glm::dvec3 tmp(_vec[0], _vec[1], _vec[2]);
    return glm::normalize(tmp);
}

glm::vec3 PowerScaledCoordinate::getDirectionf() const
{
    glm::vec3 tmp(_vec[0], _vec[1], _vec[2]);
    return glm::normalize(tmp);
}

PowerScaledCoordinate PowerScaledCoordinate::mul(const glm::mat4& m) const
{
    return mul(glm::dmat4(m));
}
PowerScaledCoordinate PowerScaledCoordinate::mul(const glm::dmat4& m) const
{
    glm::dvec4 tmp = m * _vec;
    return PowerScaledCoordinate(tmp[0], tmp[1], tmp[2], _vec[3]);
}

PowerScaledCoordinate& PowerScaledCoordinate::operator=(const PowerScaledCoordinate& rhs)
{
    if (this != &rhs) {
        this->_vec = rhs._vec;
    }
    return *this;  // Return a reference to myself.
}

PowerScaledCoordinate& PowerScaledCoordinate::operator=(const glm::vec4& rhs)
{
    this->_vec = glm::dvec4(rhs);
    return *this;  // Return a reference to myself.
}

PowerScaledCoordinate& PowerScaledCoordinate::operator=(const glm::vec3& rhs)
{
    this->_vec = glm::dvec4(rhs[0], rhs[1], rhs[2], 0.0);
    return *this;  // Return a reference to myself.
}

PowerScaledCoordinate& PowerScaledCoordinate::operator=(const glm::dvec4& rhs)
{
    this->_vec = rhs;
    return *this;  // Return a reference to myself.
}

PowerScaledCoordinate& PowerScaledCoordinate::operator=(const glm::dvec3& rhs)
{
    this->_vec = glm::dvec4(rhs[0], rhs[1], rhs[2], 0.0);
    return *this;  // Return a reference to myself.
}

PowerScaledCoordinate& PowerScaledCoordinate::operator+=(const PowerScaledCoordinate& rhs)
{
    double ds = this->_vec[3] - rhs._vec[3];
    if (ds >= 0) {
        double p = pow(k, -ds);
        *this = PowerScaledCoordinate(rhs._vec[0] * p + this->_vec[0],
                                      rhs._vec[1] * p + this->_vec[1],
                                      rhs._vec[2] * p + this->_vec[2], this->_vec[3]);
    } else {
        double p = pow(k, ds);
        *this = PowerScaledCoordinate(rhs._vec[0] + this->_vec[0] * p,
                                      rhs._vec[1] + this->_vec[1] * p,
                                      rhs._vec[2] + this->_vec[2] * p, rhs._vec[3]);
    }

    return *this;
}

const PowerScaledCoordinate PowerScaledCoordinate::
      operator+(const PowerScaledCoordinate& rhs) const
{
    return PowerScaledCoordinate(*this) += rhs;
}

PowerScaledCoordinate& PowerScaledCoordinate::operator-=(const PowerScaledCoordinate& rhs)
{
    double ds = this->_vec[3] - rhs._vec[3];
    if (ds >= 0) {
        double p = pow(k, -ds);
        *this = PowerScaledCoordinate(-rhs._vec[0] * p + this->_vec[0],
                                      -rhs._vec[1] * p + this->_vec[1],
                                      -rhs._vec[2] * p + this->_vec[2], this->_vec[3]);
    } else {
        double p = pow(k, ds);
        *this = PowerScaledCoordinate(-rhs._vec[0] + this->_vec[0] * p,
                                      -rhs._vec[1] + this->_vec[1] * p,
                                      -rhs._vec[2] + this->_vec[2] * p, rhs._vec[3]);
    }

    return *this;
}

const PowerScaledCoordinate PowerScaledCoordinate::operator*(const double& rhs) const
{
    return PowerScaledCoordinate(_vec[0] * rhs, _vec[1] * rhs, _vec[2] * rhs, _vec[3]);
}

const PowerScaledCoordinate PowerScaledCoordinate::operator*(const float& rhs) const
{
    return PowerScaledCoordinate(_vec[0] * rhs, _vec[1] * rhs, _vec[2] * rhs, _vec[3]);
}

PowerScaledCoordinate& PowerScaledCoordinate::operator*=(const pss& rhs)
{
    double ds = this->_vec[3] - rhs.vec_[1];
    if (ds >= 0) {
        double p = pow(k, -ds);
        *this = PowerScaledCoordinate(
              rhs.vec_[0] * p * this->_vec[0], rhs.vec_[0] * p * this->_vec[1],
              rhs.vec_[0] * p * this->_vec[2], this->_vec[3] + this->_vec[3]);
    } else {
        double p = pow(k, ds);
        *this = PowerScaledCoordinate(
              rhs.vec_[0] * this->_vec[0] * p, rhs.vec_[0] * this->_vec[1] * p,
              rhs.vec_[0] * this->_vec[2] * p, rhs.vec_[1] + rhs.vec_[1]);
    }
    return *this;
}

const PowerScaledCoordinate PowerScaledCoordinate::operator*(const pss& rhs) const
{
    return PowerScaledCoordinate(*this) *= rhs;
}

const PowerScaledCoordinate PowerScaledCoordinate::
      operator-(const PowerScaledCoordinate& rhs) const
{
    return PowerScaledCoordinate(*this) -= rhs;
}

double& PowerScaledCoordinate::operator[](unsigned int idx)
{
    return _vec[idx];
}
const double& PowerScaledCoordinate::operator[](unsigned int idx) const
{
    return _vec[idx];
}

const double PowerScaledCoordinate::dot(const PowerScaledCoordinate& rhs) const
{
    double ds = this->_vec[3] - rhs._vec[3];
    if (ds >= 0) {
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

const double PowerScaledCoordinate::angle(const PowerScaledCoordinate& rhs) const
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
    double ds = this->_vec[3] - other._vec[3];
    if (ds >= 0) {
        double p = pow(k, -ds);
        glm::dvec3 uPowerScaledCoordinatealed(other._vec[0] * p, other._vec[1] * p,
                                              other._vec[2] * p);
        glm::dvec3 shortened(_vec[0], _vec[1], _vec[2]);
        return glm::length(shortened) < glm::length(uPowerScaledCoordinatealed);
    } else {
        double p = pow(k, ds);
        glm::dvec3 uPowerScaledCoordinatealed(_vec[0] * p, _vec[1] * p, _vec[2] * p);
        glm::dvec3 shortened(other._vec[0], other._vec[1], other._vec[2]);
        return glm::length(shortened) < glm::length(uPowerScaledCoordinatealed);
    }
}

bool PowerScaledCoordinate::operator>(const PowerScaledCoordinate& other) const
{
    double ds = this->_vec[3] - other._vec[3];
    if (ds >= 0) {
        double p = pow(k, -ds);
        glm::dvec3 uPowerScaledCoordinatealed(other._vec[0] * p, other._vec[1] * p,
                                              other._vec[2] * p);
        glm::dvec3 shortened(_vec[0], _vec[1], _vec[2]);
        return glm::length(shortened) > glm::length(uPowerScaledCoordinatealed);
    } else {
        double p = pow(k, ds);
        glm::dvec3 uPowerScaledCoordinatealed(_vec[0] * p, _vec[1] * p, _vec[2] * p);
        glm::dvec3 shortened(other._vec[0], other._vec[1], other._vec[2]);
        return glm::length(shortened) > glm::length(uPowerScaledCoordinatealed);
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
    os << "(" << rhs[0] << ", " << rhs[1] << ", " << rhs[2] << ", " << rhs[3] << ")";
    return os;
}

}  // namespace openspace
