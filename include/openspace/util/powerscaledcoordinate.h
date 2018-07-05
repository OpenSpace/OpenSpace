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

#ifndef __OPENSPACE_CORE___POWERSCALEDCOORDINATE___H__
#define __OPENSPACE_CORE___POWERSCALEDCOORDINATE___H__

#include <ghoul/glm.h>

namespace openspace {

class PowerScaledScalar;

class PowerScaledCoordinate {
public:
    // constructors
    PowerScaledCoordinate() = default;

    PowerScaledCoordinate(PowerScaledCoordinate&& rhs);
    PowerScaledCoordinate(const PowerScaledCoordinate& rhs);

    // Sets the power scaled coordinates directly
    PowerScaledCoordinate(glm::vec4 v);
    PowerScaledCoordinate(float f1, float f2, float f3, float f4);
    // Sets the power scaled coordinates with w = 0
    PowerScaledCoordinate(glm::vec3 v);

    static PowerScaledCoordinate CreatePowerScaledCoordinate(double d1, double d2,
        double d3);

    // get functions
    // return the full, unmodified PSC
    const glm::vec4& vec4() const;

    // returns the rescaled, "normal" coordinates
    glm::vec3 vec3() const;

    // return the full psc as dvec4()
     glm::dvec4 dvec4() const;

    // rescaled return as dvec3
    glm::dvec3 dvec3() const;

    // length of the vector as a pss
    PowerScaledScalar length() const;
    glm::vec3 direction() const;

    // operator overloading
    PowerScaledCoordinate& operator=(const PowerScaledCoordinate& rhs);
    PowerScaledCoordinate& operator=(PowerScaledCoordinate&& rhs);
    PowerScaledCoordinate& operator+=(const PowerScaledCoordinate& rhs);
    PowerScaledCoordinate operator+(const PowerScaledCoordinate& rhs) const;
    PowerScaledCoordinate& operator-=(const PowerScaledCoordinate& rhs);
    PowerScaledCoordinate operator-(const PowerScaledCoordinate& rhs) const;
    float& operator[](unsigned int idx);
    float operator[](unsigned int idx) const;
    double dot(const PowerScaledCoordinate& rhs) const;
    double angle(const PowerScaledCoordinate& rhs) const;

    // scalar operators
    PowerScaledCoordinate operator*(const double& rhs) const;
    PowerScaledCoordinate operator*(const float& rhs) const;
    PowerScaledCoordinate& operator*=(const PowerScaledScalar& rhs);
    PowerScaledCoordinate operator*(const PowerScaledScalar& rhs) const;
    PowerScaledCoordinate operator*(const glm::mat4& matrix) const;


    // comparison
    bool operator==(const PowerScaledCoordinate& other) const;
    bool operator!=(const PowerScaledCoordinate& other) const;
    bool operator<(const PowerScaledCoordinate& other) const;
    bool operator>(const PowerScaledCoordinate& other) const;
    bool operator<=(const PowerScaledCoordinate& other) const;
    bool operator>=(const PowerScaledCoordinate& other) const;

    // glm integration
    PowerScaledCoordinate& operator=(const glm::dvec4& rhs);
    PowerScaledCoordinate& operator=(const glm::vec4& rhs);
    PowerScaledCoordinate& operator=(const glm::dvec3& rhs);
    PowerScaledCoordinate& operator=(const glm::vec3& rhs);

    friend std::ostream& operator<<(std::ostream& os, const PowerScaledCoordinate& rhs);

    // allow the power scaled scalars to access private members
    friend class PowerScaledScalar;

private:
    // internal glm vector
    glm::vec4 _vec = glm::vec4(0.f);
};

typedef PowerScaledCoordinate psc;

} // namespace openspace

#endif // __OPENSPACE_CORE___POWERSCALEDCOORDINATE___H__
