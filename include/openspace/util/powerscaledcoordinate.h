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

#ifndef __POWERSCALEDCOORDINATE_H__
#define __POWERSCALEDCOORDINATE_H__

// open space includes
// glm includes
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <iostream>

namespace openspace
{

// forward declare the power scaled scalars
class pss;

class PowerScaledCoordinate {
public:
    // constructors
    PowerScaledCoordinate();

    // Sets the power scaled coordinates directly
    PowerScaledCoordinate(glm::vec4 v);
    PowerScaledCoordinate(glm::dvec4 v);
    PowerScaledCoordinate(float f1, float f2, float f3, float f4);
    PowerScaledCoordinate(double d1, double d2, double d3, double d4);
    // Sets the power scaled coordinates with w = 0
    PowerScaledCoordinate(glm::vec3 v);
    PowerScaledCoordinate(glm::dvec3 v);

    static PowerScaledCoordinate CreatePowerScaledCoordinate(double d1, double d2, double d3);

    // get functions
    // return the full, unmodified PSC 
    const glm::dvec4& vec4() const;
    glm::vec4 vec4f() const;

    glm::dvec3 getVec3() const;
    glm::vec3 getVec3f() const;
    pss length() const;
    glm::dvec3 getDirection() const;
    glm::vec3 getDirectionf() const;

    // multiplication
    PowerScaledCoordinate mul(const glm::mat4& m) const;
    PowerScaledCoordinate mul(const glm::dmat4& m) const;

    // operator overloading
    PowerScaledCoordinate& operator=(const PowerScaledCoordinate& rhs);
    PowerScaledCoordinate& operator+=(const PowerScaledCoordinate& rhs);
    const PowerScaledCoordinate operator+(const PowerScaledCoordinate& rhs) const;
    PowerScaledCoordinate& operator-=(const PowerScaledCoordinate& rhs);
    const PowerScaledCoordinate operator-(const PowerScaledCoordinate& rhs) const;
    double& operator[](unsigned int idx);
    const double& operator[](unsigned int idx) const;
    const double dot(const PowerScaledCoordinate& rhs) const;
    const double angle(const PowerScaledCoordinate& rhs) const;

    // scalar operators
    const PowerScaledCoordinate operator*(const double& rhs) const;
    const PowerScaledCoordinate operator*(const float& rhs) const;
    PowerScaledCoordinate& operator*=(const pss& rhs);
    const PowerScaledCoordinate operator*(const pss& rhs) const;

    // comparasion
    bool operator==(const PowerScaledCoordinate& other) const;
    bool operator!=(const PowerScaledCoordinate& other) const;
    bool operator<(const PowerScaledCoordinate& other) const;
    bool operator>(const PowerScaledCoordinate& other) const;
    bool operator<=(const PowerScaledCoordinate& other) const;
    bool operator>=(const PowerScaledCoordinate& other) const;

    // glm integration
    PowerScaledCoordinate& operator=(const glm::vec4& rhs);
    PowerScaledCoordinate& operator=(const glm::vec3& rhs);
    PowerScaledCoordinate& operator=(const glm::dvec4& rhs);
    PowerScaledCoordinate& operator=(const glm::dvec3& rhs);

    friend std::ostream& operator<<(std::ostream& os, const PowerScaledCoordinate& rhs);

    // allow the power scaled scalars to access private members
    friend class pss;

private:
    // internal glm vector
    glm::dvec4 _vec;
};

typedef PowerScaledCoordinate psc;
//typedef PowerScaledCoordinate psc;

} // namespace openspace

#endif // __POWERSCALEDCOORDINATE_H__
