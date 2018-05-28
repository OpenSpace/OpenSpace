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

#ifndef __OPENSPACE_CORE___POWERSCALEDSCALAR___H__
#define __OPENSPACE_CORE___POWERSCALEDSCALAR___H__

#include <ghoul/glm.h>

namespace openspace {

class PowerScaledScalar {
public:
    PowerScaledScalar() = default;
    PowerScaledScalar(const glm::vec2 &v);
    PowerScaledScalar(float f1, float f2);
    static PowerScaledScalar CreatePSS(double d1);

    const glm::vec2& vec2() const;
    float lengthf() const;
    double lengthd() const;


    // operator overloading
    PowerScaledScalar& operator=(const PowerScaledScalar& rhs);
    PowerScaledScalar& operator+=(const PowerScaledScalar& rhs);
    const PowerScaledScalar& operator+(const PowerScaledScalar& rhs) const;
    PowerScaledScalar& operator-=(const PowerScaledScalar& rhs);
    const PowerScaledScalar& operator-(const PowerScaledScalar& rhs) const;
    PowerScaledScalar& operator*=(const PowerScaledScalar& rhs);
    const PowerScaledScalar& operator*(const PowerScaledScalar& rhs) const;
    PowerScaledScalar& operator*=(float rhs);
    const PowerScaledScalar& operator*(float rhs) const;
    float& operator[](unsigned int idx);
    float operator[](unsigned int idx) const;

    // comparison
    bool operator==(const PowerScaledScalar& other) const;
    bool operator<(const PowerScaledScalar& other) const;
    bool operator>(const PowerScaledScalar& other) const;
    bool operator<=(const PowerScaledScalar& other) const;
    bool operator>=(const PowerScaledScalar& other) const;

    bool operator==(double other) const;
    bool operator<(double other) const;
    bool operator>(double other) const;
    bool operator<=(double other) const;
    bool operator>=(double other) const;

    // glm integration
    PowerScaledScalar& operator=(const glm::vec2& rhs);
    PowerScaledScalar& operator=(float rhs);

    friend std::ostream& operator<<(std::ostream& os, const PowerScaledScalar& rhs);

    // allow the power scaled coordinates to access private members
    friend class PowerScaledCoordinate;

private:
    // float vector used when returning float values
    glm::vec2 _data = glm::vec2(0.f);
};

typedef PowerScaledScalar pss;

} // namespace openspace

#endif // __OPENSPACE_CORE___POWERSCALEDSCALAR___H__
