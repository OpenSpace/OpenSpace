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

#ifndef __POWERSCALEDSCALAR_H__
#define __POWERSCALEDSCALAR_H__

// glm includes
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>

#include <iostream>

namespace openspace {

class pss {
public:
	// constructors
	pss();
	pss(const glm::vec2 &v);
	pss(const glm::dvec2 &v);
	pss(const float &f1,const float &f2);
	pss(const double &d1,const double &d2);
	static pss CreatePSS(double d1);

	// print n' debug
	void print() const;
	void print(const char *name) const;

	// get functions
	const double * value_ptr();
	const float * value_ptrf();
	glm::dvec2 getVec2() const;
	glm::vec2 getVec2f() const;
	double length() const;
	float lengthf() const;

	// operator overloading
	pss & operator=(const pss &rhs);
	pss & operator+=(const pss &rhs);
	const pss operator+(const pss &rhs) const;
	pss & operator-=(const pss &rhs);
	const pss operator-(const pss &rhs) const;
	pss & operator*=(const pss &rhs);
	const pss operator*(const pss &rhs) const;
	pss & operator*=(const double &rhs);
	const pss operator*(const double &rhs) const;
	pss & operator*=(const float &rhs);
	const pss operator*(const float &rhs) const;
	double& operator[](unsigned int idx);
	const double& operator[](unsigned int idx) const;

	// comparasion
	bool operator==(const pss &other) const;
	bool operator<(const pss &other) const;
	bool operator>(const pss &other) const;
	bool operator<=(const pss &other) const;
	bool operator>=(const pss &other) const;
	
	bool operator==(const double &other) const;
	bool operator<(const double &other) const;
	bool operator>(const double &other) const;
	bool operator<=(const double &other) const;
	bool operator>=(const double &other) const;

	// glm integration
	pss & operator=(const glm::vec2 &rhs);
	pss & operator=(const float &rhs);
	pss & operator=(const glm::dvec2 &rhs);
	pss & operator=(const double &rhs);
    
    friend std::ostream& operator<<(::std::ostream& os, const pss& rhs);
	
	// allow the power scaled coordinates to acces private members
	friend class PowerScaledCoordinate;
private:

	// internal glm vector
	glm::dvec2 vec_;

	// float vector used when returning float values
	mutable glm::vec2 vecf_;

};


} // namespace openspace

#endif // __POWERSCALEDSCALAR_H__
