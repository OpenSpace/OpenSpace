#ifndef PSC_H
#define PSC_H

// open space includes
#include "Object.h"

// glm includes
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>

namespace openspace
{

#define k 10.0

// forward declare the power scaled scalars
class pss;

class psc: public Object {
public:

	// constructors
	psc();
	psc(const glm::vec4 &v);
	psc(const glm::dvec4 &v);
	psc(const glm::vec3 &v);
	psc(const glm::dvec3 &v);
	psc(const float &f1,const float &f2,const float &f3,const float &f4);
	psc(const double &d1,const double &d2,const double &d3,const double &d4);

	static psc CreatePSC(double d1, double d2, double d3);

	// print n' debug
	void print() const;
	void print(const char *name) const;

	// get functions
	const double * value_ptr();
	const float * value_ptrf();
	glm::dvec4 getVec4() const;
	glm::vec4 getVec4f();
	glm::dvec3 getVec3() const;
	glm::vec3 getVec3f();
	pss length() const;
	glm::dvec3 getDirection() const;
	glm::vec3 getDirectionf() const;

	// multiplication
	psc mul(const glm::mat4 &m) const;
	psc mul(const glm::dmat4 &m) const;

	// operator overloading
	psc & operator=(const psc &rhs);
	psc & operator+=(const psc &rhs);
	const psc operator+(const psc &rhs) const;
	psc & operator-=(const psc &rhs);
	const psc operator-(const psc &rhs) const;
	double& operator[](unsigned int idx);
	const double& operator[](unsigned int idx) const;
	const double dot(const psc &rhs) const;
	const double angle(const psc &rhs) const;

	// scalar operators
	const psc operator*(const double &rhs) const;
	const psc operator*(const float &rhs) const;
	psc &operator*=(const pss &rhs);
	const psc operator*(const pss &rhs) const;

	// comparasion
	 bool operator==(const psc &other) const;
	 bool operator<(const psc &other) const;
	 bool operator>(const psc &other) const;
	 bool operator<=(const psc &other) const;
	 bool operator>=(const psc &other) const;

	// glm integration
	psc & operator=(const glm::vec4 &rhs);
	psc & operator=(const glm::vec3 &rhs);
	psc & operator=(const glm::dvec4 &rhs);
	psc & operator=(const glm::dvec3 &rhs);
	
	// allow the power scaled scalars to acces private members
	friend class pss;
private:

	// internal glm vector
	glm::dvec4 vec_;

	// float vector used when returning float values
	glm::vec4 vecf_;

};


} // namespace openspace

#endif