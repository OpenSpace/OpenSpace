#ifndef PSS_H
#define PSS_H

// glm includes
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>

namespace openspace
{

// forward declare the power scaled coordinates
class psc;

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
	glm::vec2 getVec2f();
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
	
	// allow the power scaled coordinates to acces private members
	friend class psc;
private:

	// internal glm vector
	glm::dvec2 vec_;

	// float vector used when returning float values
	glm::vec2 vecf_;

};


} // namespace openspace

#endif