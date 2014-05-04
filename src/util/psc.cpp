
// open space includes
#include <openspace/util/psc.h>
#include <openspace/util/pss.h>

// std includes
#include <cstdio>
#include <cstdlib>
#include <cmath>

namespace openspace {

namespace {
    const double k = 10.0;
}

psc::psc() {

}

psc::psc(const glm::vec4 &v) {
	vec_ = glm::dvec4(v);
}

psc::psc(const glm::dvec4 &v) {
	vec_ = v;
}

psc::psc(const glm::vec3 &v) {
	vec_ = glm::dvec4(v[0],v[1],v[2],0.0);
}

psc::psc(const glm::dvec3 &v) {
	vec_ = glm::dvec4(v[0],v[1],v[2],0.0);
}

psc::psc(const float &f1,const float &f2,const float &f3,const float &f4) {
	vec_ = glm::dvec4(f1, f2, f3, f4);
}

psc::psc(const double &d1,const double &d2,const double &d3,const double &d4) {
	vec_ = glm::dvec4(d1, d2, d3, d4);
}

psc psc::CreatePSC(double d1, double d2, double d3) {
	char buff[30];

	// find the number with maximum number of digits
	double ad1 = abs(d1);
	double ad2 = abs(d2);
	double ad3 = abs(d3);
	double max = (ad2 > ad1) ? ad2 : (ad3 > ad1) ? ad3 : ad1;

	// find out how many digits
	sprintf ( buff, "%.0f", max);
	unsigned int digits = static_cast<unsigned int>(strlen(buff))-1;

	// rescale and return
	double tp = 1.0 / pow(k, digits);
	return psc(d1*tp, d2*tp, d3*tp, digits);
}

void psc::print() const {
	std::printf("[\n  %f\n  %f\n  %f\n  %f\n]\n",vec_[0], vec_[1], vec_[2], vec_[3]);
}
void psc::print(const char *name) const {
	std::printf("%s = [\n  %.15f\n  %.15f\n  %.15f\n  %.15f\n]\n", name, vec_[0], vec_[1], vec_[2], vec_[3]);
}

const double * psc::value_ptr() {
	return glm::value_ptr(vec_);
}

const float * psc::value_ptrf() {
	// need existing variable to return pointer to, local variables perish..
	vecf_ = glm::vec4(vec_);
	return glm::value_ptr(vecf_);
}

glm::dvec4  psc::getVec4() const{
	return vec_;
}

glm::vec4  psc::getVec4f() const {
	vecf_ = glm::vec4(vec_);
	return vecf_;
}

glm::dvec3 psc::getVec3() const {
	return glm::dvec3(vec_[0]*pow(k,vec_[3]),vec_[1]*pow(k,vec_[3]),vec_[2]*pow(k,vec_[3]));
}

glm::vec3 psc::getVec3f() const {
	return glm::vec3(vec_[0]*pow(k,vec_[3]),vec_[1]*pow(k,vec_[3]),vec_[2]*pow(k,vec_[3]));
}

pss psc::length() const {
	return pss(glm::length(glm::dvec3(vec_[0],vec_[1],vec_[2])),vec_[3]);
}

glm::dvec3 psc::getDirection() const{
    if (vec_[0] == 0.0 && vec_[1] == 0.0 && vec_[2] == 0.0)
        return glm::dvec3(0.0, 0.0, 1.0);
	glm::dvec3 tmp(vec_[0],vec_[1],vec_[2]);
	return glm::normalize(tmp);
}

glm::vec3 psc::getDirectionf() const {
	glm::vec3 tmp(vec_[0],vec_[1],vec_[2]);
	return glm::normalize(tmp);
}


psc psc::mul(const glm::mat4 &m) const {
	return mul(glm::dmat4(m));
}
psc psc::mul(const glm::dmat4 &m) const {
	glm::dvec4 tmp = m * vec_;
	return psc(tmp[0],tmp[1],tmp[2],vec_[3]);
}

psc& psc::operator=(const psc &rhs) {
	if (this != &rhs){
		this->vec_ = rhs.vec_;
	}
	return *this;  // Return a reference to myself.
}

psc & psc::operator=(const glm::vec4 &rhs) {
	this->vec_ = glm::dvec4(rhs);
	return *this;  // Return a reference to myself.
}

psc & psc::operator=(const glm::vec3 &rhs) {
	this->vec_ = glm::dvec4(rhs[0],rhs[1],rhs[2],0.0);
	return *this;  // Return a reference to myself.
}

psc & psc::operator=(const glm::dvec4 &rhs) {
	this->vec_ = rhs;
	return *this;  // Return a reference to myself.
}

psc & psc::operator=(const glm::dvec3 &rhs) {
	this->vec_ = glm::dvec4(rhs[0],rhs[1],rhs[2],0.0);
	return *this;  // Return a reference to myself.
}

psc & psc::operator+=(const psc &rhs) {

	double ds = this->vec_[3] - rhs.vec_[3];
	if(ds >= 0) {
		double p = pow(k,-ds);
		*this = psc(rhs.vec_[0]*p + this->vec_[0], rhs.vec_[1]*p + this->vec_[1], rhs.vec_[2]*p + this->vec_[2], this->vec_[3]);
	} else {
		double p = pow(k,ds);
		*this = psc(rhs.vec_[0] + this->vec_[0]*p, rhs.vec_[1] + this->vec_[1]*p, rhs.vec_[2] + this->vec_[2]*p, rhs.vec_[3]);
	}

	return *this;
}

const psc psc::operator+(const psc &rhs) const {
	return psc(*this) += rhs;
}

psc & psc::operator-=(const psc &rhs) {

	double ds = this->vec_[3] - rhs.vec_[3];
	if(ds >= 0) {
		double p = pow(k,-ds);
		*this = psc(-rhs.vec_[0]*p + this->vec_[0], -rhs.vec_[1]*p + this->vec_[1], -rhs.vec_[2]*p + this->vec_[2], this->vec_[3]);
	} else {
		double p = pow(k,ds);
		*this = psc(-rhs.vec_[0] + this->vec_[0]*p, -rhs.vec_[1] + this->vec_[1]*p, -rhs.vec_[2] + this->vec_[2]*p, rhs.vec_[3]);
	}

	return *this;
}

const psc psc::operator*(const double &rhs) const {
	return psc(vec_[0]*rhs,vec_[1]*rhs,vec_[2]*rhs,vec_[3]);
}

const psc psc::operator*(const float &rhs) const {
	return psc(vec_[0]*rhs,vec_[1]*rhs,vec_[2]*rhs,vec_[3]);
}

psc& psc::operator*=(const pss &rhs) {
	double ds = this->vec_[3] - rhs.vec_[1];
	if(ds >= 0) {
		double p = pow(k,-ds);
		*this = psc(rhs.vec_[0]*p * this->vec_[0], rhs.vec_[0]*p * this->vec_[1], rhs.vec_[0]*p * this->vec_[2], this->vec_[3] +this->vec_[3]);
	} else {
		double p = pow(k,ds);
		*this = psc(rhs.vec_[0] * this->vec_[0]*p, rhs.vec_[0] * this->vec_[1]*p, rhs.vec_[0] * this->vec_[2]*p, rhs.vec_[1]+ rhs.vec_[1]);
	}
	return *this;
}

const psc psc::operator*(const pss &rhs) const {
	return psc(*this) *= rhs;
}

const psc psc::operator-(const psc &rhs) const {
	return psc(*this) -= rhs;
}

double& psc::operator[](unsigned int idx) {
	return vec_[idx];
}
const double& psc::operator[](unsigned int idx) const {
	return vec_[idx];
}

const double psc::dot(const psc &rhs) const {
	double ds = this->vec_[3] - rhs.vec_[3];
	if(ds >= 0) {
		double p = pow(k,-ds);
		glm::dvec3 upscaled(rhs.vec_[0]*p,rhs.vec_[1]*p,rhs.vec_[2]*p);
		glm::dvec3 shortened(vec_[0],vec_[1],vec_[2]);
		return glm::dot(upscaled,shortened);
	} else {
		double p = pow(k,ds);
		glm::dvec3 upscaled(vec_[0]*p,vec_[1]*p,vec_[2]*p);
		glm::dvec3 shortened(rhs.vec_[0],rhs.vec_[1],rhs.vec_[2]);
		return glm::dot(upscaled,shortened);
	}
}

const double psc::angle(const psc &rhs) const {
	glm::dvec3 upscaled(rhs.vec_[0],rhs.vec_[1],rhs.vec_[2]);
	glm::dvec3 shortened(vec_[0],vec_[1],vec_[2]);
	upscaled = glm::normalize(upscaled);
	shortened = glm::normalize(shortened);

	return acos(glm::dot(upscaled,shortened));
}

bool psc::operator==(const psc &other) const {
	return vec_ == other.vec_;
}
    
bool psc::operator!=(const psc &other) const {
    return vec_ != other.vec_;
}

bool psc::operator<(const psc &other) const {
	double ds = this->vec_[3] - other.vec_[3];
	if(ds >= 0) {
		double p = pow(k,-ds);
		glm::dvec3 upscaled(other.vec_[0]*p,other.vec_[1]*p,other.vec_[2]*p);
		glm::dvec3 shortened(vec_[0],vec_[1],vec_[2]);
		return glm::length(shortened) < glm::length(upscaled);
	} else {
		double p = pow(k,ds);
		glm::dvec3 upscaled(vec_[0]*p,vec_[1]*p,vec_[2]*p);
		glm::dvec3 shortened(other.vec_[0],other.vec_[1],other.vec_[2]);
		return glm::length(shortened) < glm::length(upscaled);
	}
}

bool psc::operator>(const psc &other) const {
	double ds = this->vec_[3] - other.vec_[3];
	if(ds >= 0) {
		double p = pow(k,-ds);
		glm::dvec3 upscaled(other.vec_[0]*p,other.vec_[1]*p,other.vec_[2]*p);
		glm::dvec3 shortened(vec_[0],vec_[1],vec_[2]);
		return glm::length(shortened) > glm::length(upscaled);
	} else {
		double p = pow(k,ds);
		glm::dvec3 upscaled(vec_[0]*p,vec_[1]*p,vec_[2]*p);
		glm::dvec3 shortened(other.vec_[0],other.vec_[1],other.vec_[2]);
		return glm::length(shortened) > glm::length(upscaled);
	}
}

bool psc::operator<=(const psc &other) const {
	return *this < other || *this == other;
}

bool psc::operator>=(const psc &other) const {
	return *this > other || *this == other;
}

    
std::ostream& operator<<(::std::ostream& os, const psc& rhs) {
    os << "(" << rhs[0] << ", " << rhs[1] << ", " << rhs[2] << ", " << rhs[3] << ")";
    return os;
}


} // namespace openspace
