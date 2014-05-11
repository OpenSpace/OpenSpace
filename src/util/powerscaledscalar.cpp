
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


pss::pss() {

}

pss::pss(const glm::vec2 &v) {
	vec_ = glm::dvec2(v);
}

pss::pss(const glm::dvec2 &v) {
	vec_ = v;
}

pss::pss(const float &f1,const float &f2) {
	vec_ = glm::dvec2(f1, f2);
}

pss::pss(const double &d1,const double &d2) {
	vec_ = glm::dvec2(d1, d2);
}

pss pss::CreatePSS(double d1) {
	char buff[30];

	// find the number with maximum number of digits
	double ad1 = abs(d1);

	// find out how many digits
	sprintf ( buff, "%.0f", ad1);
	unsigned int digits = static_cast<unsigned int>(strlen(buff))-1;

	// rescale and return
	double tp = 1.0 / pow(k, digits);
	return pss(d1*tp, digits);
}

void pss::print() const {
	std::printf("[\n  %f\n  %f\n]\n",vec_[0], vec_[1]);
}
void pss::print(const char *name) const {
	std::printf("%s = [\n  %f\n  %f\n]\n", name, vec_[0], vec_[1]);
}

const double * pss::value_ptr() {
	return glm::value_ptr(vec_);
}

const float * pss::value_ptrf() {
	// need existing variable to return pointer to, local variables perish..
	vecf_ = glm::vec2(vec_);
	return glm::value_ptr(vecf_);
}

glm::dvec2  pss::getVec2() const{
	return vec_;
}

glm::vec2  pss::getVec2f() const {
	vecf_ = glm::vec2(vec_);
	return vecf_;
}

double pss::length() const {
	return vec_[0] * pow(k,vec_[1]);
}

float pss::lengthf() const {
	return static_cast<float>(vec_[0] * pow(k,vec_[1]));
}

pss& pss::operator=(const pss &rhs) {
	if (this != &rhs){
		this->vec_ = rhs.vec_;
	}
	return *this;  // Return a reference to myself.
}

pss & pss::operator=(const glm::vec2 &rhs) {
	this->vec_ = glm::dvec2(rhs);
	return *this;  // Return a reference to myself.
}

pss & pss::operator=(const float &rhs) {
	this->vec_ = glm::dvec2(rhs,0.0);
	return *this;  // Return a reference to myself.
}

pss & pss::operator=(const glm::dvec2 &rhs) {
	this->vec_ = rhs;
	return *this;  // Return a reference to myself.
}

pss & pss::operator=(const double&rhs) {
	this->vec_ = glm::dvec2(rhs,0.0);
	return *this;  // Return a reference to myself.
}

pss & pss::operator+=(const pss &rhs) {

	double ds = this->vec_[1] - rhs.vec_[1];
	if(ds >= 0) {
		*this = pss(rhs.vec_[0]*pow(k,-ds) + this->vec_[0], this->vec_[1]);
	} else {
		*this = pss(rhs.vec_[0] + this->vec_[0]*pow(k,ds), rhs.vec_[1]);
	}

	return *this;
}

const pss pss::operator+(const pss &rhs) const {
	return pss(*this) += rhs;
}

pss & pss::operator-=(const pss &rhs) {

	double ds = this->vec_[1] - rhs.vec_[1];
	if(ds >= 0) {
		*this = pss(-rhs.vec_[0]*pow(k,-ds) + this->vec_[0], this->vec_[1]);
	} else {
		*this = pss(-rhs.vec_[0] + this->vec_[0]*pow(k,ds), rhs.vec_[1]);
	}

	return *this;
}

const pss pss::operator-(const pss &rhs) const {
	return pss(*this) -= rhs;
}

pss & pss::operator*=(const pss &rhs) {
	double ds = this->vec_[1] - rhs.vec_[1];
	if(ds >= 0) {
		*this = pss(rhs.vec_[0]*pow(k,-ds) * this->vec_[0], this->vec_[1]+this->vec_[1]);
	} else {
		*this = pss(rhs.vec_[0] * this->vec_[0]*pow(k,ds), rhs.vec_[1]+rhs.vec_[1]);
	}

	return *this;
}


const pss pss::operator*(const pss &rhs) const {
	return pss(*this) *= rhs;
}

pss & pss::operator*=(const double &rhs) {
	double ds = this->vec_[1];
	if(ds >= 0) {
		*this = pss(rhs*pow(k,-ds) * this->vec_[0], this->vec_[1]);
	} else {
		*this = pss(rhs * this->vec_[0]*pow(k,ds), 0.0);
	}

	return *this;
}


const pss pss::operator*(const double &rhs) const {
	return pss(*this) *= rhs;
}

pss & pss::operator*=(const float &rhs) {
	double ds = this->vec_[1];
	if(ds >= 0) {
		*this = pss(rhs*pow(k,-ds) * this->vec_[0],this->vec_[1]+this->vec_[1]);
	} else {
		*this = pss(rhs * this->vec_[0]*pow(k,ds), 0.0);
	}

	return *this;
}

const pss pss::operator*(const float &rhs) const {
	return pss(*this) *= rhs;
}

double& pss::operator[](unsigned int idx) {
	return vec_[idx];
}
const double& pss::operator[](unsigned int idx) const {
	return vec_[idx];
}

bool pss::operator==(const pss &other) const {
	return vec_ == other.vec_;
}

bool pss::operator<(const pss &other) const {
	double ds = this->vec_[1] - other.vec_[1];
	if(ds >= 0) {
		double upscaled = other.vec_[0]*pow(k,-ds);
        return vec_[0] < upscaled;
        /*
        bool retur =(vec_[0] < upscaled);
        std::printf("this: %f, upscaled: %f, this<upscaled: %i\n", vec_[0], upscaled, retur);
		return retur;
        */
	} else {
		double upscaled = vec_[0]*pow(k,-ds);
		return other.vec_[0] > upscaled;
	}
}

bool pss::operator>(const pss &other) const {
	double ds = this->vec_[1] - other.vec_[1];
	if(ds >= 0) {
		double upscaled = other.vec_[0]*pow(k,-ds);
		return vec_[0] > upscaled;
	} else {
		double upscaled = vec_[0]*pow(k,-ds);
		return other.vec_[0] < upscaled;
	}
}

bool pss::operator<=(const pss &other) const {
	return *this < other || *this == other;
}

bool pss::operator>=(const pss &other) const {
	return *this > other || *this == other;
}

bool pss::operator==(const double &other) const {
	double ds = this->vec_[1];
	if(ds >= 0) {
		double upscaled = other*pow(k,-ds);
		return vec_[0] == upscaled;
	} else {
		double upscaled = vec_[0]*pow(k,-ds);
		return other == upscaled;
	}
}

bool pss::operator<(const double &other) const {
	double ds = this->vec_[1];
	if(ds >= 0) {
		double upscaled = other*pow(k,-ds);
		return vec_[0] < upscaled;
	} else {
		double upscaled = vec_[0]*pow(k,-ds);
		return other > upscaled;
	}
}

bool pss::operator>(const double &other) const {
	double ds = this->vec_[1];
	if(ds >= 0) {
		double upscaled = other*pow(k,-ds);
		return vec_[0] > upscaled;
	} else {
		double upscaled = vec_[0]*pow(k,-ds);
		return other < upscaled;
	}
}

bool pss::operator<=(const double &other) const {
	return *this < other || *this == other;
}

bool pss::operator>=(const double &other) const {
	return *this > other || *this == other;
}

std::ostream& operator<<(::std::ostream& os, const pss& rhs) {
    os << "(" << rhs[0] << ", " << rhs[1] << ")";
    return os;
}

} // namespace openspace
