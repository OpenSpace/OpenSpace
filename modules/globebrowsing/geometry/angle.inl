/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <modules/globebrowsing/geometry/angle.h>




namespace openspace {


template <typename T>
const T Angle<T>::PI = 3.14159265358979323846264338327950;

template <typename T>
const T Angle<T>::EPSILON = 1e-10; // Should depend on the typedef /eb 



//////////////////////////////////////////////////////////////////////////////////////////
//                             STATIC CONSTANTS                                         //
//////////////////////////////////////////////////////////////////////////////////////////

template <typename T>
const Angle<T> Angle<T>::ZERO = Angle::fromRadians(0);

template <typename T>
const Angle<T> Angle<T>::QUARTER = Angle::fromRadians(PI/2);

template <typename T>
const Angle<T> Angle<T>::HALF = Angle::fromRadians(PI);

template <typename T>
const Angle<T> Angle<T>::FULL = Angle::fromRadians(2*PI);



//////////////////////////////////////////////////////////////////////////////////////////
//                                 Constructors                                         //
//////////////////////////////////////////////////////////////////////////////////////////

template <typename T> Angle<T>::Angle(const Angle<T>& other)
	: _radians(other._radians) { }

template <typename T> Angle<T>::Angle(T radians)
	: _radians(radians) { }




//////////////////////////////////////////////////////////////////////////////////////////
//                                 Factory Methods                                      //
//////////////////////////////////////////////////////////////////////////////////////////

template <typename T>
Angle<T> Angle<T>::fromRadians(T rads) {
	return Angle<T>(rads);
}

template <typename T>
Angle<T> Angle<T>::fromDegrees(T degrees) {
	return Angle<T>(degrees * PI / 180.0);
}

template <typename T>
Angle<T> Angle<T>::fromRevolutions(T revs) {
	return Angle<T>(revs * 2 * PI);
}



//////////////////////////////////////////////////////////////////////////////////////////
//                                     Conversions                                      //
//////////////////////////////////////////////////////////////////////////////////////////

template <typename T>
T Angle<T>::asRadians() const {
	return _radians;
}

template <typename T>
T Angle<T>::asDegrees() const {
	return _radians * 180.0 / PI;
}

template <typename T>
T Angle<T>::asRevolutions() const {
	return _radians / (2 * PI);
}


//////////////////////////////////////////////////////////////////////////////////////////
//                                     Operators                                        //
//////////////////////////////////////////////////////////////////////////////////////////


template <typename T>
Angle<T> Angle<T>::operator+(const Angle<T>& rhs) const{
	return Angle<T>(_radians + rhs._radians);
}

template <typename T>
Angle<T> Angle<T>::operator-(const Angle<T>& rhs) const{
	return Angle<T>(_radians - rhs._radians);
}

template <typename T>
Angle<T> Angle<T>::operator*(T multiplier) const{
	return Angle<T>(_radians * multiplier);
}

template <typename T>
Angle<T> Angle<T>::operator/(T divisor) const{
	return Angle<T>(_radians / divisor);
}


template <typename T>
Angle<T> Angle<T>::operator-() const {
	return Angle<T>(-_radians);
}


template <typename T>
void Angle<T>::operator+=(const Angle<T>& rhs){
	_radians += rhs._radians;
}

template <typename T>
void Angle<T>::operator-=(const Angle<T>& rhs){
	_radians -= rhs._radians;
}

template <typename T>
void Angle<T>::operator*=(T multiplier){
	_radians *= multiplier;
}

template <typename T>
void Angle<T>::operator/=(T divisor){
	_radians /= divisor;
}




template <typename T>
bool Angle<T>::operator<(const Angle<T>& rhs) const{
	return _radians < rhs._radians;
}

template <typename T>
bool Angle<T>::operator<=(const Angle<T>& rhs) const{
	return _radians <= rhs._radians;
}

template <typename T>
bool Angle<T>::operator>(const Angle<T>& rhs) const{
	return _radians > rhs._radians;
}

template <typename T>
bool Angle<T>::operator>=(const Angle<T>& rhs) const{
	return _radians >= rhs._radians;
}

template <typename T>
bool Angle<T>::operator==(const Angle<T>& rhs) const{
	return _radians == rhs._radians;
}

template <typename T>
bool Angle<T>::operator!=(const Angle<T>& rhs) const{
	return _radians != rhs._radians;
}



//////////////////////////////////////////////////////////////////////////////////////////
//                            Chainable Relative Mutators                               //
//////////////////////////////////////////////////////////////////////////////////////////

template <typename T>
Angle<T>& Angle<T>::normalize() {
	// this will cause _radians to be in value range ]-2pi, 2pi[
	_radians = fmod(_radians, 2*PI);

	// ensure _radians are positive, ie in value range [0, 2pi[
	if (_radians < 0.0){
		_radians += 2 * PI;
	}

	return *this;
}

template <typename T>
Angle<T>& Angle<T>::normalizeAround(const Angle<T>& center) {
	_radians -= center._radians + PI;
	normalize();
	_radians += center._radians - PI;
	return *this;
}

template <typename T>
Angle<T>& Angle<T>::clamp(const Angle<T>& min, const Angle<T>& max) {
	_radians = _radians < min._radians ? min._radians
		: _radians > max._radians ? max._radians
		: _radians;
	return *this;
}


template <typename T>
Angle<T>& Angle<T>::abs(){
	_radians = glm::abs(_radians);
	return *this;
}



//////////////////////////////////////////////////////////////////////////////////////////
//                         Chainable Relative Factory Methods                           //
//////////////////////////////////////////////////////////////////////////////////////////

template <typename T>
Angle<T> Angle<T>::getNormalized() const {
	return Angle<T>(*this).normalize();
}

template <typename T>
Angle<T> Angle<T>::getNormalizedAround(const Angle<T>& center) const {
	return Angle<T>(*this).normalizeAround(center);
}

template <typename T>
Angle<T> Angle<T>::getClamped(const Angle<T>& min, const Angle<T>& max) const {
	return Angle<T>(*this).clamp(min, max);
}



template <typename T>
Angle<T> Angle<T>::getAbs() const {
	return Angle<T>(*this).abs();
}

} // namespace openspace
