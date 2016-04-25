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

#ifndef __ANGLE_H__
#define __ANGLE_H__

#include <glm/glm.hpp>
#include <memory>
#include <math.h>

namespace openspace {


template <typename T>
class Angle {
public:

	//////////////////////////////////////////////////////////////////////////////////////
	//                             STATIC CONSTANTS                                     //
	//////////////////////////////////////////////////////////////////////////////////////

	static const T PI;
	static const T EPSILON;

	
	/** = 0 radians = 0 degrees = no revolution */
	static const Angle<T> ZERO;

	/** = PI/2 radians = 90 degrees = quarter of a revolution */
	static const Angle<T> QUARTER;

	/** = PI radians = 180 degrees = half a revolution */
	static const Angle<T> HALF;

	/** = 2PI radians = 360 degrees = a full revolution */
	static const Angle<T> FULL;




	//////////////////////////////////////////////////////////////////////////////////////
	//                                 Factory Methods                                  //
	//////////////////////////////////////////////////////////////////////////////////////

	static Angle<T> fromRadians(T radians);
	static Angle<T> fromDegrees(T degrees);
	static Angle<T> fromRevolutions(T revs);


public:
	/** Copy constructor */
	Angle<T>(const Angle<T>& other);

private:
	/** Private constructor. Use factory methods to avoid unit confusion */
	Angle<T>(T rad);


	//////////////////////////////////////////////////////////////////////////////////////
	//                                     Conversions                                  //
	//////////////////////////////////////////////////////////////////////////////////////
public:
	inline T asRadians() const;
	inline T asDegrees() const;
	inline T asRevolutions() const;


	//////////////////////////////////////////////////////////////////////////////////////
	//                          Operators (boilerplate, I know.. /eb)                   //
	//////////////////////////////////////////////////////////////////////////////////////

	Angle<T> operator+(const Angle<T>& rhs) const;
	Angle<T> operator-(const Angle<T>& rhs) const;
	Angle<T> operator*(T rhs) const;
	Angle<T> operator/(T rhs) const;

	Angle<T> operator-() const;

	void operator+=(const Angle<T>& rhs);
	void operator-=(const Angle<T>& rhs);
	void operator*=(T rhs);
	void operator/=(T rhs);

	bool operator<(const Angle<T>& rhs) const;
	bool operator<=(const Angle<T>& rhs) const;
	bool operator>(const Angle<T>& rhs) const;
	bool operator>=(const Angle<T>& rhs) const;
	bool operator==(const Angle<T>& rhs) const;
	bool operator!=(const Angle<T>& rhs) const;





	//////////////////////////////////////////////////////////////////////////////////////
	//                            Chainable Relative Mutators                           //
	//////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Normalizes the angle to the interval [0, 2pi[
	 */
	Angle<T>& normalize();

	/**
	* Normalizes the angle to the interval [center - pi, center + pi[
	*/
	Angle<T>& normalizeAround(const Angle<T>& center);

	/**
	* Clamps the angle to the interval [min, max]. 
	* Default arguments are [0, 2pi]. 
	*/
	Angle<T>& clamp(const Angle<T>& min = ZERO, const Angle<T>& max = FULL);


	Angle<T>& abs();




	//////////////////////////////////////////////////////////////////////////////////////
	//                         Chainable Relative Factory Methods                       //
	//////////////////////////////////////////////////////////////////////////////////////

	/**
	* Returns a new angle normalized to the interval [0, 2pi[
	*/
	Angle<T> getNormalized() const;

	/**
	* Returns a new angle normalized to the interval [center - pi, center + pi[
	*/
	Angle<T> getNormalizedAround(const Angle<T>& center) const;

	/**
	* Returns a new angle clamped to the interval [min, max].
	* Default arguments are [0, 2pi].
	*/
	Angle<T> getClamped(const Angle<T>& min = ZERO, const Angle<T>& max = FULL) const;


	Angle<T> getAbs() const;
	


private:

	T _radians;

};

using dAngle = Angle<double>;
using fAngle = Angle<float>;

} // namespace openspace



#include <modules/globebrowsing/geodetics/angle.inl>





#endif // __ANGLE_H__
