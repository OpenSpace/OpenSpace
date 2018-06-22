/*****************************************************************************************
*                                                                                       *
* GHOUL                                                                                 *
* General Helpful Open Utility Library                                                  *
*                                                                                       *
* Copyright (c) 2012-2017                                                               *
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

#include <modules/marsrover/surfaceprojection/marsprojection.h>

#include <modules/globebrowsing/globebrowsingmodule.h>

#include <ghoul/misc/assert.h>

#include <ghoul/logging/logmanager.h>

namespace {
 const std::string _loggerCat = "MarsProjection";
}

namespace openspace {

 MarsProjection::MarsProjection(glm::dvec3 position, double Zvalue, int siteNr) : 
 	_position(position),
 	_height(Zvalue),
 	_site(siteNr)
	{

	//ghoul_assert(_dimensions.x >= 1, "Element of dimensions must be bigger or equal 1");
	//ghoul_assert(_dimensions.y >= 1, "Element of dimensions must be bigger or equal 1");
	//ghoul_assert(_dimensions.z >= 1, "Element of dimensions must be bigger or equal 1");

	 }

	void MarsProjection::readHeightmap() {

	}

	//put also positions for all points in array and send in
	void MarsProjection::rotateObject(int nrPoints) {
		//call 
		//getRotationMatrix for all points one by one after each rotation. 
	}

	double MarsProjection::getMedianHeight(double Zvalue) {
		return 1.0;
	}

	//test if a point is below or above surface. 
	bool MarsProjection::aboveSurface(double dvec3) {
		return false;
	}

	glm::dvec3 MarsProjection::fromCartesianToLongLat(glm::dvec3 xyz) {
		return glm::dvec3(1.0);
	}

	glm::dvec3 MarsProjection::fromLongLatToCartesian(glm::dvec3 longlat) {
		return glm::dvec3(1.0);
	}

	//distance is the distance between the current point and the rotation point. (ex back wheel) 
	glm::dmat3 MarsProjection::getRotationMatrix(double distance, glm::dvec3 point, double heightDiff) {

		double angle;
		glm::dmat3 transformMatrix = glm::dmat3(1.0);
		//  O wheel 1
		//  |\                     d = heightDiff
		//  |v\
		// d|  \ hypotenuse
		//	|   \
		//	|____\
		//  O  h  O 
		//
		/*
		double hypotenuse = sqrt(pow(distance, 2.0) + pow(heightDiff, 2.0));

		angle = sin-1(heightDiff / hypotenuse); 


		if (aboveSurface(glm::dvec3(1.0))) {
			angle = (-1) * angle;
			//negative direction for transform matrix 
		}

		double sin = glm::sin(angle);
		double cos = glm::cos(angle);

		//Fix: Correct rotation axis
		transformMatrix = glm::dmat3( cos, -sin, 1.0, 
									  sin,  cos, 1.0, 
									  1.0,  1.0, 1.0 );

		*/
		return transformMatrix;
	}


}
