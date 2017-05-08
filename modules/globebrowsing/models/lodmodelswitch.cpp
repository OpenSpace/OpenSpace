/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2017                                                               *
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

#include <modules/globebrowsing/models/lodmodelswitch.h>
#include <ghoul/logging/logmanager.h>


namespace {
	const std::string _loggerCat = "LodModelSwitch";
}

namespace openspace {
namespace globebrowsing {

	LodModelSwitch::LodModelSwitch(RenderableGlobe* owner) 
		: _owner(owner) 
	{}


LodModelSwitch::Mode LodModelSwitch::getLevel(const RenderData& data) {
	glm::dvec3 centerPosition = data.position.dvec3();

	glm::dvec3 cameraPosition = data.camera.positionVec3();
	glm::dmat4 inverseModelTransform = _owner->inverseModelTransform();
	glm::dvec3 cameraPositionModelSpace = glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1));

	glm::dvec3 centerToEllipsoidSurface = glm::dmat3(_owner->modelTransform())  * (_owner->projectOnEllipsoid(cameraPositionModelSpace));
	glm::dvec3 ellipsoidSurfaceToCamera = cameraPosition - (centerPosition + centerToEllipsoidSurface);

	double heightToSurface =
		_owner->getHeight(cameraPositionModelSpace);

	glm::dvec3 posDiff = cameraPosition - centerPosition;

	double distFromCenterToSurface =
		length(centerToEllipsoidSurface);
	double distFromEllipsoidSurfaceToCamera = length(ellipsoidSurfaceToCamera);
	double distFromCenterToCamera = length(posDiff);
	double distFromSurfaceToCamera =
		distFromEllipsoidSurfaceToCamera - heightToSurface;

	if (distFromSurfaceToCamera > 100000) {
		return Mode::High;
	}

	return Mode::Close;
}
}
}