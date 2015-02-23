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

#include <openspace/rendering/planets/simplespheregeometryprojection.h>
#include <openspace/util/constants.h>

namespace {
    const std::string _loggerCat = "SimpleSphereGeometryProjection";
}

namespace openspace {

namespace constants {
namespace simplespheregeometryprojection {
const std::string keyRadius = "Radius";
const std::string keySegments = "Segments";
}  // namespace simplespheregeometry
}

namespace planetgeometryprojection {

SimpleSphereGeometryProjection::SimpleSphereGeometryProjection(const ghoul::Dictionary& dictionary)
    : PlanetGeometryProjection()
    , _radius("radius", "Radius", glm::vec2(1.f, 0.f), glm::vec2(-10.f, -20.f),
              glm::vec2(10.f, 20.f))
    , _segments("segments", "Segments", 20, 1, 1000)
	, _vaoID("vaoID", "Vao", 1, 1, 1)
	, _vBufferID("vboID", "Vbo", 1, 1, 1)
	, _iBufferID("iboID", "Ibo", 1, 1, 1)
    , _planet(nullptr)
{
	using constants::scenegraphnode::keyName;
	using constants::simplespheregeometryprojection::keyRadius;
	using constants::simplespheregeometryprojection::keySegments;

	// The name is passed down from the SceneGraphNode
    std::string name;
    bool success = dictionary.getValue(keyName, name);
	assert(success);

    glm::vec2 radius;
    success = dictionary.getValue(keyRadius, radius);
	if (!success) {
        LERROR("SimpleSphereGeometry of '" << name << "' did not provide a key '"
                                           << keyRadius << "'");
	}
	else
		_radius = radius;

    double segments;
    success = dictionary.getValue(keySegments, segments);
	if (!success) {
        LERROR("SimpleSphereGeometry of '" << name << "' did not provide a key '"
                                           << keySegments << "'");
	}
	else
		_segments = static_cast<int>(segments);

    addProperty(_radius);
	_radius.onChange(std::bind(&SimpleSphereGeometryProjection::createSphere, this));
    addProperty(_segments);
	_segments.onChange(std::bind(&SimpleSphereGeometryProjection::createSphere, this));
}

SimpleSphereGeometryProjection::~SimpleSphereGeometryProjection()
{
}

bool SimpleSphereGeometryProjection::initialize(RenderablePlanetProjection* parent)
{
	bool success = PlanetGeometryProjection::initialize(parent);
    createSphere();

	//need to have this accessible in planetgeometryprojection for now  -- Michal
	_vaoID = static_cast<int>(_planet->_vaoID);
	_vBufferID = static_cast<int>(_planet->_vBufferID);
	_iBufferID = static_cast<int>(_planet->_iBufferID);
	addProperty(_vaoID);
	addProperty(_vBufferID);
	addProperty(_iBufferID);
    return success;
}

void SimpleSphereGeometryProjection::deinitialize()
{
    delete _planet;
    _planet = nullptr;
}

void SimpleSphereGeometryProjection::render()
{
    _planet->render();
}

void SimpleSphereGeometryProjection::createSphere()
{
    //create the power scaled scalar

    PowerScaledScalar planetSize(_radius);
    _parent->setBoundingSphere(planetSize);

    delete _planet;
    _planet = new PowerScaledSphere(planetSize, _segments);
    _planet->initialize();
}

}  // namespace planetgeometry
}  // namespace openspace
