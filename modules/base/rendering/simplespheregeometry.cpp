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

#include <modules/base/rendering/simplespheregeometry.h>
#include <openspace/util/powerscaledsphere.h>
#include <openspace/scene/scenegraphnode.h>

namespace {
    const std::string _loggerCat = "SimpleSphereGeometry";
}

namespace openspace {

namespace constants {
namespace simplespheregeometry {
    const char* keyRadius = "Radius";
    const char* keySegments = "Segments";
}  // namespace simplespheregeometry
}

namespace planetgeometry {

SimpleSphereGeometry::SimpleSphereGeometry(const ghoul::Dictionary& dictionary)
    : PlanetGeometry()
    , _realRadius("radius", "Radius", glm::vec4(1.f, 1.f, 1.f, 0.f), glm::vec4(-10.f, -10.f, -10.f, -20.f),
                glm::vec4(10.f, 10.f, 10.f, 20.f))
    , _segments("segments", "Segments", 20, 1, 5000)
    , _sphere(nullptr)
{
    using constants::simplespheregeometry::keyRadius;
    using constants::simplespheregeometry::keySegments;

    // The name is passed down from the SceneGraphNode
    bool success = dictionary.getValue(SceneGraphNode::KeyName, _name);
    assert(success);
    
    glm::vec4 radius;
    success = dictionary.getValue(keyRadius, _modRadius);
    if (!success) {
        LERROR("SimpleSphereGeometry of '" << _name << "' did not provide a key '"
                                           << keyRadius << "'");
    }
    else {
        radius[0] = _modRadius[0];
        radius[1] = _modRadius[0];
        radius[2] = _modRadius[0];
        radius[3] = _modRadius[1];
        _realRadius = radius; // In case the kernels does not supply a real
    }

    double segments;
    success = dictionary.getValue(keySegments, segments);
    if (!success) {
        LERROR("SimpleSphereGeometry of '" << _name << "' did not provide a key '"
                                           << keySegments << "'");
    }
    else
        _segments = static_cast<int>(segments);
    // The shader need the radii values but they are not changeable runtime
    // TODO: Possibly add a scaling property @AA
    addProperty(_realRadius);
    // Changing the radius/scaling should affect the shader but not the geometry? @AA
    //_radius.onChange(std::bind(&SimpleSphereGeometry::createSphere, this));
    addProperty(_segments);
    _segments.onChange(std::bind(&SimpleSphereGeometry::createSphere, this));
}

SimpleSphereGeometry::~SimpleSphereGeometry()
{
}

bool SimpleSphereGeometry::initialize(Renderable* parent)
{
    bool success = PlanetGeometry::initialize(parent);
    createSphere();
    return success;
}

void SimpleSphereGeometry::deinitialize()
{
    if (_sphere)
        delete _sphere;
    _sphere = nullptr;
}

void SimpleSphereGeometry::render()
{
    _sphere->render();
}

void SimpleSphereGeometry::createSphere(){
    //create the power scaled scalar

    PowerScaledScalar planetSize(_modRadius);
    _parent->setBoundingSphere(planetSize);

    if(_sphere)
        delete _sphere;
    //_sphere = new PowerScaledSphere(planetSize, _segments);
    _sphere = new PowerScaledSphere(_realRadius, _segments, _name);
    _sphere->initialize();
}

}  // namespace planetgeometry
}  // namespace openspace
