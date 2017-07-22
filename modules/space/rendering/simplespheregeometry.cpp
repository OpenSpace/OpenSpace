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

#include <modules/space/rendering/simplespheregeometry.h>
#include <openspace/util/powerscaledsphere.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/rendering/renderable.h>

namespace {
    const char* _loggerCat = "SimpleSphereGeometry";

    const char* keyRadius = "Radius";
    const char* keySegments = "Segments";
} // namespace

namespace openspace::planetgeometry {

SimpleSphereGeometry::SimpleSphereGeometry(const ghoul::Dictionary& dictionary)
    : PlanetGeometry()
    , _radius(
        { "radius", "Radius", "" }, // @TODO Missing documentation
        glm::vec3(1.f, 1.f, 1.f),
        glm::vec3(0.f, 0.f, 0.f),
        glm::vec3(std::pow(10.f, 20.f), std::pow(10.f, 20.f), std::pow(10.f, 20.f)))
    , _segments({ "segments", "Segments", "" }, 20, 1, 5000) // @TODO Missing documentation
    , _sphere(nullptr)
{
    float sphereRadius = 0.f;
    glm::vec3 ellipsoidRadius;
    if (dictionary.getValue(keyRadius, sphereRadius)) {
        _radius = { sphereRadius, sphereRadius, sphereRadius };
    } else if (dictionary.getValue(keyRadius, ellipsoidRadius)) {
        _radius = ellipsoidRadius;
    } else {
        LERROR("SimpleSphereGeometry did not provide a key '"
            << keyRadius << "'");   
    }

    double segments = 0;
    if (dictionary.getValue(keySegments, segments)) {
        _segments = static_cast<int>(segments);
    } else {
        LERROR("SimpleSphereGeometry did not provide a key '"
            << keySegments << "'");
    }

    // The shader need the radii values but they are not changeable runtime
    // TODO: Possibly add a scaling property @AA
    addProperty(_radius);
    // Changing the radius/scaling should affect the shader but not the geometry? @AA
    _radius.onChange(std::bind(&SimpleSphereGeometry::createSphere, this));
    addProperty(_segments);
    _segments.onChange(std::bind(&SimpleSphereGeometry::createSphere, this));
}

SimpleSphereGeometry::~SimpleSphereGeometry() {
}

bool SimpleSphereGeometry::initialize(Renderable* parent)
{
    bool success = PlanetGeometry::initialize(parent);
    createSphere();
    return success;
}

void SimpleSphereGeometry::deinitialize() {
    if (_sphere)
        delete _sphere;
    _sphere = nullptr;
}

void SimpleSphereGeometry::render() {
    _sphere->render();
}

void SimpleSphereGeometry::createSphere(){
    const glm::vec3 radius = _radius.value();
    _parent->setBoundingSphere(std::max(std::max(radius[0], radius[1]), radius[2]));

    if(_sphere)
        delete _sphere;

    _sphere = new PowerScaledSphere(glm::vec4(radius, 0.0), _segments);
    _sphere->initialize();
}

}  // namespace openspace::planetgeometry
