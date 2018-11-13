/****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <openspace/documentation/verifier.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/rendering/renderable.h>
#include <openspace/util/powerscaledsphere.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo RadiusInfo = {
        "Radius",
        "Radius",
        "This value specifies the radius of this sphere in meters."
    };

    constexpr openspace::properties::Property::PropertyInfo SegmentsInfo = {
        "Segments",
        "Segments",
        "This value specifies the number of segments that this sphere is split into."
    };
} // namespace

namespace openspace::planetgeometry {

documentation::Documentation SimpleSphereGeometry::Documentation() {
    using namespace documentation;
    return {
        "SimpleSphereGeometry",
        "space_geometry_simplesphere",
        {
            {
                RadiusInfo.identifier,
                new OrVerifier({ new DoubleVerifier, new DoubleVector3Verifier }),
                Optional::No,
                RadiusInfo.description
            },
            {
                SegmentsInfo.identifier,
                new IntVerifier,
                Optional::No,
                SegmentsInfo.description
            }
        }
    };
}

SimpleSphereGeometry::SimpleSphereGeometry(const ghoul::Dictionary& dictionary)
    : _radius(RadiusInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(std::pow(10.f, 20.f)))
    , _segments(SegmentsInfo, 20, 1, 5000)
    , _sphere(nullptr)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "SimpleSphereGeometry"
    );

    if (dictionary.hasKeyAndValue<double>(RadiusInfo.identifier)) {
        const float r = static_cast<float>(
            dictionary.value<double>(RadiusInfo.identifier)
        );
        _radius = { r, r, r };
    }
    else {
        _radius = dictionary.value<glm::vec3>(RadiusInfo.identifier);
    }

    _segments = static_cast<int>(dictionary.value<double>(SegmentsInfo.identifier));

    // The shader need the radii values but they are not changeable runtime
    // TODO: Possibly add a scaling property @AA
    // Changing the radius/scaling should affect the shader but not the geometry? @AA
    _radius.onChange([&]() { createSphere(); });
    addProperty(_radius);

    _segments.onChange([&]() { createSphere(); });
    addProperty(_segments);
}

SimpleSphereGeometry::~SimpleSphereGeometry() {} // NOLINT

void SimpleSphereGeometry::initialize() {
    createSphere();
}

void SimpleSphereGeometry::deinitialize() {
    delete _sphere;
    _sphere = nullptr;
}

void SimpleSphereGeometry::render() {
    _sphere->render();
}

float SimpleSphereGeometry::boundingSphere() const {
    const glm::vec3 radius = _radius;
    return std::max(std::max(radius[0], radius[1]), radius[2]);
}

void SimpleSphereGeometry::createSphere() {
    const glm::vec3 radius = _radius.value();

    delete _sphere;
    _sphere = new PowerScaledSphere(glm::vec4(radius, 0.0), _segments);
    _sphere->initialize();
}

}  // namespace openspace::planetgeometry
