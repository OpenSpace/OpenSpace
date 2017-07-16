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

#include <modules/space/rendering/planetgeometry.h>

#include <openspace/util/factorymanager.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

namespace {
    const char* _loggerCat = "PlanetGeometry";
    const char* KeyType = "Type";
} // namespace

namespace openspace::planetgeometry {

documentation::Documentation PlanetGeometry::Documentation() {
    using namespace documentation;
    return {
        "Planet Geometry",
        "space_geometry_planet",
        {
            {
                KeyType,
                new StringVerifier,
                "The type of the PlanetGeometry that will can be constructed.",
                Optional::No
            }
        }
    };
}

std::unique_ptr<PlanetGeometry> PlanetGeometry::createFromDictionary(
                                                      const ghoul::Dictionary& dictionary)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "PlanetGeometry"
    );

    std::string geometryType = dictionary.value<std::string>(KeyType);
    auto factory = FactoryManager::ref().factory<PlanetGeometry>();

    std::unique_ptr<PlanetGeometry> result = factory->create(geometryType, dictionary);
    if (result == nullptr) {
        throw ghoul::RuntimeError(
            "Failed to create a PlanetGeometry object of type '" + geometryType + "'"
        );
    }
    return result;
}

PlanetGeometry::PlanetGeometry()
    : properties::PropertyOwner("PlanetGeometry")
    , _parent(nullptr)
{}

PlanetGeometry::~PlanetGeometry() {}

bool PlanetGeometry::initialize(Renderable* parent) {
    _parent = parent;
    return true;
}

void PlanetGeometry::deinitialize() {}

}  // namespace openspace::planetgeometry
