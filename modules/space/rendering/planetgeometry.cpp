/*****************************************************************************************
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

#include <modules/space/rendering/planetgeometry.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/misc/templatefactory.h>

namespace {
    constexpr const char* KeyType = "Type";
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
                Optional::No,
                "The type of the PlanetGeometry that will can be constructed."
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
    return result;
}

PlanetGeometry::PlanetGeometry() : properties::PropertyOwner({ "PlanetGeometry" }) {}

void PlanetGeometry::initialize() {}

void PlanetGeometry::deinitialize() {}

}  // namespace openspace::planetgeometry
