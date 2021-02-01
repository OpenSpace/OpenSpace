/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
    struct [[codegen::Dictionary(PlanetGeometry)]] Parameters {
        // The type of the PlanetGeometry that will can be constructed
        std::string type;
    };
#include "planetgeometry_codegen.cpp"
} // namespace

namespace openspace::planetgeometry {

documentation::Documentation PlanetGeometry::Documentation() {
    documentation::Documentation doc = codegen::doc<Parameters>();
    doc.id = "space_geometry_planet";
    return doc;
}

std::unique_ptr<PlanetGeometry> PlanetGeometry::createFromDictionary(
                                                      const ghoul::Dictionary& dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    auto factory = FactoryManager::ref().factory<PlanetGeometry>();

    PlanetGeometry* result = factory->create(p.type, dictionary);
    return std::unique_ptr<PlanetGeometry>(result);
}

PlanetGeometry::PlanetGeometry() : properties::PropertyOwner({ "PlanetGeometry" }) {}

void PlanetGeometry::initialize() {}

void PlanetGeometry::deinitialize() {}

}  // namespace openspace::planetgeometry
