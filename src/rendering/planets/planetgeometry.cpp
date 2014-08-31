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

#include <openspace/rendering/planets/planetgeometry.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/constants.h>
#include <openspace/util/factorymanager.h>

namespace {
const std::string _loggerCat = "PlanetGeometry";
}

namespace openspace {
namespace planetgeometry {

PlanetGeometry* PlanetGeometry::createFromDictionary(const ghoul::Dictionary& dictionary)
{
    std::string name;
    dictionary.getValue(constants::scenegraphnode::keyName, name);

    if (!dictionary.hasValue<std::string>(constants::planetgeometry::keyType)) {
        LERROR("PlanetGeometry for '" << name << "' did not contain a '"
                                      << constants::planetgeometry::keyType << "' key");
        return nullptr;
    }
    std::string geometryType;
    dictionary.getValue(constants::planetgeometry::keyType, geometryType);

    ghoul::TemplateFactory<PlanetGeometry>* factory
          = FactoryManager::ref().factory<PlanetGeometry>();

    PlanetGeometry* result = factory->create(geometryType, dictionary);
    if (result == nullptr) {
        LERROR("Failed to create a PlanetGeometry object of type '" << geometryType
                                                                    << "'");
        return nullptr;
    }
    return result;
}

PlanetGeometry::PlanetGeometry()
    : _parent(nullptr)
{
	setName("PlanetGeometry");
}

PlanetGeometry::~PlanetGeometry()
{
}

bool PlanetGeometry::initialize(RenderablePlanet* parent)
{
    _parent = parent;
    return true;
}

void PlanetGeometry::deinitialize()
{
}

}  // namespace planetgeometry
}  // namespace openspace
