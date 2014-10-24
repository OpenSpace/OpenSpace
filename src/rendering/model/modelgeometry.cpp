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

#include <openspace/rendering/model/modelgeometry.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/constants.h>
#include <openspace/util/factorymanager.h>

namespace {
const std::string _loggerCat = "ModelGeometry";
}

namespace openspace {
namespace modelgeometry {

ModelGeometry* ModelGeometry::createFromDictionary(const ghoul::Dictionary& dictionary)
{
	std::string geometryType;
	const bool success = dictionary.getValue(
		constants::modelgeometry::keyType, geometryType);
	if (!success) {
        LERROR("ModelGeometry did not contain a correct value of the key '"
			<< constants::modelgeometry::keyType << "'");
        return nullptr;
	}
	ghoul::TemplateFactory<ModelGeometry>* factory
		= FactoryManager::ref().factory<ModelGeometry>();

	ModelGeometry* result = factory->create(geometryType, dictionary);
    if (result == nullptr) {
        LERROR("Failed to create a ModelGeometry object of type '" << geometryType
                                                                    << "'");
        return nullptr;
    }
    return result;
}

ModelGeometry::ModelGeometry()
    : _parent(nullptr)
{
	setName("ModelGeometry");
}

ModelGeometry::~ModelGeometry()
{
}

bool ModelGeometry::initialize(RenderableModel* parent){
    _parent = parent;
    return true;
}
	void ModelGeometry::deinitialize()
{
}

}  // namespace modelgeometry
}  // namespace openspace
