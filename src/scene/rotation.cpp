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

#include <openspace/scene/rotation.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/factorymanager.h>

#include <ghoul/logging/logmanager.h>

namespace {
    const char* _loggerCat = "Rotation";
    const char* KeyType = "Type";
}

namespace openspace {

documentation::Documentation Rotation::Documentation() {
    using namespace openspace::documentation;

    return {
        "Transformation Rotation",
        "core_transform_rotation",
        {
            {
                KeyType,
                new StringAnnotationVerifier("Must name a valid Rotation type."),
                "The type of the rotation that is described in this element. The "
                "available types of rotations depend on the configuration of the "
                "application and can be written to disk on application startup into the "
                "FactoryDocumentation.",
                Optional::No
            }
        }
    };
}

Rotation* Rotation::createFromDictionary(const ghoul::Dictionary& dictionary) {
    documentation::testSpecificationAndThrow(Documentation(), dictionary, "Rotation");

    std::string rotationType = dictionary.value<std::string>(KeyType);
    auto factory = FactoryManager::ref().factory<Rotation>();
    Rotation* result = factory->create(rotationType, dictionary);
    if (result == nullptr) {
        LERROR("Failed creating Rotation object of type '" << rotationType << "'");
        return nullptr;
    }

    return result;
}

Rotation::Rotation() {
    setName("Rotation");
}
    
Rotation::Rotation(const ghoul::Dictionary& dictionary) {}
    
Rotation::~Rotation() {}
    
bool Rotation::initialize() {
    return true;
}
    
const glm::dmat3& Rotation::matrix() const {
    return _matrix;
}

void Rotation::update(const UpdateData& data) {}

} // namespace openspace
