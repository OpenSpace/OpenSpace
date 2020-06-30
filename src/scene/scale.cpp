/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <openspace/scene/scale.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/templatefactory.h>

namespace {
    constexpr const char* KeyType = "Type";
} // namespace

namespace openspace {

documentation::Documentation Scale::Documentation() {
    using namespace openspace::documentation;

    return {
        "Transformation Scaling",
        "core_transform_scaling",
        {
            {
                KeyType,
                new StringAnnotationVerifier("Must name a valid Scale type"),
                Optional::No,
                "The type of the scaling that is described in this element. "
                "The available types of scaling depend on the configuration "
                "of the application and can be written to disk on "
                "application startup into the FactoryDocumentation."
            }
        }
    };
}

std::unique_ptr<Scale> Scale::createFromDictionary(const ghoul::Dictionary& dictionary) {
    documentation::testSpecificationAndThrow(Documentation(), dictionary, "Scale");

    std::string scaleType = dictionary.value<std::string>(KeyType);

    auto factory = FactoryManager::ref().factory<Scale>();
    std::unique_ptr<Scale> result = factory->create(scaleType, dictionary);
    result->setIdentifier("Scale");
    return result;
}

Scale::Scale() : properties::PropertyOwner({ "Scale" }) {}

void Scale::requireUpdate() {
    _needsUpdate = true;
}

bool Scale::initialize() {
    return true;
}

glm::dvec3 Scale::scaleValue() const {
    return _cachedScale;
}

void Scale::update(const UpdateData& data) {
    if (!_needsUpdate && data.time.j2000Seconds() == _cachedTime) {
        return;
    }
    _cachedScale = scaleValue(data);
    _cachedTime = data.time.j2000Seconds();
    _needsUpdate = false;
}

} // namespace openspace
