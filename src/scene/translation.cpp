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

#include <openspace/scene/translation.h>

#include <openspace/documentation/verifier.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/templatefactory.h>

namespace {
    const char* KeyType = "Type";
} // namespace

namespace openspace {

documentation::Documentation Translation::Documentation() {
    using namespace documentation;

    return {
        "Transformation Translation",
        "core_transform_translation",
        {
            {
                KeyType,
                new StringAnnotationVerifier("Must name a valid Translation type"),
                Optional::No,
                "The type of translation that is described in this element. "
                "The available types of translations depend on the "
                "configuration of the application and can be written to disk "
                "on application startup into the FactoryDocumentation."
            }
        }
    };
}

std::unique_ptr<Translation> Translation::createFromDictionary(
                                                      const ghoul::Dictionary& dictionary)
{
    documentation::testSpecificationAndThrow(Documentation(), dictionary, "Translation");

    const std::string& translationType = dictionary.value<std::string>(KeyType);
    ghoul::TemplateFactory<Translation>* factory
          = FactoryManager::ref().factory<Translation>();
    std::unique_ptr<Translation> result = factory->create(translationType, dictionary);
    result->setIdentifier("Translation");
    return result;
}

Translation::Translation() : properties::PropertyOwner({ "Translation" }) {}

bool Translation::initialize() {
    return true;
}

void Translation::update(const Time& time) {
    if (!_needsUpdate && time.j2000Seconds() == _cachedTime) {
        return;
    }
    const glm::dvec3 oldPosition = _cachedPosition;
    _cachedPosition = position(time);
    _cachedTime = time.j2000Seconds();
    _needsUpdate = false;

    if (oldPosition != _cachedPosition) {
        notifyObservers();
    }
}

glm::dvec3 Translation::position() const {
    return _cachedPosition;
}

void Translation::notifyObservers() const {
    if (_onParameterChangeCallback) {
        _onParameterChangeCallback();
    }
}

void Translation::requireUpdate() {
    _needsUpdate = true;
}

void Translation::onParameterChange(std::function<void()> callback) {
    _onParameterChangeCallback = std::move(callback);
}

} // namespace openspace
