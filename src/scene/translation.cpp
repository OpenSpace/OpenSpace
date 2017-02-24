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

#include <openspace/scene/translation.h>

#include <openspace/util/factorymanager.h>
#include <ghoul/logging/logmanager.h>

#include <openspace/documentation/verifier.h>

namespace {
    const std::string _loggerCat = "Translation";
    const std::string KeyType = "Type";
}

namespace openspace {

Documentation Translation::Documentation() {
    using namespace openspace::documentation;

    return {
        "Transformation Translation",
        "core_transform_translation",
        {
            {
                KeyType,
                new StringAnnotationVerifier("Must name a valid Translation type"),
                "The type of translation that is described in this element. "
                "The available types of translations depend on the "
                "configuration of the application and can be written to disk "
                "on application startup into the FactoryDocumentation.",
                Optional::No
            }
        },
        Exhaustive::No
    };
}

Translation* Translation::createFromDictionary(const ghoul::Dictionary& dictionary) {
    if (!dictionary.hasValue<std::string>(KeyType)) {
        LERROR("Translation did not have key '" << KeyType << "'");
        return nullptr;
    }

    std::string translationType;
    dictionary.getValue(KeyType, translationType);
    ghoul::TemplateFactory<Translation>* factory
          = FactoryManager::ref().factory<Translation>();
    Translation* result = factory->create(translationType, dictionary);
    result->setName("Translation");
    if (result == nullptr) {
        LERROR("Failed creating Translation object of type '" << translationType << "'");
        return nullptr;
    }

    return result;
}

Translation::~Translation() {}
    
bool Translation::initialize() {
    return true;
}
    
void Translation::update(const UpdateData& data) {}

glm::dvec3 Translation::position(double time) {
    update({
        {},
        time,
        1.0,
        false,
        false
    });

    return position();
}

void Translation::notifyObservers() {
    if (_onParameterChangeCallback) {
        _onParameterChangeCallback();
    }
}

void Translation::onParameterChange(std::function<void()> callback) {
    _onParameterChangeCallback = std::move(callback);
}

} // namespace openspace
