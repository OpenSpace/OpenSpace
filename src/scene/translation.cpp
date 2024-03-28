/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <openspace/engine/globals.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/memorymanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/templatefactory.h>

namespace {
    struct [[codegen::Dictionary(Translation)]] Parameters {
        // The type of translation that is described in this element. The available types
        // of translations depend on the configuration of the application and can be
        // written to disk on application startup into the FactoryDocumentation
        std::string type [[codegen::annotation("Must name a valid Translation type")]];
    };
#include "translation_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation Translation::Documentation() {
    return codegen::doc<Parameters>("core_transform_translation");
}

ghoul::mm_unique_ptr<Translation> Translation::createFromDictionary(
                                                      const ghoul::Dictionary& dictionary)
{
    ZoneScoped;

    const Parameters p = codegen::bake<Parameters>(dictionary);

    Translation* result = FactoryManager::ref().factory<Translation>()->create(
        p.type,
        dictionary,
        &global::memoryManager->PersistentMemory
    );
    result->_type = p.type;
    return ghoul::mm_unique_ptr<Translation>(result);
}

Translation::Translation() : properties::PropertyOwner({ "Translation" }) {}

bool Translation::initialize() {
    return true;
}

void Translation::update(const UpdateData& data) {
    if (!_needsUpdate && data.time.j2000Seconds() == _cachedTime) {
        return;
    }
    const glm::dvec3 oldPosition = _cachedPosition;
    _cachedPosition = position(data);
    _cachedTime = data.time.j2000Seconds();
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
