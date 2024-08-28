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

#include <openspace/scene/rotation.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/memorymanager.h>
#include <openspace/util/time.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/templatefactory.h>

namespace {
    struct [[codegen::Dictionary(Rotation)]] Parameters {
        // The type of the rotation that is described in this element. The available types
        // of rotations depend on the configuration of the application and can be written
        // to disk on application startup into the FactoryDocumentation
        std::string type [[codegen::annotation("Must name a valid Rotation type")]];
    };
#include "rotation_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation Rotation::Documentation() {
    return codegen::doc<Parameters>("core_transform_rotation");
}

ghoul::mm_unique_ptr<Rotation> Rotation::createFromDictionary(
                                                      const ghoul::Dictionary& dictionary)
{
    ZoneScoped;

    const Parameters p = codegen::bake<Parameters>(dictionary);

    Rotation* result = FactoryManager::ref().factory<Rotation>()->create(
        p.type,
        dictionary,
        &global::memoryManager->PersistentMemory
    );
    result->_type = p.type;
    return ghoul::mm_unique_ptr<Rotation>(result);
}

Rotation::Rotation() : properties::PropertyOwner({ "Rotation" }) {}

void Rotation::requireUpdate() {
    _needsUpdate = true;
}

bool Rotation::initialize() {
    return true;
}

const glm::dmat3& Rotation::matrix() const {
    return _cachedMatrix;
}

void Rotation::update(const UpdateData& data) {
    ZoneScoped;

    if (!_needsUpdate && (data.time.j2000Seconds() == _cachedTime)) {
        return;
    }
    _cachedMatrix = matrix(data);
    _cachedTime = data.time.j2000Seconds();
    _needsUpdate = false;
}

} // namespace openspace
