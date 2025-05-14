/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <modules/base/rotation/multirotation.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/time.h>
#include <optional>

namespace {
    // This Rotation type combines multiple individual rotations that are applied one
    // after the other. The rotations are applied in the order in which they are specified
    // in the `Rotations` key.
    struct [[codegen::Dictionary(MultiRotation)]] Parameters {
        // The list of rotations that are applied one after the other
        std::vector<ghoul::Dictionary> rotations
            [[codegen::reference("core_transform_rotation")]];
    };
#include "multirotation_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation MultiRotation::Documentation() {
    return codegen::doc<Parameters>("base_transform_rotation_multi");
}

MultiRotation::MultiRotation(const ghoul::Dictionary& dictionary)
    : Rotation(dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    int i = 0;
    for (const ghoul::Dictionary& rot : p.rotations) {
        ghoul::mm_unique_ptr<Rotation> rotation = Rotation::createFromDictionary(rot);
        rotation->setGuiName(std::format("{}: {}", i, rotation->guiName()));
        rotation->setIdentifier(std::format("{}_{}", i, rotation->identifier()));
        addPropertySubOwner(rotation.get());
        _rotations.push_back(std::move(rotation));
        i++;
    }
}

void MultiRotation::update(const UpdateData& data) {
    for (const ghoul::mm_unique_ptr<Rotation>& rot : _rotations) {
        rot->update(data);
    }
}

glm::dmat3 MultiRotation::matrix(const UpdateData& data) const {
    glm::dmat3 res = glm::dmat3(1.0);
    for (const ghoul::mm_unique_ptr<Rotation>& rot : _rotations) {
        res *= rot->matrix(data);
    }
    return res;
}

} // namespace openspace
