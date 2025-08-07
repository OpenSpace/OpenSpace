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

#include <modules/base/scale/multiscale.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/time.h>
#include <openspace/util/updatestructures.h>
#include <optional>

namespace {
    // This Scale type combines multiple individual scale operations that are applied one
    // after the other.
    struct [[codegen::Dictionary(MultiScale)]] Parameters {
        // The list of scales that are applied one after the other
        std::vector<ghoul::Dictionary> scales
            [[codegen::reference("core_transform_scale")]];
    };
#include "multiscale_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation MultiScale::Documentation() {
    return codegen::doc<Parameters>("base_transform_scale_multi");
}

MultiScale::MultiScale(const ghoul::Dictionary& dictionary)
    : Scale(dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    int i = 0;
    for (const ghoul::Dictionary& s : p.scales) {
        ghoul::mm_unique_ptr<Scale> scale = Scale::createFromDictionary(s);
        scale->setGuiName(std::format("{}: {}", i, scale->guiName()));
        scale->setIdentifier(std::format("{}_{}", i, scale->identifier()));
        addPropertySubOwner(scale.get());
        _scales.push_back(std::move(scale));
        i++;
    }
}

void MultiScale::initialize() {
    Scale::initialize();
    for (const ghoul::mm_unique_ptr<Scale>& scale : _scales) {
        scale->initialize();
    }
}

void MultiScale::update(const UpdateData& data) {
    for (const ghoul::mm_unique_ptr<Scale>& scale : _scales) {
        scale->update(data);
    }
}

glm::dvec3 MultiScale::scaleValue(const UpdateData& data) const {
    glm::dvec3 res = glm::dvec3(1.0);
    for (const ghoul::mm_unique_ptr<Scale>& scale : _scales) {
        res *= scale->scaleValue(data);
    }
    return res;
}

} // namespace openspace
