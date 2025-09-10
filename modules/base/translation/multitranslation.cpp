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

#include <modules/base/translation/multitranslation.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/time.h>
#include <optional>

namespace {
    // This Translation type combines multiple translations that are applied one after the
    // other.
    struct [[codegen::Dictionary(MultiTranslation)]] Parameters {
        // The list of translations that are applied one after the other
        std::vector<ghoul::Dictionary> translations
            [[codegen::reference("core_transform_translation")]];
    };
#include "multitranslation_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation MultiTranslation::Documentation() {
    return codegen::doc<Parameters>("base_transform_translation_multi");
}

MultiTranslation::MultiTranslation(const ghoul::Dictionary& dictionary)
    : Translation(dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    int i = 0;
    for (const ghoul::Dictionary& trans : p.translations) {
        ghoul::mm_unique_ptr<Translation> translation =
            Translation::createFromDictionary(trans);
        translation->setGuiName(std::format("{}: {}", i, translation->guiName()));
        translation->setIdentifier(std::format("{}_{}", i, translation->identifier()));
        addPropertySubOwner(translation.get());
        _translations.push_back(std::move(translation));
        i++;
    }
}

void MultiTranslation::initialize() {
    Translation::initialize();
    for (const ghoul::mm_unique_ptr<Translation>& translation : _translations) {
        translation->initialize();
    }
}

void MultiTranslation::update(const UpdateData& data) {
    for (const ghoul::mm_unique_ptr<Translation>& translation : _translations) {
        translation->update(data);
    }
}

glm::dvec3 MultiTranslation::position(const UpdateData& data) const {
    glm::dvec3 res = glm::dvec3(1.0);
    for (const ghoul::mm_unique_ptr<Translation>& translation : _translations) {
        res += translation->position(data);
    }
    return res;
}

} // namespace openspace
