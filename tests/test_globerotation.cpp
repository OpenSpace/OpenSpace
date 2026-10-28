/*****************************************************************************************\
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this   *
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
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE   *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include <catch2/catch_test_macros.hpp>

#include <modules/base/rotation/globerotation.h>
#include <modules/base/translation/globetranslation.h>
#include <openspace/documentation/documentation.h>
#include <openspace/misc/dictionary.h>

#include <string>

namespace {

using namespace openspace;

// Find the "Globe" entry in a documentation object and report whether it is marked as
// optional
bool isGlobeOptional(const Documentation& doc) {
    for (const DocumentationEntry& entry : doc.entries) {
        if (entry.key == "Globe") {
            return entry.optional == Optional::Yes;
        }
    }
    return false;
}

} // namespace

TEST_CASE("GlobeRotation: Globe parameter is optional", "[globerotation]") {
    // A dictionary without a Globe field should be valid: the rotation then falls back to
    // the scene graph node it is attached to
    Dictionary dict;
    dict.setValue("Type", std::string("GlobeRotation"));
    dict.setValue("Latitude", 20.0);
    dict.setValue("Longitude", -45.0);

    // Constructing a GlobeRotation from this dictionary must not throw
    GlobeRotation rotation(dict);
    CHECK(&rotation != nullptr);
}

TEST_CASE("GlobeRotation: Globe parameter is still accepted", "[globerotation]") {
    // A dictionary with an explicit Globe field should still be valid
    Dictionary dict;
    dict.setValue("Type", std::string("GlobeRotation"));
    dict.setValue("Globe", std::string("SomeGlobeNode"));
    dict.setValue("Latitude", 20.0);
    dict.setValue("Longitude", -45.0);

    GlobeRotation rotation(dict);
    CHECK(&rotation != nullptr);
}

TEST_CASE("GlobeRotation: documentation marks Globe as optional", "[globerotation]") {
    const Documentation doc = GlobeRotation::Documentation();
    CHECK(isGlobeOptional(doc));
}

TEST_CASE("GlobeTranslation: Globe parameter is optional", "[globetranslation]") {
    // A dictionary without a Globe field should be valid: the translation then falls back
    // to the scene graph node it is attached to
    Dictionary dict;
    dict.setValue("Type", std::string("GlobeTranslation"));
    dict.setValue("Latitude", 20.0);
    dict.setValue("Longitude", -45.0);
    dict.setValue("Altitude", 1000.0);

    // Constructing a GlobeTranslation from this dictionary must not throw
    GlobeTranslation translation(dict);
    CHECK(&translation != nullptr);
}

TEST_CASE("GlobeTranslation: Globe parameter is still accepted", "[globetranslation]") {
    // A dictionary with an explicit Globe field should still be valid
    Dictionary dict;
    dict.setValue("Type", std::string("GlobeTranslation"));
    dict.setValue("Globe", std::string("SomeGlobeNode"));
    dict.setValue("Latitude", 20.0);
    dict.setValue("Longitude", -45.0);
    dict.setValue("Altitude", 1000.0);

    GlobeTranslation translation(dict);
    CHECK(&translation != nullptr);
}

TEST_CASE(
    "GlobeTranslation: documentation marks Globe as optional",
    "[globetranslation]"
)
{
    const Documentation doc = GlobeTranslation::Documentation();
    CHECK(isGlobeOptional(doc));
}
