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

#include <catch2/catch_test_macros.hpp>

#include <openspace/engine/globals.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/scripting/scriptengine.h>

using namespace openspace;
using namespace properties;

namespace {
    void triggerScriptRun() {
        // Offloading this into a separate function as it would otherwise be a lot of
        // non-intuitive copy-and-paste that we might want to change later anyway
        global::scriptEngine->preSync(true);
        global::scriptEngine->postSync(true);
    }
} // namespace

TEST_CASE("SetPropertyValue", "[setpropertyvalue]") {
    PropertyOwner owner = PropertyOwner({ "base" });
    global::rootPropertyOwner->addPropertySubOwner(owner);
    properties::FloatProperty p1(properties::Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner.addProperty(p1);

    global::scriptEngine->queueScript(
        "openspace.setPropertyValueSingle('base.p1', 2.0)"
    );

    CHECK(p1 == 1.f);
    triggerScriptRun();
    CHECK(p1 == 2.f);
}
