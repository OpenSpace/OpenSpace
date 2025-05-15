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
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/sceneinitializer.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/defer.h>
#include <catch2/matchers/catch_matchers_floating_point.hpp>
#include <chrono>

using namespace openspace;
using namespace properties;

namespace {
    // Offloading this into a separate function as it would otherwise be a lot of
    // non-intuitive copy-and-paste that we might want to change later anyway
    void triggerScriptRun() {
        global::scriptEngine->preSync(true);
        global::scriptEngine->postSync(true);
    }
} // namespace

TEST_CASE("Property: Basic", "[property]") {
    PropertyOwner owner = PropertyOwner({ "base" });
    global::rootPropertyOwner->addPropertySubOwner(owner);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner.addProperty(p1);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript({
            .code = "return openspace.property('base.p1')",
            .callback = [&p1](ghoul::Dictionary d) {
                REQUIRE(d.size() == 1);
                REQUIRE(d.hasKey("1"));
                REQUIRE(d.hasValue<ghoul::Dictionary>("1"));
                ghoul::Dictionary e = d.value<ghoul::Dictionary>("1");
                REQUIRE(e.size() == 1);
                REQUIRE(e.hasKey("1"));
                REQUIRE(e.hasValue<std::string>("1"));
                const std::string v = e.value<std::string>("1");
                CHECK(v == p1.uri());
            }
        });

        triggerScriptRun();
    }
}

TEST_CASE("Property: Empty", "[property]") {
    PropertyOwner owner = PropertyOwner({ "base" });
    global::rootPropertyOwner->addPropertySubOwner(owner);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner.addProperty(p1);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript({
            .code = "return openspace.property('other-name')",
            .callback = [](ghoul::Dictionary d) {
                REQUIRE(d.size() == 1);
                REQUIRE(d.hasKey("1"));
                REQUIRE(d.hasValue<ghoul::Dictionary>("1"));
                ghoul::Dictionary e = d.value<ghoul::Dictionary>("1");
                CHECK(e.size() == 0);
            }
        });

        triggerScriptRun();
    }
    
    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript({
            .code = "return openspace.property('base.other-name')",
            .callback = [](ghoul::Dictionary d) {
                REQUIRE(d.size() == 1);
                REQUIRE(d.hasKey("1"));
                REQUIRE(d.hasValue<ghoul::Dictionary>("1"));
                ghoul::Dictionary e = d.value<ghoul::Dictionary>("1");
                CHECK(e.size() == 0);
            }
        });

        triggerScriptRun();
    }
}

TEST_CASE("Property: Multiple", "[property]") {
    PropertyOwner owner = PropertyOwner({ "base" });
    global::rootPropertyOwner->addPropertySubOwner(owner);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner.addProperty(p1);
    FloatProperty p2 = FloatProperty(Property::PropertyInfo("p2", "a", "b"), 1.f);
    owner.addProperty(p2);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript({
            .code = "return openspace.property('base.*')",
            .callback = [&p1, &p2](ghoul::Dictionary d) {
                REQUIRE(d.size() == 1);
                REQUIRE(d.hasKey("1"));
                REQUIRE(d.hasValue<ghoul::Dictionary>("1"));
                ghoul::Dictionary e = d.value<ghoul::Dictionary>("1");
                REQUIRE(e.size() == 2);

                // Sorting the results as the property return order is undefined
                std::array<std::string, 2> r;
                {
                    REQUIRE(e.hasKey("1"));
                    REQUIRE(e.hasValue<std::string>("1"));
                    const std::string v = e.value<std::string>("1");
                    r[0] = v;
                }
                {
                    REQUIRE(e.hasKey("2"));
                    REQUIRE(e.hasValue<std::string>("2"));
                    const std::string v = e.value<std::string>("2");
                    r[1] = v;
                }

                std::sort(r.begin(), r.end());
                CHECK(r[0] == p1.uri());
                CHECK(r[1] == p2.uri());
            }
        });

        triggerScriptRun();
    }
}

TEST_CASE("Property: Multiple/2", "[property]") {
    PropertyOwner owner1 = PropertyOwner({ "base1" });
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);
    FloatProperty p2 = FloatProperty(Property::PropertyInfo("p2", "a", "b"), 1.f);
    owner1.addProperty(p2);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);
    FloatProperty p22 = FloatProperty(Property::PropertyInfo("p2", "a", "b"), 1.f);
    owner2.addProperty(p22);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript({
            .code = "return openspace.property('base1.*')",
            .callback = [&p1, &p2](ghoul::Dictionary d) {
                REQUIRE(d.size() == 1);
                REQUIRE(d.hasKey("1"));
                REQUIRE(d.hasValue<ghoul::Dictionary>("1"));
                ghoul::Dictionary e = d.value<ghoul::Dictionary>("1");
                REQUIRE(e.size() == 2);

                // Sorting the results as the property return order is undefined
                std::array<std::string, 2> r;
                {
                    REQUIRE(e.hasKey("1"));
                    REQUIRE(e.hasValue<std::string>("1"));
                    const std::string v = e.value<std::string>("1");
                    r[0] = v;
                }
                {
                    REQUIRE(e.hasKey("2"));
                    REQUIRE(e.hasValue<std::string>("2"));
                    const std::string v = e.value<std::string>("2");
                    r[1] = v;
                }
                std::sort(r.begin(), r.end());
                CHECK(r[0] == p1.uri());
                CHECK(r[1] == p2.uri());
            }
        });

        triggerScriptRun();
    }

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript({
            .code = "return openspace.property('*.p1')",
            .callback = [&p1, &p21](ghoul::Dictionary d) {
                REQUIRE(d.size() == 1);
                REQUIRE(d.hasKey("1"));
                REQUIRE(d.hasValue<ghoul::Dictionary>("1"));
                ghoul::Dictionary e = d.value<ghoul::Dictionary>("1");
                REQUIRE(e.size() == 2);

                // Sorting the results as the property return order is undefined
                std::array<std::string, 2> r;
                {
                    REQUIRE(e.hasKey("1"));
                    REQUIRE(e.hasValue<std::string>("1"));
                    const std::string v = e.value<std::string>("1");
                    r[0] = v;
                }
                {
                    REQUIRE(e.hasKey("2"));
                    REQUIRE(e.hasValue<std::string>("2"));
                    const std::string v = e.value<std::string>("2");
                    r[1] = v;
                }
                std::sort(r.begin(), r.end());
                CHECK(r[0] == p1.uri());
                CHECK(r[1] == p21.uri());
            }
        });

        triggerScriptRun();
    }

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript({
            .code = "return openspace.property('base*.p1')",
            .callback = [&p1, &p21](ghoul::Dictionary d) {
                REQUIRE(d.size() == 1);
                REQUIRE(d.hasKey("1"));
                REQUIRE(d.hasValue<ghoul::Dictionary>("1"));
                ghoul::Dictionary e = d.value<ghoul::Dictionary>("1");
                REQUIRE(e.size() == 2);

                // Sorting the results as the property return order is undefined
                std::array<std::string, 2> r;
                {
                    REQUIRE(e.hasKey("1"));
                    REQUIRE(e.hasValue<std::string>("1"));
                    const std::string v = e.value<std::string>("1");
                    r[0] = v;
                }
                {
                    REQUIRE(e.hasKey("2"));
                    REQUIRE(e.hasValue<std::string>("2"));
                    const std::string v = e.value<std::string>("2");
                    r[1] = v;
                }
                std::sort(r.begin(), r.end());
                CHECK(r[0] == p1.uri());
                CHECK(r[1] == p21.uri());
            }
        });

        triggerScriptRun();
    }
}
