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

    // Updates any ongoing interpolations with an optional time delay
    void updateInterpolations(std::optional<std::chrono::milliseconds> ms = std::nullopt)
    {
        if (ms.has_value()) {
            std::this_thread::sleep_for(*ms);
        }
        global::renderEngine->scene()->updateInterpolations();
    }
} // namespace

TEST_CASE("SetPropertyValueSingle: Basic", "[setpropertyvalue]") {
    PropertyOwner owner = PropertyOwner({ "base" });
    global::rootPropertyOwner->addPropertySubOwner(owner);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner.addProperty(p1);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValueSingle('base.p1', 2.0)"
        );

        CHECK(p1 == 1.f);
        triggerScriptRun();
        CHECK(p1 == 2.f);
    }
}

TEST_CASE("SetPropertyValueSingle: Wrong Type", "[setpropertyvalue]") {
    PropertyOwner owner = PropertyOwner({ "base" });
    global::rootPropertyOwner->addPropertySubOwner(owner);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner.addProperty(p1);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValueSingle('base.p1', 'abc')"
        );
        triggerScriptRun();
        int errorCounter = LogMgr.messageCounter(ghoul::logging::LogLevel::Error);
        CHECK(errorCounter == 1);
    }
}

TEST_CASE("SetPropertyValueSingle: Non-existing", "[setpropertyvalue]") {
    PropertyOwner owner = PropertyOwner({ "base" });
    global::rootPropertyOwner->addPropertySubOwner(owner);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner.addProperty(p1);
    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValueSingle('base.p2', 1.0)"
        );
        triggerScriptRun();
        int errorCounter = LogMgr.messageCounter(ghoul::logging::LogLevel::Error);
        CHECK(errorCounter == 1);
    }
}

TEST_CASE("SetPropertyValueSingle: Interpolation", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner = PropertyOwner({ "base" });
    global::rootPropertyOwner->addPropertySubOwner(owner);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner.addProperty(p1);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValueSingle('base.p1', 2.0, 1.0)"
        );
        defer { scene->removePropertyInterpolation(&p1); };

        CHECK(p1 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double v = glm::mix(1.0, 2.0, 0.1);
        CHECK(p1 > 1.f);
        CHECK_THAT(p1, Catch::Matchers::WithinAbs(v, 0.05));
    }
}

TEST_CASE("SetPropertyValueSingle: Easing", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner = PropertyOwner({ "base" });
    global::rootPropertyOwner->addPropertySubOwner(owner);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner.addProperty(p1);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValueSingle('base.p1', 2.0, 1.0, 'ExponentialEaseOut')"
        );
        defer { scene->removePropertyInterpolation(&p1); };

        CHECK(p1 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        CHECK(p1 > 1.f);
        const double t = ghoul::exponentialEaseOut(0.1);
        const double v = glm::mix(1.0, 2.0, t);
        CHECK_THAT(p1, Catch::Matchers::WithinAbs(v, 0.075));
    }
}

TEST_CASE("SetPropertyValueSingle: PostScript", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

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

        global::scriptEngine->queueScript(R"(
            openspace.setPropertyValueSingle(
                'base.p1',
                2.0,
                0.1,
                'ExponentialEaseOut',
                [[openspace.setPropertyValueSingle('base.p2', 0.75)]]
            )
        )");
        defer { scene->removePropertyInterpolation(&p1); };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(150));
        triggerScriptRun();
        CHECK(p1 == 2.f);
        CHECK(p2 == 0.75f);
    }
}

TEST_CASE("SetPropertyValue: Basic", "[setpropertyvalue]") {
    PropertyOwner owner = PropertyOwner({ "base" });
    global::rootPropertyOwner->addPropertySubOwner(owner);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner.addProperty(p1);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('base.p1', 2.0)"
        );

        CHECK(p1 == 1.f);
        triggerScriptRun();
        CHECK(p1 == 2.f);
    }
}

TEST_CASE("SetPropertyValue: Wrong Type", "[setpropertyvalue]") {
    PropertyOwner owner = PropertyOwner({ "base" });
    global::rootPropertyOwner->addPropertySubOwner(owner);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner.addProperty(p1);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('base.p1', 'abc')"
        );
        triggerScriptRun();
        int errorCounter = LogMgr.messageCounter(ghoul::logging::LogLevel::Error);
        CHECK(errorCounter == 2);
    }
}

TEST_CASE("SetPropertyValue: Non-existing", "[setpropertyvalue]") {
    PropertyOwner owner = PropertyOwner({ "base" });
    global::rootPropertyOwner->addPropertySubOwner(owner);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner.addProperty(p1);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('base.p2', 1.0)"
        );
        triggerScriptRun();
        int errorCounter = LogMgr.messageCounter(ghoul::logging::LogLevel::Error);
        CHECK(errorCounter == 1);
    }
}

TEST_CASE("SetPropertyValue: Interpolation", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner = PropertyOwner({ "base" });
    global::rootPropertyOwner->addPropertySubOwner(owner);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner.addProperty(p1);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('base.p1', 2.0, 1.0)"
        );
        defer { scene->removePropertyInterpolation(&p1); };

        CHECK(p1 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double v = glm::mix(1.0, 2.0, 0.1);
        CHECK(p1 > 1.f);
        CHECK_THAT(p1, Catch::Matchers::WithinAbs(v, 0.05));
    }
}

TEST_CASE("SetPropertyValue: Easing", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner = PropertyOwner({ "base" });
    global::rootPropertyOwner->addPropertySubOwner(owner);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner.addProperty(p1);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('base.p1', 2.0, 1.0, 'ExponentialEaseOut')"
        );
        defer { scene->removePropertyInterpolation(&p1); };

        CHECK(p1 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        CHECK(p1 > 1.f);
        const double t = ghoul::exponentialEaseOut(0.1);
        const double v = glm::mix(1.0, 2.0, t);
        CHECK_THAT(p1, Catch::Matchers::WithinAbs(v, 0.075));
    }
}

TEST_CASE("SetPropertyValue: PostScript", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

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

        global::scriptEngine->queueScript(R"(
            openspace.setPropertyValue(
                'base.p1',
                2.0,
                0.1,
                'ExponentialEaseOut',
                [[openspace.setPropertyValue('base.p2', 0.75)]]
            )
        )");
        defer { scene->removePropertyInterpolation(&p1); };


        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(150));
        triggerScriptRun();
        CHECK(p1 == 2.f);
        CHECK(p2 == 0.75f);
    }
}

TEST_CASE("SetPropertyValue: Wildcard Basic", "[setpropertyvalue]") {
    PropertyOwner owner1 = PropertyOwner({ "base1" });
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript("openspace.setPropertyValue('base1.*', 2.0)");

        CHECK(p1 == 1.f);
        CHECK(p21 == 1.f);
        triggerScriptRun();
        CHECK(p1 == 2.f);
        CHECK(p21 == 1.f);
    }

    p1 = 1.f;
    p21 = 1.f;

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript("openspace.setPropertyValue('*.p1', 2.0)");

        CHECK(p1 == 1.f);
        CHECK(p21 == 1.f);
        triggerScriptRun();
        CHECK(p1 == 2.f);
        CHECK(p21 == 2.f);
    }
}

TEST_CASE("SetPropertyValue: Wildcard Basic Multiple", "[setpropertyvalue]") {
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

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript("openspace.setPropertyValue('base1.*', 2.0)");

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        triggerScriptRun();
        CHECK(p1 == 2.f);
        CHECK(p2 == 2.f);
        CHECK(p21 == 1.f);
    }

    p1 = 1.f;
    p2 = 1.f;
    p21 = 1.f;

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript("openspace.setPropertyValue('*.p1', 2.0)");

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        triggerScriptRun();
        CHECK(p1 == 2.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 2.f);
    }
    
    p1 = 1.f;
    p2 = 1.f;
    p21 = 1.f;

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript("openspace.setPropertyValue('base*.p1', 2.0)");

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        triggerScriptRun();
        CHECK(p1 == 2.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 2.f);
    }
}

TEST_CASE("SetPropertyValue: Wildcard Basic Multiple/2", "[setpropertyvalue]") {
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

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('base1.p*', 2.0)"
        );

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        triggerScriptRun();
        CHECK(p1 == 2.f);
        CHECK(p2 == 2.f);
        CHECK(p21 == 1.f);
    }
}

TEST_CASE("SetPropertyValue: Wildcard Interpolation", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner1 = PropertyOwner({ "base1" });
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('base1.*', 2.0, 1.0)"
        );
        defer { scene->removePropertyInterpolation(&p1); };

        CHECK(p1 == 1.f);
        CHECK(p21 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double v = glm::mix(1.0, 2.0, 0.1);
        CHECK(p1 > 1.f);
        CHECK_THAT(p1, Catch::Matchers::WithinAbs(v, 0.05));
        CHECK(p21 == 1.f);
    }
}

TEST_CASE("SetPropertyValue: Wildcard Interpolation Multiple", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

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

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('base1.*', 2.0, 1.0)"
        );
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p2);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double v = glm::mix(1.0, 2.0, 0.1);
        CHECK(p1 > 1.f);
        CHECK_THAT(p1, Catch::Matchers::WithinAbs(v, 0.05));
        CHECK(p2 > 1.f);
        CHECK_THAT(p2, Catch::Matchers::WithinAbs(v, 0.05));
        CHECK(p21 == 1.f);
    }

    p1 = 1.f;
    p2 = 1.f;
    p21 = 1.f;

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('*.p1', 2.0, 1.0)"
        );
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p2);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double v = glm::mix(1.0, 2.0, 0.1);
        CHECK(p1 > 1.f);
        CHECK_THAT(p1, Catch::Matchers::WithinAbs(v, 0.05));
        CHECK(p2 == 1.f);
        CHECK(p21 > 1.f);
        CHECK_THAT(p21, Catch::Matchers::WithinAbs(v, 0.05));
    }
    
    p1 = 1.f;
    p2 = 1.f;
    p21 = 1.f;

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('base*.p1', 2.0, 1.0)"
        );
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p2);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double v = glm::mix(1.0, 2.0, 0.1);
        CHECK(p1 > 1.f);
        CHECK_THAT(p1, Catch::Matchers::WithinAbs(v, 0.05));
        CHECK(p2 == 1.f);
        CHECK(p21 > 1.f);
        CHECK_THAT(p21, Catch::Matchers::WithinAbs(v, 0.05));
    }
}

TEST_CASE("SetPropertyValue: Wildcard Interpolation Multiple/2", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

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

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('base1.p*', 2.0, 1.0)"
        );
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p2);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double v = glm::mix(1.0, 2.0, 0.1);
        CHECK(p1 > 1.f);
        CHECK_THAT(p1, Catch::Matchers::WithinAbs(v, 0.05));
        CHECK(p2 > 1.f);
        CHECK_THAT(p2, Catch::Matchers::WithinAbs(v, 0.05));
        CHECK(p21 == 1.f);
    }
}

TEST_CASE("SetPropertyValue: Wildcard Easing Multiple", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

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

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('base1.*', 2.0, 1.0, 'ExponentialEaseOut')"
        );
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p2);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double t = ghoul::exponentialEaseOut(0.1);
        const double v = glm::mix(1.0, 2.0, t);
        CHECK(p1 > 1.f);
        CHECK_THAT(p1, Catch::Matchers::WithinAbs(v, 0.075));
        CHECK(p2 > 1.f);
        CHECK_THAT(p2, Catch::Matchers::WithinAbs(v, 0.075));
        CHECK(p21 == 1.f);
    }

    p1 = 1.f;
    p2 = 1.f;
    p21 = 1.f;

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('*.p1', 2.0, 1.0, 'ExponentialEaseOut')"
        );
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p2);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double t = ghoul::exponentialEaseOut(0.1);
        const double v = glm::mix(1.0, 2.0, t);
        CHECK(p1 > 1.f);
        CHECK_THAT(p1, Catch::Matchers::WithinAbs(v, 0.075));
        CHECK(p2 == 1.f);
        CHECK(p21 > 1.f);
        CHECK_THAT(p21, Catch::Matchers::WithinAbs(v, 0.075));
    }
    
    p1 = 1.f;
    p2 = 1.f;
    p21 = 1.f;

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('base*.p1', 2.0, 1.0, 'ExponentialEaseOut')"
        );
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p2);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double t = ghoul::exponentialEaseOut(0.1);
        const double v = glm::mix(1.0, 2.0, t);
        CHECK(p1 > 1.f);
        CHECK_THAT(p1, Catch::Matchers::WithinAbs(v, 0.075));
        CHECK(p2 == 1.f);
        CHECK(p21 > 1.f);
        CHECK_THAT(p21, Catch::Matchers::WithinAbs(v, 0.075));
    }
}

TEST_CASE("SetPropertyValue: Wildcard Easing Multiple/2", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

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

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('base1.p*', 2.0, 1.0, 'ExponentialEaseOut')"
        );
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p2);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double t = ghoul::exponentialEaseOut(0.1);
        const double v = glm::mix(1.0, 2.0, t);
        CHECK(p1 > 1.f);
        CHECK_THAT(p1, Catch::Matchers::WithinAbs(v, 0.075));
        CHECK(p2 > 1.f);
        CHECK_THAT(p2, Catch::Matchers::WithinAbs(v, 0.075));
        CHECK(p21 == 1.f);
    }
}

TEST_CASE("SetPropertyValue: Wildcard PostScript Multiple", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner1 = PropertyOwner({ "base1" });
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);
    FloatProperty p2 = FloatProperty(Property::PropertyInfo("p2", "a", "b"), 1.f);
    owner1.addProperty(p2);
    FloatProperty q1 = FloatProperty(Property::PropertyInfo("q1", "a", "b"), 1.f);
    owner1.addProperty(q1);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(R"(
            openspace.setPropertyValue(
                'base1.*',
                2.0,
                0.1,
                'ExponentialEaseOut',
                [[openspace.setPropertyValue('base1.q1', 0.75)]]
            )
        )");
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p2);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(q1 == 1.f);
        CHECK(p21 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(150));
        triggerScriptRun();
        CHECK(p1 == 2.f);
        CHECK(p2 == 2.f);
        CHECK(q1 == 0.75f);
        CHECK(p21 == 1.f);
    }

    p1 = 1.f;
    p2 = 1.f;
    q1 = 1.f;
    p21 = 1.f;

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(R"(
            openspace.setPropertyValue(
                '*.p1',
                2.0,
                0.1,
                'ExponentialEaseOut',
                [[openspace.setPropertyValue('base1.q1', 0.75)]]
            )
        )");
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p2);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(q1 == 1.f);
        CHECK(p21 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(150));
        triggerScriptRun();
        CHECK(p1 == 2.f);
        CHECK(p2 == 1.f);
        CHECK(q1 == 0.75f);
        CHECK(p21 == 2.f);
    }
    
    p1 = 1.f;
    p2 = 1.f;
    q1 = 1.f;
    p21 = 1.f;

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(R"(
            openspace.setPropertyValue(
                'base*.p1',
                2.0,
                0.1,
                'ExponentialEaseOut',
                [[openspace.setPropertyValue('base1.q1', 0.75)]]
            )
        )");
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p2);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(q1 == 1.f);
        CHECK(p21 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(150));
        triggerScriptRun();
        CHECK(p1 == 2.f);
        CHECK(p2 == 1.f);
        CHECK(q1 == 0.75f);
        CHECK(p21 == 2.f);
    }
}

TEST_CASE("SetPropertyValue: Wildcard PostScript Multiple/2", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner1 = PropertyOwner({ "base1" });
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);
    FloatProperty p2 = FloatProperty(Property::PropertyInfo("p2", "a", "b"), 1.f);
    owner1.addProperty(p2);
    FloatProperty q1 = FloatProperty(Property::PropertyInfo("q1", "a", "b"), 1.f);
    owner1.addProperty(q1);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(R"(
            openspace.setPropertyValue(
                'base1.p*',
                2.0,
                0.1,
                'ExponentialEaseOut',
                [[openspace.setPropertyValue('base1.q1', 0.75)]]
            )
        )");
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p2);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(q1 == 1.f);
        CHECK(p21 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(150));
        triggerScriptRun();
        CHECK(p1 == 2.f);
        CHECK(p2 == 2.f);
        CHECK(q1 == 0.75f);
        CHECK(p21 == 1.f);
    }
}

TEST_CASE("SetPropertyValue: Tags Basic", "[setpropertyvalue]") {
    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript("openspace.setPropertyValue('{tag1}.p1', 2.0)");

        CHECK(p1 == 1.f);
        CHECK(p21 == 1.f);
        triggerScriptRun();
        CHECK(p1 == 2.f);
        CHECK(p21 == 1.f);
    }
}

TEST_CASE("SetPropertyValue: Tags Basic Multiple", "[setpropertyvalue]") {
    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);
    FloatProperty p2 = FloatProperty(Property::PropertyInfo("p2", "a", "b"), 1.f);
    owner1.addProperty(p2);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);

    PropertyOwner owner3 = PropertyOwner({ "base3" });
    owner3.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner3);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner3); };
    FloatProperty p31 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner3.addProperty(p31);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript("openspace.setPropertyValue('{tag1}.p1', 2.0)");

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
        triggerScriptRun();
        CHECK(p1 == 2.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 2.f);
    }
}

TEST_CASE("SetPropertyValue: Tags Interpolation", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('{tag1}.p1', 2.0, 1.0)"
        );
        defer { scene->removePropertyInterpolation(&p1); };

        CHECK(p1 == 1.f);
        CHECK(p21 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double v = glm::mix(1.0, 2.0, 0.1);
        CHECK(p1 > 1.f);
        CHECK_THAT(p1, Catch::Matchers::WithinAbs(v, 0.05));
        CHECK(p21 == 1.f);
    }
}

TEST_CASE("SetPropertyValue: Tags Interpolation Multiple", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);
    FloatProperty p2 = FloatProperty(Property::PropertyInfo("p2", "a", "b"), 1.f);
    owner1.addProperty(p2);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);

    PropertyOwner owner3 = PropertyOwner({ "base3" });
    owner3.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner3);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner3); };
    FloatProperty p31 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner3.addProperty(p31);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('{tag1}.p1', 2.0, 1.0)"
        );
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p31);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double v = glm::mix(1.0, 2.0, 0.1);
        CHECK(p1 > 1.f);
        CHECK_THAT(p1, Catch::Matchers::WithinAbs(v, 0.05));
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 > 1.f);
        CHECK_THAT(p31, Catch::Matchers::WithinAbs(v, 0.05));
    }
}

TEST_CASE("SetPropertyValue: Tags Easing Multiple", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);
    FloatProperty p2 = FloatProperty(Property::PropertyInfo("p2", "a", "b"), 1.f);
    owner1.addProperty(p2);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);

    PropertyOwner owner3 = PropertyOwner({ "base3" });
    owner3.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner3);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner3); };
    FloatProperty p31 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner3.addProperty(p31);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('{tag1}.p1', 2.0, 1.0, 'ExponentialEaseOut')"
        );
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p31);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double t = ghoul::exponentialEaseOut(0.1);
        const double v = glm::mix(1.0, 2.0, t);
        CHECK(p1 > 1.f);
        CHECK_THAT(p1, Catch::Matchers::WithinAbs(v, 0.075));
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 > 1.f);
        CHECK_THAT(p31, Catch::Matchers::WithinAbs(v, 0.075));
    }
}

TEST_CASE("SetPropertyValue: Tags PostScript Multiple", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);
    FloatProperty p2 = FloatProperty(Property::PropertyInfo("p2", "a", "b"), 1.f);
    owner1.addProperty(p2);
    FloatProperty q1 = FloatProperty(Property::PropertyInfo("q1", "a", "b"), 1.f);
    owner1.addProperty(q1);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);

    PropertyOwner owner3 = PropertyOwner({ "base3" });
    owner3.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner3);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner3); };
    FloatProperty p31 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner3.addProperty(p31);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(R"(
            openspace.setPropertyValue(
                '{tag1}.p1',
                2.0,
                0.1,
                'ExponentialEaseOut',
                [[openspace.setPropertyValue('base1.q1', 0.75)]]
            )
        )");
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p31);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(q1 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(150));
        triggerScriptRun();
        CHECK(p1 == 2.f);
        CHECK(p2 == 1.f);
        CHECK(q1 == 0.75f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 2.f);
    }
}

TEST_CASE("SetPropertyValue: Tags Intersection Basic", "[setpropertyvalue]") {
    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag1");
    owner2.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);
    
    PropertyOwner owner3 = PropertyOwner({ "base3" });
    owner3.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner3);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner3); };
    FloatProperty p31 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner3.addProperty(p31);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('{tag1&tag2}.p1', 2.0)"
        );

        CHECK(p1 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
        triggerScriptRun();
        CHECK(p1 == 1.f);
        CHECK(p21 == 2.f);
        CHECK(p31 == 1.f);
    }
}

TEST_CASE("SetPropertyValue: Tags Intersection Basic Multiple", "[setpropertyvalue]") {
    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);
    FloatProperty p2 = FloatProperty(Property::PropertyInfo("p2", "a", "b"), 1.f);
    owner1.addProperty(p2);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag1");
    owner2.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);

    PropertyOwner owner3 = PropertyOwner({ "base3" });
    owner3.addTag("tag1");
    owner3.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner3);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner3); };
    FloatProperty p31 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner3.addProperty(p31);
    
    PropertyOwner owner4 = PropertyOwner({ "base4" });
    owner4.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner4);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner4); };
    FloatProperty p41 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner4.addProperty(p41);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('{tag1&tag2}.p1', 2.0)"
        );

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
        CHECK(p41 == 1.f);
        triggerScriptRun();
        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 2.f);
        CHECK(p31 == 2.f);
        CHECK(p41 == 1.f);
    }
}

TEST_CASE("SetPropertyValue: Tags Intersection Interpolation", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag1");
    owner2.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);
    
    PropertyOwner owner3 = PropertyOwner({ "base3" });
    owner3.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner3);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner3); };
    FloatProperty p31 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner3.addProperty(p31);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('{tag1&tag2}.p1', 2.0, 1.0)"
        );
        defer { scene->removePropertyInterpolation(&p1); };

        CHECK(p1 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double v = glm::mix(1.0, 2.0, 0.1);
        CHECK(p1 == 1.f);
        CHECK(p21 > 1.f);
        CHECK_THAT(p21, Catch::Matchers::WithinAbs(v, 0.05));
        CHECK(p31 == 1.f);
    }
}

TEST_CASE(
    "SetPropertyValue: Tags Intersection Interpolation Multiple",
    "[setpropertyvalue]"
)
{
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);
    FloatProperty p2 = FloatProperty(Property::PropertyInfo("p2", "a", "b"), 1.f);
    owner1.addProperty(p2);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag1");
    owner2.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);

    PropertyOwner owner3 = PropertyOwner({ "base3" });
    owner3.addTag("tag1");
    owner3.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner3);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner3); };
    FloatProperty p31 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner3.addProperty(p31);
    
    PropertyOwner owner4 = PropertyOwner({ "base4" });
    owner4.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner4);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner4); };
    FloatProperty p41 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner4.addProperty(p41);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('{tag1&tag2}.p1', 2.0, 1.0)"
        );
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p31);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
        CHECK(p41 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double v = glm::mix(1.0, 2.0, 0.1);
        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 > 1.f);
        CHECK_THAT(p21, Catch::Matchers::WithinAbs(v, 0.05));
        CHECK(p31 > 1.f);
        CHECK_THAT(p31, Catch::Matchers::WithinAbs(v, 0.05));
        CHECK(p41 == 1.f);
    }
}

TEST_CASE("SetPropertyValue: Tags Intersection Easing Multiple", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);
    FloatProperty p2 = FloatProperty(Property::PropertyInfo("p2", "a", "b"), 1.f);
    owner1.addProperty(p2);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag1");
    owner2.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);

    PropertyOwner owner3 = PropertyOwner({ "base3" });
    owner3.addTag("tag1");
    owner3.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner3);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner3); };
    FloatProperty p31 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner3.addProperty(p31);
    
    PropertyOwner owner4 = PropertyOwner({ "base4" });
    owner4.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner4);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner4); };
    FloatProperty p41 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner4.addProperty(p41);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('{tag1&tag2}.p1', 2.0, 1.0, 'ExponentialEaseOut')"
        );
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p31);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
        CHECK(p41 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double t = ghoul::exponentialEaseOut(0.1);
        const double v = glm::mix(1.0, 2.0, t);
        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 > 1.f);
        CHECK_THAT(p21, Catch::Matchers::WithinAbs(v, 0.075));
        CHECK(p31 > 1.f);
        CHECK_THAT(p31, Catch::Matchers::WithinAbs(v, 0.075));
        CHECK(p41 == 1.f);
    }
}

TEST_CASE("SetPropertyValue: Tags Intersection PostScript Multiple", "[setpropertyvalue]")
{
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);
    FloatProperty p2 = FloatProperty(Property::PropertyInfo("p2", "a", "b"), 1.f);
    owner1.addProperty(p2);
    FloatProperty q1 = FloatProperty(Property::PropertyInfo("q1", "a", "b"), 1.f);
    owner1.addProperty(q1);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag1");
    owner2.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);

    PropertyOwner owner3 = PropertyOwner({ "base3" });
    owner3.addTag("tag1");
    owner3.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner3);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner3); };
    FloatProperty p31 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner3.addProperty(p31);
    
    PropertyOwner owner4 = PropertyOwner({ "base4" });
    owner4.addTag("tag1");
    owner4.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner4);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner4); };
    FloatProperty p41 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner4.addProperty(p41);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(R"(
            openspace.setPropertyValue(
                '{tag1&tag2}.p1',
                2.0,
                0.1,
                'ExponentialEaseOut',
                [[openspace.setPropertyValue('base1.q1', 0.75)]]
            )
        )");
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p31);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(q1 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
        CHECK(p41 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(150));
        triggerScriptRun();
        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(q1 == 0.75f);
        CHECK(p21 == 2.f);
        CHECK(p31 == 2.f);
        CHECK(p41 == 2.f);
    }
}

TEST_CASE("SetPropertyValue: Tags Union Basic", "[setpropertyvalue]") {
    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);
    
    PropertyOwner owner3 = PropertyOwner({ "base3" });
    owner3.addTag("tag1");
    owner3.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner3);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner3); };
    FloatProperty p31 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner3.addProperty(p31);
      
    PropertyOwner owner4 = PropertyOwner({ "base4" });
    owner4.addTag("tag3");
    global::rootPropertyOwner->addPropertySubOwner(owner4);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner4); };
    FloatProperty p41 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner4.addProperty(p41);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('{tag1|tag2}.p1', 2.0)"
        );

        CHECK(p1 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
        CHECK(p41 == 1.f);
        triggerScriptRun();
        CHECK(p1 == 2.f);
        CHECK(p21 == 2.f);
        CHECK(p31 == 2.f);
        CHECK(p41 == 1.f);
    }
}

TEST_CASE("SetPropertyValue: Tags Union Basic Multiple", "[setpropertyvalue]") {
    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);
    FloatProperty p2 = FloatProperty(Property::PropertyInfo("p2", "a", "b"), 1.f);
    owner1.addProperty(p2);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag1");
    owner2.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);

    PropertyOwner owner3 = PropertyOwner({ "base3" });
    owner3.addTag("tag1");
    owner3.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner3);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner3); };
    FloatProperty p31 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner3.addProperty(p31);
    
    PropertyOwner owner4 = PropertyOwner({ "base4" });
    owner4.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner4);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner4); };
    FloatProperty p41 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner4.addProperty(p41);
    
    PropertyOwner owner5 = PropertyOwner({ "base5" });
    owner5.addTag("tag3");
    global::rootPropertyOwner->addPropertySubOwner(owner5);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner5); };
    FloatProperty p51 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner5.addProperty(p51);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('{tag1|tag2}.p1', 2.0)"
        );

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
        CHECK(p41 == 1.f);
        CHECK(p51 == 1.f);
        triggerScriptRun();
        CHECK(p1 == 2.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 2.f);
        CHECK(p31 == 2.f);
        CHECK(p41 == 2.f);
        CHECK(p51 == 1.f);
    }
}

TEST_CASE("SetPropertyValue: Tags Union Interpolation", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag1");
    owner2.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);
    
    PropertyOwner owner3 = PropertyOwner({ "base3" });
    owner3.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner3);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner3); };
    FloatProperty p31 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner3.addProperty(p31);
    
    PropertyOwner owner4 = PropertyOwner({ "base4" });
    owner4.addTag("tag3");
    global::rootPropertyOwner->addPropertySubOwner(owner4);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner4); };
    FloatProperty p41 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner4.addProperty(p41);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('{tag1|tag2}.p1', 2.0, 1.0)"
        );
        defer { scene->removePropertyInterpolation(&p1); };

        CHECK(p1 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
        CHECK(p41 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double v = glm::mix(1.0, 2.0, 0.1);
        CHECK(p1 > 1.f);
        CHECK_THAT(p1, Catch::Matchers::WithinAbs(v, 0.05));
        CHECK(p21 > 1.f);
        CHECK_THAT(p21, Catch::Matchers::WithinAbs(v, 0.05));
        CHECK(p31 > 1.f);
        CHECK_THAT(p31, Catch::Matchers::WithinAbs(v, 0.05));
        CHECK(p41 == 1.f);
    }
}

TEST_CASE("SetPropertyValue: Tags Union Interpolation Multiple", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);
    FloatProperty p2 = FloatProperty(Property::PropertyInfo("p2", "a", "b"), 1.f);
    owner1.addProperty(p2);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag1");
    owner2.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);

    PropertyOwner owner3 = PropertyOwner({ "base3" });
    owner3.addTag("tag1");
    owner3.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner3);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner3); };
    FloatProperty p31 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner3.addProperty(p31);
    
    PropertyOwner owner4 = PropertyOwner({ "base4" });
    owner4.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner4);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner4); };
    FloatProperty p41 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner4.addProperty(p41);
    
    PropertyOwner owner5 = PropertyOwner({ "base5" });
    owner5.addTag("tag3");
    global::rootPropertyOwner->addPropertySubOwner(owner5);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner5); };
    FloatProperty p51 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner5.addProperty(p51);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('{tag1|tag2}.p1', 2.0, 1.0)"
        );
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p31);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
        CHECK(p41 == 1.f);
        CHECK(p51 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double v = glm::mix(1.0, 2.0, 0.1);
        CHECK(p1 > 1.f);
        CHECK_THAT(p1, Catch::Matchers::WithinAbs(v, 0.05));
        CHECK(p2 == 1.f);
        CHECK(p21 > 1.f);
        CHECK_THAT(p21, Catch::Matchers::WithinAbs(v, 0.05));
        CHECK(p31 > 1.f);
        CHECK_THAT(p31, Catch::Matchers::WithinAbs(v, 0.05));
        CHECK(p41 > 1.f);
        CHECK_THAT(p41, Catch::Matchers::WithinAbs(v, 0.05));
        CHECK(p51 == 1.f);
    }
}

TEST_CASE("SetPropertyValue: Tags Union Easing Multiple", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);
    FloatProperty p2 = FloatProperty(Property::PropertyInfo("p2", "a", "b"), 1.f);
    owner1.addProperty(p2);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag1");
    owner2.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);

    PropertyOwner owner3 = PropertyOwner({ "base3" });
    owner3.addTag("tag1");
    owner3.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner3);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner3); };
    FloatProperty p31 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner3.addProperty(p31);
    
    PropertyOwner owner4 = PropertyOwner({ "base4" });
    owner4.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner4);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner4); };
    FloatProperty p41 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner4.addProperty(p41);
    
    PropertyOwner owner5 = PropertyOwner({ "base5" });
    owner5.addTag("tag3");
    global::rootPropertyOwner->addPropertySubOwner(owner5);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner5); };
    FloatProperty p51 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner5.addProperty(p51);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('{tag1|tag2}.p1', 2.0, 1.0, 'ExponentialEaseOut')"
        );
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p31);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
        CHECK(p41 == 1.f);
        CHECK(p51 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double t = ghoul::exponentialEaseOut(0.1);
        const double v = glm::mix(1.0, 2.0, t);
        CHECK(p1 > 1.f);
        CHECK_THAT(p1, Catch::Matchers::WithinAbs(v, 0.075));
        CHECK(p2 == 1.f);
        CHECK(p21 > 1.f);
        CHECK_THAT(p21, Catch::Matchers::WithinAbs(v, 0.075));
        CHECK(p31 > 1.f);
        CHECK_THAT(p31, Catch::Matchers::WithinAbs(v, 0.075));
        CHECK(p41 > 1.f);
        CHECK_THAT(p41, Catch::Matchers::WithinAbs(v, 0.075));
        CHECK(p51 == 1.f);
    }
}

TEST_CASE("SetPropertyValue: Tags Union PostScript Multiple", "[setpropertyvalue]")
{
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);
    FloatProperty p2 = FloatProperty(Property::PropertyInfo("p2", "a", "b"), 1.f);
    owner1.addProperty(p2);
    FloatProperty q1 = FloatProperty(Property::PropertyInfo("q1", "a", "b"), 1.f);
    owner1.addProperty(q1);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag1");
    owner2.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);

    PropertyOwner owner3 = PropertyOwner({ "base3" });
    owner3.addTag("tag1");
    owner3.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner3);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner3); };
    FloatProperty p31 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner3.addProperty(p31);
    
    PropertyOwner owner4 = PropertyOwner({ "base4" });
    owner4.addTag("tag3");
    global::rootPropertyOwner->addPropertySubOwner(owner4);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner4); };
    FloatProperty p41 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner4.addProperty(p41);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(R"(
            openspace.setPropertyValue(
                '{tag1|tag2}.p1',
                2.0,
                0.1,
                'ExponentialEaseOut',
                [[openspace.setPropertyValue('base1.q1', 0.75)]]
            )
        )");
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p31);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(q1 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
        CHECK(p41 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(150));
        triggerScriptRun();
        CHECK(p1 == 2.f);
        CHECK(p2 == 1.f);
        CHECK(q1 == 0.75f);
        CHECK(p21 == 2.f);
        CHECK(p31 == 2.f);
        CHECK(p41 == 1.f);
    }
}

TEST_CASE("SetPropertyValue: Tags Negation Basic", "[setpropertyvalue]") {
    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);
    
    PropertyOwner owner3 = PropertyOwner({ "base3" });
    owner3.addTag("tag1");
    owner3.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner3);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner3); };
    FloatProperty p31 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner3.addProperty(p31);
      
    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('{tag1~tag2}.p1', 2.0)"
        );

        CHECK(p1 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
        triggerScriptRun();
        CHECK(p1 == 2.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
    }
}

TEST_CASE("SetPropertyValue: Tags Negation Basic Multiple", "[setpropertyvalue]") {
    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);
    FloatProperty p2 = FloatProperty(Property::PropertyInfo("p2", "a", "b"), 1.f);
    owner1.addProperty(p2);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);

    PropertyOwner owner3 = PropertyOwner({ "base3" });
    owner3.addTag("tag1");
    owner3.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner3);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner3); };
    FloatProperty p31 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner3.addProperty(p31);
    
    PropertyOwner owner4 = PropertyOwner({ "base4" });
    owner4.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner4);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner4); };
    FloatProperty p41 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner4.addProperty(p41);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('{tag1~tag2}.p1', 2.0)"
        );

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
        CHECK(p41 == 1.f);
        triggerScriptRun();
        CHECK(p1 == 2.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 2.f);
        CHECK(p31 == 1.f);
        CHECK(p41 == 1.f);
    }
}

TEST_CASE("SetPropertyValue: Tags Negation Interpolation", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag1");
    owner2.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);
    
    PropertyOwner owner3 = PropertyOwner({ "base3" });
    owner3.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner3);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner3); };
    FloatProperty p31 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner3.addProperty(p31);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('{tag1~tag2}.p1', 2.0, 1.0)"
        );
        defer { scene->removePropertyInterpolation(&p1); };

        CHECK(p1 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double v = glm::mix(1.0, 2.0, 0.1);
        CHECK(p1 > 1.f);
        CHECK_THAT(p1, Catch::Matchers::WithinAbs(v, 0.05));
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
    }
}

TEST_CASE("SetPropertyValue: Tags Negation Interpolation Multiple", "[setpropertyvalue]")
{
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);
    FloatProperty p2 = FloatProperty(Property::PropertyInfo("p2", "a", "b"), 1.f);
    owner1.addProperty(p2);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);

    PropertyOwner owner3 = PropertyOwner({ "base3" });
    owner3.addTag("tag1");
    owner3.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner3);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner3); };
    FloatProperty p31 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner3.addProperty(p31);
    
    PropertyOwner owner4 = PropertyOwner({ "base4" });
    owner4.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner4);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner4); };
    FloatProperty p41 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner4.addProperty(p41);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('{tag1~tag2}.p1', 2.0, 1.0)"
        );
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p31);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
        CHECK(p41 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double v = glm::mix(1.0, 2.0, 0.1);
        CHECK(p1 > 1.f);
        CHECK_THAT(p1, Catch::Matchers::WithinAbs(v, 0.05));
        CHECK(p2 == 1.f);
        CHECK(p21 > 1.f);
        CHECK_THAT(p21, Catch::Matchers::WithinAbs(v, 0.05));
        CHECK(p31 == 1.f);
        CHECK(p41 == 1.f);
    }
}

TEST_CASE("SetPropertyValue: Tags Negation Easing Multiple", "[setpropertyvalue]") {
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);
    FloatProperty p2 = FloatProperty(Property::PropertyInfo("p2", "a", "b"), 1.f);
    owner1.addProperty(p2);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);

    PropertyOwner owner3 = PropertyOwner({ "base3" });
    owner3.addTag("tag1");
    owner3.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner3);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner3); };
    FloatProperty p31 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner3.addProperty(p31);
    
    PropertyOwner owner4 = PropertyOwner({ "base4" });
    owner4.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner4);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner4); };
    FloatProperty p41 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner4.addProperty(p41);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('{tag1~tag2}.p1', 2.0, 1.0, 'ExponentialEaseOut')"
        );
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p31);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
        CHECK(p41 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(100));
        const double t = ghoul::exponentialEaseOut(0.1);
        const double v = glm::mix(1.0, 2.0, t);
        CHECK(p1 > 1.f);
        CHECK_THAT(p1, Catch::Matchers::WithinAbs(v, 0.075));
        CHECK(p2 == 1.f);
        CHECK(p21 > 1.f);
        CHECK_THAT(p21, Catch::Matchers::WithinAbs(v, 0.075));
        CHECK(p31 == 1.f);
        CHECK(p41 == 1.f);
    }
}

TEST_CASE("SetPropertyValue: Tags Negation PostScript Multiple", "[setpropertyvalue]")
{
    std::unique_ptr<Scene> scene = std::make_unique<Scene>(
        std::make_unique<SceneInitializer>()
    );
    global::renderEngine->setScene(scene.get());
    defer { global::renderEngine->setScene(nullptr); };

    PropertyOwner owner1 = PropertyOwner({ "base1" });
    owner1.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner1);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner1); };
    FloatProperty p1 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner1.addProperty(p1);
    FloatProperty p2 = FloatProperty(Property::PropertyInfo("p2", "a", "b"), 1.f);
    owner1.addProperty(p2);
    FloatProperty q1 = FloatProperty(Property::PropertyInfo("q1", "a", "b"), 1.f);
    owner1.addProperty(q1);

    PropertyOwner owner2 = PropertyOwner({ "base2" });
    owner2.addTag("tag1");
    global::rootPropertyOwner->addPropertySubOwner(owner2);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner2); };
    FloatProperty p21 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner2.addProperty(p21);

    PropertyOwner owner3 = PropertyOwner({ "base3" });
    owner3.addTag("tag1");
    owner3.addTag("tag2");
    global::rootPropertyOwner->addPropertySubOwner(owner3);
    defer { global::rootPropertyOwner->removePropertySubOwner(owner3); };
    FloatProperty p31 = FloatProperty(Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner3.addProperty(p31);

    {
        LogMgr.resetMessageCounters();
        defer { LogMgr.resetMessageCounters(); };

        global::scriptEngine->queueScript(R"(
            openspace.setPropertyValue(
                '{tag1~tag2}.p1',
                2.0,
                0.1,
                'ExponentialEaseOut',
                [[openspace.setPropertyValue('base1.q1', 0.75)]]
            )
        )");
        defer {
            scene->removePropertyInterpolation(&p1);
            scene->removePropertyInterpolation(&p31);
        };

        CHECK(p1 == 1.f);
        CHECK(p2 == 1.f);
        CHECK(q1 == 1.f);
        CHECK(p21 == 1.f);
        CHECK(p31 == 1.f);
        triggerScriptRun();
        updateInterpolations(std::chrono::milliseconds(150));
        triggerScriptRun();
        CHECK(p1 == 2.f);
        CHECK(p2 == 1.f);
        CHECK(q1 == 0.75f);
        CHECK(p21 == 2.f);
        CHECK(p31 == 1.f);
    }
}
