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

#include <catch2/catch_test_macros.hpp>

#include <openspace/scripting/scriptscheduler.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/dictionary.h>
#include <limits>

TEST_CASE("ScriptScheduler: Simple Forward", "[scriptscheduler]") {
    using namespace openspace;
    using namespace openspace::scripting;

    SpiceManager::initialize();
    SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );


    ScriptScheduler scheduler;

    std::vector<ScriptScheduler::ScheduledScript> scripts;
    {
        ScriptScheduler::ScheduledScript script;
        script.time = Time::convertTime("2000 JAN 03");
        script.forwardScript = "ForwardScript1";
        script.backwardScript = "BackwardScript1";
        scripts.push_back(script);
    }

    scheduler.progressTo(Time::convertTime("2000 JAN 01"));
    scheduler.loadScripts(scripts);

    std::vector<std::string> res = scheduler.progressTo(Time::convertTime("2000 JAN 02"));
    CHECK(res.empty());

    res = scheduler.progressTo(Time::convertTime("2000 JAN 03"));
    REQUIRE(res.size() == 1);
    CHECK(res[0] == "ForwardScript1");

    SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Multiple Forward Single Jump", "[scriptscheduler]") {
    using namespace openspace;
    using namespace openspace::scripting;

    SpiceManager::initialize();
    SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    std::vector<ScriptScheduler::ScheduledScript> scripts;
    {
        ScriptScheduler::ScheduledScript script1;
        script1.time = Time::convertTime("2000 JAN 03");
        script1.forwardScript = "ForwardScript1";
        script1.backwardScript = "BackwardScript1";
        scripts.push_back(script1);

        ScriptScheduler::ScheduledScript script2;
        script2.time = Time::convertTime("2000 JAN 05");
        script2.forwardScript = "ForwardScript2";
        script2.backwardScript = "BackwardScript2";
        scripts.push_back(script2);
    }
    ScriptScheduler scheduler;

    scheduler.progressTo(Time::convertTime("2000 JAN 01"));
    scheduler.loadScripts(scripts);

    std::vector<std::string> res = scheduler.progressTo(Time::convertTime("2000 JAN 02"));
    CHECK(res.empty());

    res = scheduler.progressTo(Time::convertTime("2000 JAN 04"));
    REQUIRE(res.size() == 1);
    CHECK(res[0] == "ForwardScript1");

    res = scheduler.progressTo(Time::convertTime("2000 JAN 06"));
    REQUIRE(res.size() == 1);
    CHECK(res[0] == "ForwardScript2");

    SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Multiple Forward Ordering", "[scriptscheduler]") {
    using namespace openspace;
    using namespace openspace::scripting;
    using namespace std::string_literals;

    SpiceManager::initialize();
    SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );


    std::vector<ScriptScheduler::ScheduledScript> scripts;
    {
        ScriptScheduler::ScheduledScript script1;
        script1.time = Time::convertTime("2000 JAN 03");
        script1.forwardScript = "ForwardScript1";
        script1.backwardScript = "BackwardScript1";
        scripts.push_back(script1);

        ScriptScheduler::ScheduledScript script2;
        script2.time = Time::convertTime("2000 JAN 05");
        script2.forwardScript = "ForwardScript2";
        script2.backwardScript = "BackwardScript2";
        scripts.push_back(script2);
    }

    ScriptScheduler scheduler;
    scheduler.progressTo(Time::convertTime("2000 JAN 01"));
    scheduler.loadScripts(scripts);

    std::vector<std::string> res = scheduler.progressTo(Time::convertTime("2000 JAN 02"));
    CHECK(res.empty());

    res = scheduler.progressTo(Time::convertTime("2000 JAN 06"));
    REQUIRE(res.size() == 2);
    CHECK(res[0] == "ForwardScript1");
    CHECK(res[1] == "ForwardScript2");

    SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Simple Backward", "[scriptscheduler]") {
    using namespace openspace;
    using namespace openspace::scripting;
    using namespace std::string_literals;

    SpiceManager::initialize();
    SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    std::vector<ScriptScheduler::ScheduledScript> scripts;
    {
        ScriptScheduler::ScheduledScript script1;
        script1.time = Time::convertTime("2000 JAN 03");
        script1.forwardScript = "ForwardScript1";
        script1.backwardScript = "BackwardScript1";
        scripts.push_back(script1);
    }

    ScriptScheduler scheduler;
    scheduler.progressTo(Time::convertTime("2000 JAN 05"));
    scheduler.loadScripts(scripts);

    std::vector<std::string> res = scheduler.progressTo(Time::convertTime("2000 JAN 04"));
    CHECK(res.empty());

    res = scheduler.progressTo(Time::convertTime("2000 JAN 02"));
    REQUIRE(res.size() == 1);
    CHECK(res[0] == "BackwardScript1");

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Multiple Backward Single Jump", "[scriptscheduler]") {
    using namespace openspace;
    using namespace openspace::scripting;
    using namespace std::string_literals;

    SpiceManager::initialize();
    SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );


    std::vector<ScriptScheduler::ScheduledScript> scripts;
    {
        ScriptScheduler::ScheduledScript script1;
        script1.time = Time::convertTime("2000 JAN 03");
        script1.forwardScript = "ForwardScript1";
        script1.backwardScript = "BackwardScript1";
        scripts.push_back(script1);

        ScriptScheduler::ScheduledScript script2;
        script2.time = Time::convertTime("2000 JAN 05");
        script2.forwardScript = "ForwardScript2";
        script2.backwardScript = "BackwardScript2";
        scripts.push_back(script2);
    }

    ScriptScheduler scheduler;
    scheduler.progressTo(Time::convertTime("2000 JAN 07"));
    scheduler.loadScripts(scripts);

    std::vector<std::string> res = scheduler.progressTo(Time::convertTime("2000 JAN 06"));
    CHECK(res.empty());

    res = scheduler.progressTo(Time::convertTime("2000 JAN 04"));
    REQUIRE(res.size() == 1);
    CHECK(res[0] == "BackwardScript2");

    res = scheduler.progressTo(Time::convertTime("2000 JAN 01"));
    REQUIRE(res.size() == 1);
    CHECK(res[0] == "BackwardScript1");

    SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Multiple Backward Ordering", "[scriptscheduler]") {
    using namespace openspace;
    using namespace openspace::scripting;
    using namespace std::string_literals;

    SpiceManager::initialize();
    SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    std::vector<ScriptScheduler::ScheduledScript> scripts;
    {
        ScriptScheduler::ScheduledScript script1;
        script1.time = Time::convertTime("2000 JAN 03");
        script1.forwardScript = "ForwardScript1";
        script1.backwardScript = "BackwardScript1";
        scripts.push_back(script1);

        ScriptScheduler::ScheduledScript script2;
        script2.time = Time::convertTime("2000 JAN 05");
        script2.forwardScript = "ForwardScript2";
        script2.backwardScript = "BackwardScript2";
        scripts.push_back(script2);
    }

    ScriptScheduler scheduler;
    scheduler.progressTo(Time::convertTime("2000 JAN 07"));
    scheduler.loadScripts(scripts);

    std::vector<std::string> res = scheduler.progressTo(Time::convertTime("2000 JAN 06"));
    CHECK(res.empty());

    res = scheduler.progressTo(Time::convertTime("2000 JAN 01"));
    REQUIRE(res.size() == 2);
    CHECK(res[0] == "BackwardScript2");
    CHECK(res[1] == "BackwardScript1");

    SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Empty", "[scriptscheduler]") {
    using namespace openspace;
    using namespace openspace::scripting;

    SpiceManager::initialize();
    SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    static const std::vector<double> TestTimes = {
        0.0, 1.0, -1.0, std::numeric_limits<double>::min(),
        -std::numeric_limits<double>::max(), std::numeric_limits<double>::max()
    };

    // First test if a new ScriptScheduler will return an empty list
    for (const double t : TestTimes) {
        ScriptScheduler scheduler;
        const std::vector<std::string> res = scheduler.progressTo(t);
        CHECK(res.empty());
    }

    // Then test the same thing but keeping the same ScriptScheduler
    ScriptScheduler scheduler;
    for (const double t : TestTimes) {
        const std::vector<std::string> res = scheduler.progressTo(t);
        CHECK(res.empty());
    }

    SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Forward Backwards", "[scriptscheduler]") {
    using namespace openspace;
    using namespace openspace::scripting;
    using namespace std::string_literals;

    SpiceManager::initialize();
    SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    std::vector<ScriptScheduler::ScheduledScript> scripts;
    {
        ScriptScheduler::ScheduledScript script1;
        script1.time = Time::convertTime("2000 JAN 03");
        script1.forwardScript = "ForwardScript1";
        script1.backwardScript = "BackwardScript1";
        scripts.push_back(script1);

        ScriptScheduler::ScheduledScript script2;
        script2.time = Time::convertTime("2000 JAN 05");
        script2.forwardScript = "ForwardScript2";
        script2.backwardScript = "BackwardScript2";
        scripts.push_back(script2);
    }

    ScriptScheduler scheduler;
    scheduler.progressTo(Time::convertTime("2000 JAN 01"));
    scheduler.loadScripts(scripts);

    std::vector<std::string> res = scheduler.progressTo(Time::convertTime("2000 JAN 04"));
    REQUIRE(res.size() == 1);
    CHECK(res[0] == "ForwardScript1");

    res = scheduler.progressTo(Time::convertTime("2000 JAN 01"));
    REQUIRE(res.size() == 1);
    CHECK(res[0] == "BackwardScript1");

    res = scheduler.progressTo(Time::convertTime("2000 JAN 07"));
    REQUIRE(res.size() == 2);

    res = scheduler.progressTo(Time::convertTime("2000 JAN 04"));
    REQUIRE(res.size() == 1);
    CHECK(res[0] == "BackwardScript2");

    SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Rewind", "[scriptscheduler]") {
    using namespace openspace;
    using namespace openspace::scripting;
    using namespace std::string_literals;

    SpiceManager::initialize();
    SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    std::vector<ScriptScheduler::ScheduledScript> scripts;
    {
        ScriptScheduler::ScheduledScript script1;
        script1.time = Time::convertTime("2000 JAN 03");
        script1.forwardScript = "ForwardScript1";
        script1.backwardScript = "BackwardScript1";
        scripts.push_back(script1);

        ScriptScheduler::ScheduledScript script2;
        script2.time = Time::convertTime("2000 JAN 05");
        script2.forwardScript = "ForwardScript2";
        script2.backwardScript = "BackwardScript2";
        scripts.push_back(script2);
    }

    ScriptScheduler scheduler;
    scheduler.progressTo(Time::convertTime("2000 JAN 01"));
    scheduler.loadScripts(scripts);

    std::vector<std::string> res = scheduler.progressTo(Time::convertTime("2000 JAN 07"));
    REQUIRE(res.size() == 2);

    scheduler.rewind();

    res = scheduler.progressTo(Time::convertTime("2000 JAN 04"));
    REQUIRE(res.size() == 1);
    CHECK(res[0] == "ForwardScript1");

    SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: CurrentTime", "[scriptscheduler]") {
    using namespace openspace;
    using namespace openspace::scripting;

    SpiceManager::initialize();
    SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    static const std::vector<double> TestValues = {
        0.0, 1.0, 42.0, std::numeric_limits<double>::min(),
        -std::numeric_limits<double>::max(), std::numeric_limits<double>::max()
    };

    for (const double t : TestValues) {
        ScriptScheduler scheduler;
        scheduler.progressTo(t);
        CHECK(t == scheduler.currentTime());
    }

    SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: All Scripts", "[scriptscheduler]") {
    using namespace openspace;
    using namespace openspace::scripting;
    using namespace std::string_literals;

    SpiceManager::initialize();
    SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    std::vector<ScriptScheduler::ScheduledScript> scripts;
    {
        ScriptScheduler::ScheduledScript script1;
        script1.time = Time::convertTime("2000 JAN 03");
        script1.forwardScript = "ForwardScript1";
        script1.backwardScript = "BackwardScript1";
        scripts.push_back(script1);

        ScriptScheduler::ScheduledScript script2;
        script2.time = Time::convertTime("2000 JAN 05");
        script2.forwardScript = "ForwardScript2";
        script2.backwardScript = "BackwardScript2";
        scripts.push_back(script2);

        ScriptScheduler::ScheduledScript script3;
        script3.time = Time::convertTime("2000 JAN 10");
        script3.forwardScript = "ForwardScript3";
        script3.backwardScript = "BackwardScript3";
        scripts.push_back(script3);
    }

    ScriptScheduler scheduler;
    scheduler.loadScripts(scripts);

    std::vector<ScriptScheduler::ScheduledScript> allScripts = scheduler.allScripts();
    REQUIRE(allScripts.size() == 3);

    CHECK(allScripts[0].time < allScripts[1].time);
    CHECK(allScripts[1].time < allScripts[2].time);

    SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Jump Equal", "[scriptscheduler]") {
    using namespace openspace;
    using namespace openspace::scripting;
    using namespace std::string_literals;

    SpiceManager::initialize();
    SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    std::vector<ScriptScheduler::ScheduledScript> scripts;
    {
        ScriptScheduler::ScheduledScript script1;
        script1.time = Time::convertTime("2000 JAN 03 12:00:00");
        script1.forwardScript = "ForwardScript1";
        script1.backwardScript = "BackwardScript1";
        scripts.push_back(script1);
    }

    ScriptScheduler scheduler;
    scheduler.loadScripts(scripts);

    std::vector<std::string> res =
        scheduler.progressTo(Time::convertTime("2000 JAN 03 11:00:00"));
    CHECK(res.empty());

    res = scheduler.progressTo(Time::convertTime("2000 JAN 03 12:00:00"));
    REQUIRE(res.size() == 1);
    CHECK(res[0] == "ForwardScript1");

    res = scheduler.progressTo(Time::convertTime("2000 JAN 03 12:01:00"));
    REQUIRE(res.empty());

    res = scheduler.progressTo(Time::convertTime("2000 JAN 03 12:00:00"));
    REQUIRE(res.size() == 1);
    CHECK(res[0] == "BackwardScript1");

    SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Same Time", "[scriptscheduler]") {
    using namespace openspace;
    using namespace openspace::scripting;
    using namespace std::string_literals;

    SpiceManager::initialize();
    SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    std::vector<ScriptScheduler::ScheduledScript> scripts;
    {
        ScriptScheduler::ScheduledScript script1;
        script1.time = Time::convertTime("2000 JAN 03 12:00:00");
        script1.forwardScript = "ForwardScript1";
        script1.backwardScript = "BackwardScript1";
        scripts.push_back(script1);
    }

    ScriptScheduler scheduler;
    scheduler.loadScripts(scripts);

    std::vector<std::string> res =
        scheduler.progressTo(Time::convertTime("2000 JAN 03 12:00:00"));
    REQUIRE(res.size() == 1);
    CHECK(res[0] == "ForwardScript1");

    res = scheduler.progressTo(Time::convertTime("2000 JAN 03 12:00:00"));
    CHECK(res.empty());

    SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Multi Inner Jump", "[scriptscheduler]") {
    using namespace openspace;
    using namespace openspace::scripting;
    using namespace std::string_literals;

    SpiceManager::initialize();
    SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    std::vector<ScriptScheduler::ScheduledScript> scripts;
    {
        ScriptScheduler::ScheduledScript script1;
        script1.time = Time::convertTime("2000 JAN 03 12:00:00");
        script1.forwardScript = "ForwardScript1";
        script1.backwardScript = "BackwardScript1";
        scripts.push_back(script1);
    }

    ScriptScheduler scheduler;
    scheduler.loadScripts(scripts);

    std::vector<std::string> res =
        scheduler.progressTo(Time::convertTime("2000 JAN 03 10:00:00"));
    CHECK(res.empty());

    res = scheduler.progressTo(Time::convertTime("2000 JAN 03 11:00:00"));
    CHECK(res.empty());

    res = scheduler.progressTo(Time::convertTime("2000 JAN 03 13:00:00"));
    REQUIRE(res.size() == 1);
    CHECK(res[0] == "ForwardScript1");

    res = scheduler.progressTo(Time::convertTime("2000 JAN 03 12:30:00"));
    CHECK(res.empty());

    SpiceManager::deinitialize();
}

TEST_CASE(
    "ScriptScheduler: Multiple Forward Single Jump Multiple Load",
    "[scriptscheduler]")
{
    using namespace openspace;
    using namespace openspace::scripting;
    using namespace std::string_literals;

    SpiceManager::initialize();
    SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    ScriptScheduler::ScheduledScript script1;
    script1.time = Time::convertTime("2000 JAN 03");
    script1.forwardScript = "ForwardScript1";
    script1.backwardScript = "BackwardScript1";

    ScriptScheduler::ScheduledScript script2;
    script2.time = Time::convertTime("2000 JAN 05");
    script2.forwardScript = "ForwardScript2";
    script2.backwardScript = "BackwardScript2";


    ScriptScheduler scheduler;
    scheduler.progressTo(Time::convertTime("2000 JAN 01"));
    scheduler.loadScripts({ script1 });
    scheduler.loadScripts({ script2 });

    std::vector<std::string> res = scheduler.progressTo(Time::convertTime("2000 JAN 02"));
    CHECK(res.empty());

    res = scheduler.progressTo(Time::convertTime("2000 JAN 04"));
    REQUIRE(res.size() == 1);
    CHECK(res[0] == "ForwardScript1");

    res = scheduler.progressTo(Time::convertTime("2000 JAN 06"));
    REQUIRE(res.size() == 1);
    CHECK(res[0] == "ForwardScript2");

    SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Multiple Forward Ordering Multiple Load" "[scriptscheduler]")
{
    using namespace openspace;
    using namespace openspace::scripting;
    using namespace std::string_literals;

    SpiceManager::initialize();
    SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    ScriptScheduler::ScheduledScript script1;
    script1.time = Time::convertTime("2000 JAN 03");
    script1.forwardScript = "ForwardScript1";
    script1.backwardScript = "BackwardScript1";

    ScriptScheduler::ScheduledScript script2;
    script2.time = Time::convertTime("2000 JAN 05");
    script2.forwardScript = "ForwardScript2";
    script2.backwardScript = "BackwardScript2";

    ScriptScheduler scheduler;
    scheduler.progressTo(Time::convertTime("2000 JAN 01"));
    scheduler.loadScripts({ script1 });
    scheduler.loadScripts({ script2 });

    std::vector<std::string> res = scheduler.progressTo(Time::convertTime("2000 JAN 02"));
    CHECK(res.empty());

    res = scheduler.progressTo(Time::convertTime("2000 JAN 06"));
    REQUIRE(res.size() == 2);
    CHECK(res[0] == "ForwardScript1");
    CHECK(res[1] == "ForwardScript2");

    SpiceManager::deinitialize();
}

TEST_CASE(
    "ScriptScheduler: Multiple Backward Single Jump Multiple Load",
    "[scriptscheduler]")
{
    using namespace openspace;
    using namespace openspace::scripting;
    using namespace std::string_literals;

    SpiceManager::initialize();
    SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    ScriptScheduler::ScheduledScript script1;
    script1.time = Time::convertTime("2000 JAN 03");
    script1.forwardScript = "ForwardScript1";
    script1.backwardScript = "BackwardScript1";

    ScriptScheduler::ScheduledScript script2;
    script2.time = Time::convertTime("2000 JAN 05");
    script2.forwardScript = "ForwardScript2";
    script2.backwardScript = "BackwardScript2";

    ScriptScheduler scheduler;
    scheduler.progressTo(Time::convertTime("2000 JAN 07"));
    scheduler.loadScripts({ script1 });
    scheduler.loadScripts({ script2 });

    std::vector<std::string> res = scheduler.progressTo(Time::convertTime("2000 JAN 06"));
    CHECK(res.empty());

    res = scheduler.progressTo(Time::convertTime("2000 JAN 04"));
    REQUIRE(res.size() == 1);
    CHECK(res[0] == "BackwardScript2");

    res = scheduler.progressTo(Time::convertTime("2000 JAN 01"));
    REQUIRE(res.size() == 1);
    CHECK(res[0] == "BackwardScript1");

    SpiceManager::deinitialize();
}

TEST_CASE(
    "ScriptScheduler: Multiple Backward Ordering Multiple Load",
    "[scriptscheduler]")
{
    using namespace openspace;
    using namespace openspace::scripting;
    using namespace std::string_literals;

    SpiceManager::initialize();
    SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    ScriptScheduler scheduler;

    ScriptScheduler::ScheduledScript script1;
    script1.time = Time::convertTime("2000 JAN 03");
    script1.forwardScript = "ForwardScript1";
    script1.backwardScript = "BackwardScript1";

    ScriptScheduler::ScheduledScript script2;
    script2.time = Time::convertTime("2000 JAN 05");
    script2.forwardScript = "ForwardScript2";
    script2.backwardScript = "BackwardScript2";

    scheduler.progressTo(Time::convertTime("2000 JAN 07"));
    scheduler.loadScripts({ script1 });
    scheduler.loadScripts({ script2 });

    std::vector<std::string> res = scheduler.progressTo(Time::convertTime("2000 JAN 06"));
    CHECK(res.empty());

    res = scheduler.progressTo(Time::convertTime("2000 JAN 01"));
    REQUIRE(res.size() == 2);
    CHECK(res[0] == "BackwardScript2");
    CHECK(res[1] == "BackwardScript1");

    SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Forward Backwards Multiple Load", "[scriptscheduler]") {
    using namespace openspace;
    using namespace openspace::scripting;
    using namespace std::string_literals;

    SpiceManager::initialize();
    SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    ScriptScheduler::ScheduledScript script1;
    script1.time = Time::convertTime("2000 JAN 03");
    script1.forwardScript = "ForwardScript1";
    script1.backwardScript = "BackwardScript1";

    ScriptScheduler::ScheduledScript script2;
    script2.time = Time::convertTime("2000 JAN 05");
    script2.forwardScript = "ForwardScript2";
    script2.backwardScript = "BackwardScript2";

    ScriptScheduler scheduler;
    scheduler.progressTo(Time::convertTime("2000 JAN 01"));
    scheduler.loadScripts({ script1 });
    scheduler.loadScripts({ script2 });

    std::vector<std::string> res = scheduler.progressTo(Time::convertTime("2000 JAN 04"));
    REQUIRE(res.size() == 1);
    CHECK(res[0] == "ForwardScript1");

    res = scheduler.progressTo(Time::convertTime("2000 JAN 01"));
    REQUIRE(res.size() == 1);
    CHECK(res[0] == "BackwardScript1");

    res = scheduler.progressTo(Time::convertTime("2000 JAN 07"));
    REQUIRE(res.size() == 2);

    res = scheduler.progressTo(Time::convertTime("2000 JAN 04"));
    REQUIRE(res.size() == 1);
    CHECK(res[0] == "BackwardScript2");

    SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Rewind Multiple Load", "[scriptscheduler]") {
    using namespace openspace;
    using namespace openspace::scripting;
    using namespace std::string_literals;

    SpiceManager::initialize();
    SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    ScriptScheduler::ScheduledScript script1;
    script1.time = Time::convertTime("2000 JAN 03");
    script1.forwardScript = "ForwardScript1";
    script1.backwardScript = "BackwardScript1";

    ScriptScheduler::ScheduledScript script2;
    script2.time = Time::convertTime("2000 JAN 05");
    script2.forwardScript = "ForwardScript2";
    script2.backwardScript = "BackwardScript2";

    ScriptScheduler scheduler;
    scheduler.progressTo(Time::convertTime("2000 JAN 01"));
    scheduler.loadScripts({ script1 });
    scheduler.loadScripts({ script2 });

    std::vector<std::string> res = scheduler.progressTo(Time::convertTime("2000 JAN 07"));
    REQUIRE(res.size() == 2);

    scheduler.rewind();

    res = scheduler.progressTo(Time::convertTime("2000 JAN 04"));
    REQUIRE(res.size() == 1);
    CHECK(res[0] == "ForwardScript1");

    SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: All Scripts Multiple Load", "[scriptscheduler]") {
    using namespace openspace;
    using namespace openspace::scripting;
    using namespace std::string_literals;

    SpiceManager::initialize();
    SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    ScriptScheduler::ScheduledScript script1;
    script1.time = Time::convertTime("2000 JAN 03");
    script1.forwardScript = "ForwardScript1";
    script1.backwardScript = "BackwardScript1";

    ScriptScheduler::ScheduledScript script2;
    script2.time = Time::convertTime("2000 JAN 05");
    script2.forwardScript = "ForwardScript2";
    script2.backwardScript = "BackwardScript2";

    ScriptScheduler::ScheduledScript script3;
    script3.time = Time::convertTime("2000 JAN 10");
    script3.forwardScript = "ForwardScript3";
    script3.backwardScript = "BackwardScript3";

    ScriptScheduler scheduler;
    scheduler.loadScripts({ script1 });
    scheduler.loadScripts({ script2 });
    scheduler.loadScripts({ script3 });

    std::vector<ScriptScheduler::ScheduledScript> allScripts = scheduler.allScripts();
    REQUIRE(allScripts.size() == 3);

    CHECK(allScripts[0].time < allScripts[1].time);
    CHECK(allScripts[1].time < allScripts[2].time);

    SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: All Scripts Mixed Load", "[scriptscheduler]") {
    using namespace openspace;
    using namespace openspace::scripting;
    using namespace std::string_literals;

    SpiceManager::initialize();
    SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    ScriptScheduler::ScheduledScript script1;
    script1.time = Time::convertTime("2000 JAN 03");
    script1.forwardScript = "ForwardScript1";
    script1.backwardScript = "BackwardScript1";

    ScriptScheduler::ScheduledScript script2;
    script2.time = Time::convertTime("2000 JAN 05");
    script2.forwardScript = "ForwardScript2";
    script2.backwardScript = "BackwardScript2";

    ScriptScheduler::ScheduledScript script3;
    script3.time = Time::convertTime("2000 JAN 10");
    script3.forwardScript = "ForwardScript3";
    script3.backwardScript = "BackwardScript3";

    ScriptScheduler scheduler;
    scheduler.loadScripts({ script1 });
    scheduler.loadScripts({ script2, script3 });

    std::vector<ScriptScheduler::ScheduledScript> allScripts = scheduler.allScripts();
    REQUIRE(allScripts.size() == 3);

    CHECK(allScripts[0].time < allScripts[1].time);
    CHECK(allScripts[1].time < allScripts[2].time);

    SpiceManager::deinitialize();
}
