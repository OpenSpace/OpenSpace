/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include "catch2/catch.hpp"

#include <openspace/scripting/scriptscheduler.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/dictionary.h>
#include <limits>

TEST_CASE("ScriptScheduler: Simple Forward", "[scriptscheduler]") {
    openspace::SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    using namespace openspace::scripting;
    using namespace std::string_literals;
    
    ScriptScheduler scheduler;

    ghoul::Dictionary testDictionary = {
        { "Time", "2000 JAN 03"s },
        { "ForwardScript", "ForwardScript1"s },
        { "BackwardScript", "BackwardScript1"s }
    };
    
    scheduler.progressTo(openspace::Time::convertTime("2000 JAN 01"));
    scheduler.loadScripts({ { "1", testDictionary } });
    
    auto res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 02"));
    REQUIRE(res.first == res.second);
    
    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 03"));
    REQUIRE(std::distance(res.first, res.second) == 1);
    REQUIRE(*(res.first) == "ForwardScript1");

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Multiple Forward Single Jump", "[scriptscheduler]") {
    openspace::SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    using namespace openspace::scripting;
    using namespace std::string_literals;

    ghoul::Dictionary testDictionary1 = {
        { "Time", "2000 JAN 03"s },
        { "ForwardScript", "ForwardScript1"s },
        { "BackwardScript", "BackwardScript1"s }
    };
    
    ghoul::Dictionary testDictionary2 = {
        { "Time", "2000 JAN 05"s },
        { "ForwardScript", "ForwardScript2"s },
        { "BackwardScript", "BackwardScript2"s }

    };
    
    ScriptScheduler scheduler;

    scheduler.progressTo(openspace::Time::convertTime("2000 JAN 01"));
    scheduler.loadScripts({
        { "1", testDictionary1 },
        { "2", testDictionary2 }
    });

    auto res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 02"));
    REQUIRE(res.first == res.second);
    
    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 04"));
    REQUIRE(std::distance(res.first, res.second) == 1);
    REQUIRE(*(res.first) == "ForwardScript1");
    
    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 06"));
    REQUIRE(std::distance(res.first, res.second) == 1);
    REQUIRE(*(res.first) == "ForwardScript2");

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Multiple Forward Ordering", "[scriptscheduler]") {
    openspace::SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    using namespace openspace::scripting;
    using namespace std::string_literals;

    ghoul::Dictionary testDictionary1 = {
        { "Time", "2000 JAN 03"s },
        { "ForwardScript", "ForwardScript1"s },
        { "BackwardScript", "BackwardScript1"s }
    };
    
    ghoul::Dictionary testDictionary2 = {
        { "Time", "2000 JAN 05"s },
        { "ForwardScript", "ForwardScript2"s },
        { "BackwardScript", "BackwardScript2"s }
    };
    
    ScriptScheduler scheduler;

    scheduler.progressTo(openspace::Time::convertTime("2000 JAN 01"));
    scheduler.loadScripts({
        { "1", testDictionary1 },
        { "2", testDictionary2 }
    });
    
    auto res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 02"));
    REQUIRE(res.first == res.second);

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 06"));
    REQUIRE(std::distance(res.first, res.second) == 2);
    REQUIRE(*(res.first) == "ForwardScript1");
    REQUIRE(*(std::next(res.first)) == "ForwardScript2");

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Simple Backward", "[scriptscheduler]") {
    openspace::SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    using namespace openspace::scripting;
    using namespace std::string_literals;

    ScriptScheduler scheduler;
    
    ghoul::Dictionary testDictionary = {
        { "Time", "2000 JAN 03"s },
        { "ForwardScript", "ForwardScript1"s },
        { "BackwardScript", "BackwardScript1"s }
    };
    
    scheduler.progressTo(openspace::Time::convertTime("2000 JAN 05"));
    scheduler.loadScripts({
        { "1", testDictionary }
    });
    
    auto res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 04"));
    REQUIRE(res.first == res.second);

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 02"));
    REQUIRE(std::distance(res.first, res.second) == 1);
    REQUIRE(*(res.first) == "BackwardScript1");

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Multiple Backward Single Jump", "[scriptscheduler]") {
    openspace::SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    using namespace openspace::scripting;
    using namespace std::string_literals;

    ScriptScheduler scheduler;
    
    ghoul::Dictionary testDictionary1 = {
        { "Time", "2000 JAN 03"s },
        { "ForwardScript", "ForwardScript1"s },
        { "BackwardScript", "BackwardScript1"s }
    };
    
    ghoul::Dictionary testDictionary2 = {
        { "Time", "2000 JAN 05"s },
        { "ForwardScript", "ForwardScript2"s },
        { "BackwardScript", "BackwardScript2"s }
    };
    
    scheduler.progressTo(openspace::Time::convertTime("2000 JAN 07"));
    scheduler.loadScripts({
        { "1", testDictionary1 },
        { "2", testDictionary2 }
    });
    
    auto res =  scheduler.progressTo(openspace::Time::convertTime("2000 JAN 06"));
    REQUIRE(res.first == res.second);
    
    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 04"));
    REQUIRE(std::distance(res.first, res.second) == 1);
    REQUIRE(*(res.first) == "BackwardScript2");
    
    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 01"));
    REQUIRE(std::distance(res.first, res.second) == 1);
    REQUIRE(*(res.first) == "BackwardScript1");

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Multiple Backward Ordering", "[scriptscheduler]") {
    openspace::SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    using namespace openspace::scripting;
    using namespace std::string_literals;

    ScriptScheduler scheduler;
    
    ghoul::Dictionary testDictionary1 = {
        { "Time", "2000 JAN 03"s },
        { "ForwardScript", "ForwardScript1"s },
        { "BackwardScript", "BackwardScript1"s }
    };
    
    ghoul::Dictionary testDictionary2 = {
        { "Time", "2000 JAN 05"s },
        { "ForwardScript", "ForwardScript2"s },
        { "BackwardScript", "BackwardScript2"s }
    };
    
    scheduler.progressTo(openspace::Time::convertTime("2000 JAN 07"));
    scheduler.loadScripts({
        { "1", testDictionary1 },
        { "2", testDictionary2 }
    });
    
    auto res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 06"));
    REQUIRE(res.first == res.second);

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 01"));
    REQUIRE(std::distance(res.first, res.second) == 2);
    REQUIRE(*(res.first) == "BackwardScript2");
    REQUIRE(*(std::next(res.first)) == "BackwardScript1");

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Empty", "[scriptscheduler]") {
    openspace::SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    using namespace openspace::scripting;
    
    static const std::vector<double> TestTimes = {
        0.0, 1.0, -1.0, std::numeric_limits<double>::min(),
        -std::numeric_limits<double>::max(), std::numeric_limits<double>::max()
    };
    
    // First test if a new ScriptScheduler will return an empty list
    for (double t : TestTimes) {
        ScriptScheduler scheduler;
        auto res = scheduler.progressTo(t);
        REQUIRE(res.first == res.second);
    }
    
    // Then test the same thing but keeping the same ScriptScheduler
    ScriptScheduler scheduler;
    for (double t : TestTimes) {
        auto res = scheduler.progressTo(t);
        REQUIRE(res.first == res.second);
    }

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Forward Backwards", "[scriptscheduler]") {
    openspace::SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    using namespace openspace::scripting;
    using namespace std::string_literals;
    
    ScriptScheduler scheduler;
    
    ghoul::Dictionary testDictionary1 = {
        { "Time", "2000 JAN 03"s },
        { "ForwardScript", "ForwardScript1"s },
        { "BackwardScript", "BackwardScript1"s }
    };
    
    ghoul::Dictionary testDictionary2 = {
        { "Time", "2000 JAN 05"s },
        { "ForwardScript", "ForwardScript2"s },
        { "BackwardScript", "BackwardScript2"s }
    };
    
    scheduler.progressTo(openspace::Time::convertTime("2000 JAN 01"));
    scheduler.loadScripts({
        { "1", testDictionary1 },
        { "2", testDictionary2 }
    });
    
    auto res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 04"));
    REQUIRE(std::distance(res.first, res.second) == 1);
    REQUIRE(*(res.first) == "ForwardScript1");
    
    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 01"));
    REQUIRE(std::distance(res.first, res.second) == 1);
    REQUIRE(*(res.first) == "BackwardScript1");
    
    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 07"));
    REQUIRE(std::distance(res.first, res.second) == 2);
    
    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 04"));
    REQUIRE(std::distance(res.first, res.second) == 1);
    REQUIRE(*(res.first) == "BackwardScript2");

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Rewind", "[scriptscheduler]") {
    openspace::SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    using namespace openspace::scripting;
    using namespace std::string_literals;

    ScriptScheduler scheduler;
    
    ghoul::Dictionary testDictionary1 = {
        { "Time", "2000 JAN 03"s },
        { "ForwardScript", "ForwardScript1"s },
        { "BackwardScript", "BackwardScript1"s }
    };
    
    ghoul::Dictionary testDictionary2 = {
        { "Time", "2000 JAN 05"s },
        { "ForwardScript", "ForwardScript2"s },
        { "BackwardScript", "BackwardScript2"s }
    };
    
    scheduler.progressTo(openspace::Time::convertTime("2000 JAN 01"));
    scheduler.loadScripts({
        { "1", testDictionary1 },
        { "2", testDictionary2 }
    });
    
    auto res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 07"));
    REQUIRE(std::distance(res.first, res.second) == 2);
    
    scheduler.rewind();
    
    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 04"));
    REQUIRE(std::distance(res.first, res.second) == 1);
    REQUIRE(*(res.first) == "ForwardScript1");

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: CurrentTime", "[scriptscheduler]") {
    openspace::SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    using namespace openspace::scripting;
    
    static const std::vector<double> TestValues = {
        0.0, 1.0, 42.0, std::numeric_limits<double>::min(),
        -std::numeric_limits<double>::max(), std::numeric_limits<double>::max()
    };
    
    for (double t : TestValues) {
        ScriptScheduler scheduler;
        scheduler.progressTo(t);
        REQUIRE(t == scheduler.currentTime());
    }

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: All Scripts", "[scriptscheduler]") {
    openspace::SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    using namespace openspace::scripting;
    using namespace std::string_literals;
    
    ScriptScheduler scheduler;
    
    ghoul::Dictionary testDictionary1 = {
        { "Time", "2000 JAN 03"s },
        { "ForwardScript", "ForwardScript1"s },
        { "BackwardScript", "BackwardScript1"s }
    };
    
    ghoul::Dictionary testDictionary2 = {
        { "Time", "2000 JAN 05"s },
        { "ForwardScript", "ForwardScript2"s },
        { "BackwardScript", "BackwardScript2"s }
    };

    ghoul::Dictionary testDictionary3 = {
        { "Time", "2000 JAN 10"s },
        { "ForwardScript", "ForwardScript3"s },
        { "BackwardScript", "BackwardScript3"s }
    };
    
    scheduler.loadScripts({
        { "1", testDictionary1 },
        { "2", testDictionary2 },
        { "3", testDictionary3 }
    });

    auto allScripts = scheduler.allScripts();
    REQUIRE(allScripts.size() == 3);

    REQUIRE(allScripts[0].time < allScripts[1].time);
    REQUIRE(allScripts[1].time < allScripts[2].time);

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Jump Equal", "[scriptscheduler]") {
    openspace::SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    using namespace openspace::scripting;
    using namespace std::string_literals;

    ScriptScheduler scheduler;

    ghoul::Dictionary testDictionary1 = {
        { "Time", "2000 JAN 03 12:00:00"s },
        { "ForwardScript", "ForwardScript1"s },
        { "BackwardScript", "BackwardScript1"s }
    };

    scheduler.loadScripts({
        { "1", testDictionary1 }
    });

    auto res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 03 11:00:00"));
    REQUIRE(res.first == res.second);

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 03 12:00:00"));
    REQUIRE(std::distance(res.first, res.second) == 1);
    REQUIRE(*(res.first) == "ForwardScript1");

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 03 12:01:00"));
    REQUIRE(res.first == res.second);

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 03 12:00:00"));
    REQUIRE(std::distance(res.first, res.second) == 1);
    REQUIRE(*(res.first) == "BackwardScript1");

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Same Time", "[scriptscheduler]") {
    openspace::SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    using namespace openspace::scripting;
    using namespace std::string_literals;

    ScriptScheduler scheduler;

    ghoul::Dictionary testDictionary1 = {
        { "Time", "2000 JAN 03 12:00:00"s },
        { "ForwardScript", "ForwardScript1"s },
        { "BackwardScript", "BackwardScript1"s }
    };

    scheduler.loadScripts({ { "1", testDictionary1 } });

    auto res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 03 12:00:00"));
    REQUIRE(std::distance(res.first, res.second) == 1);
    REQUIRE(*(res.first) == "ForwardScript1");

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 03 12:00:00"));
    REQUIRE(res.first == res.second);

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Multi Inner Jump", "[scriptscheduler]") {
    openspace::SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    using namespace openspace::scripting;
    using namespace std::string_literals;

    ScriptScheduler scheduler;

    ghoul::Dictionary testDictionary1 = {
        { "Time", "2000 JAN 03 12:00:00"s },
        { "ForwardScript", "ForwardScript1"s },
        { "BackwardScript", "BackwardScript1"s }
    };

    scheduler.loadScripts({ { "1", testDictionary1 } });

    auto res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 03 10:00:00"));
    REQUIRE(res.first == res.second);

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 03 11:00:00"));
    REQUIRE(res.first == res.second);

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 03 13:00:00"));
    REQUIRE(std::distance(res.first, res.second) == 1);
    REQUIRE(*(res.first) == "ForwardScript1");

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 03 12:30:00"));
    REQUIRE(res.first == res.second);

    openspace::SpiceManager::deinitialize();
}

TEST_CASE(
    "ScriptScheduler: Multiple Forward Single Jump Multiple Load",
    "[scriptscheduler]")
{
    openspace::SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    using namespace openspace::scripting;
    using namespace std::string_literals;

    ghoul::Dictionary testDictionary1 = {
        { "Time", "2000 JAN 03"s },
        { "ForwardScript", "ForwardScript1"s },
        { "BackwardScript", "BackwardScript1"s }
    };

    ghoul::Dictionary testDictionary2 = {
        { "Time", "2000 JAN 05"s },
        { "ForwardScript", "ForwardScript2"s },
        { "BackwardScript", "BackwardScript2"s }

    };

    ScriptScheduler scheduler;

    scheduler.progressTo(openspace::Time::convertTime("2000 JAN 01"));
    scheduler.loadScripts({ { "1", testDictionary1 } });

    scheduler.loadScripts({ { "1", testDictionary2 } });

    auto res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 02"));
    REQUIRE(res.first == res.second);

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 04"));
    REQUIRE(std::distance(res.first, res.second) == 1);
    REQUIRE(*(res.first) == "ForwardScript1");

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 06"));
    REQUIRE(std::distance(res.first, res.second) == 1);
    REQUIRE(*(res.first) == "ForwardScript2");

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Multiple Forward Ordering Multiple Load" "[scriptscheduler]")
{
    openspace::SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    using namespace openspace::scripting;
    using namespace std::string_literals;

    ghoul::Dictionary testDictionary1 = {
        { "Time", "2000 JAN 03"s },
        { "ForwardScript", "ForwardScript1"s },
        { "BackwardScript", "BackwardScript1"s }
    };

    ghoul::Dictionary testDictionary2 = {
        { "Time", "2000 JAN 05"s },
        { "ForwardScript", "ForwardScript2"s },
        { "BackwardScript", "BackwardScript2"s }
    };

    ScriptScheduler scheduler;

    scheduler.progressTo(openspace::Time::convertTime("2000 JAN 01"));
    scheduler.loadScripts({ { "1", testDictionary1 } });
    scheduler.loadScripts({ { "1", testDictionary2 } });

    auto res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 02"));
    REQUIRE(res.first == res.second);

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 06"));
    REQUIRE(std::distance(res.first, res.second) == 2);
    REQUIRE(*(res.first) == "ForwardScript1");
    REQUIRE(*(std::next(res.first)) == "ForwardScript2");

    openspace::SpiceManager::deinitialize();
}

TEST_CASE(
    "ScriptScheduler: Multiple Backward Single Jump Multiple Load",
    "[scriptscheduler]")
{
    openspace::SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    using namespace openspace::scripting;
    using namespace std::string_literals;

    ScriptScheduler scheduler;

    ghoul::Dictionary testDictionary1 = {
        { "Time", "2000 JAN 03"s },
        { "ForwardScript", "ForwardScript1"s },
        { "BackwardScript", "BackwardScript1"s }
    };

    ghoul::Dictionary testDictionary2 = {
        { "Time", "2000 JAN 05"s },
        { "ForwardScript", "ForwardScript2"s },
        { "BackwardScript", "BackwardScript2"s }
    };

    scheduler.progressTo(openspace::Time::convertTime("2000 JAN 07"));
    scheduler.loadScripts({ { "1", testDictionary1 } });
    scheduler.loadScripts({ { "1", testDictionary2 } });

    auto res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 06"));
    REQUIRE(res.first == res.second);

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 04"));
    REQUIRE(std::distance(res.first, res.second) == 1);
    REQUIRE(*(res.first) == "BackwardScript2");

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 01"));
    REQUIRE(std::distance(res.first, res.second) == 1);
    REQUIRE(*(res.first) == "BackwardScript1");

    openspace::SpiceManager::deinitialize();
}

TEST_CASE(
    "ScriptScheduler: Multiple Backward Ordering Multiple Load",
    "[scriptscheduler]")
{
    openspace::SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    using namespace openspace::scripting;
    using namespace std::string_literals;

    ScriptScheduler scheduler;

    ghoul::Dictionary testDictionary1 = {
        { "Time", "2000 JAN 03"s },
        { "ForwardScript", "ForwardScript1"s },
        { "BackwardScript", "BackwardScript1"s }
    };

    ghoul::Dictionary testDictionary2 = {
        { "Time", "2000 JAN 05"s },
        { "ForwardScript", "ForwardScript2"s },
        { "BackwardScript", "BackwardScript2"s }
    };

    scheduler.progressTo(openspace::Time::convertTime("2000 JAN 07"));
    scheduler.loadScripts({ { "1", testDictionary1 } });
    scheduler.loadScripts({ { "1", testDictionary2 } });

    std::pair<ScriptScheduler::ScriptIt, ScriptScheduler::ScriptIt> res =
        scheduler.progressTo(openspace::Time::convertTime("2000 JAN 06"));
    REQUIRE(res.first == res.second);

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 01"));
    REQUIRE(std::distance(res.first, res.second) == 2);
    REQUIRE(*(res.first) == "BackwardScript2");
    REQUIRE(*(std::next(res.first)) == "BackwardScript1");

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Forward Backwards Multiple Load", "[scriptscheduler]") {
    openspace::SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    using namespace openspace::scripting;
    using namespace std::string_literals;

    ScriptScheduler scheduler;

    ghoul::Dictionary testDictionary1 = {
        { "Time", "2000 JAN 03"s },
        { "ForwardScript", "ForwardScript1"s },
        { "BackwardScript", "BackwardScript1"s }
    };

    ghoul::Dictionary testDictionary2 = {
        { "Time", "2000 JAN 05"s },
        { "ForwardScript", "ForwardScript2"s },
        { "BackwardScript", "BackwardScript2"s }
    };

    scheduler.progressTo(openspace::Time::convertTime("2000 JAN 01"));
    scheduler.loadScripts({ { "1", testDictionary1 } });
    scheduler.loadScripts({ { "1", testDictionary2 } });

    auto res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 04"));
    REQUIRE(std::distance(res.first, res.second) == 1);
    REQUIRE(*(res.first) == "ForwardScript1");

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 01"));
    REQUIRE(std::distance(res.first, res.second) == 1);
    REQUIRE(*(res.first) == "BackwardScript1");

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 07"));
    REQUIRE(std::distance(res.first, res.second) == 2);

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 04"));
    REQUIRE(std::distance(res.first, res.second) == 1);
    REQUIRE(*(res.first) == "BackwardScript2");

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: Rewind Multiple Load", "[scriptscheduler]") {
    openspace::SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    using namespace openspace::scripting;
    using namespace std::string_literals;

    ScriptScheduler scheduler;

    ghoul::Dictionary testDictionary1 = {
        { "Time", "2000 JAN 03"s },
        { "ForwardScript", "ForwardScript1"s },
        { "BackwardScript", "BackwardScript1"s }
    };

    ghoul::Dictionary testDictionary2 = {
        { "Time", "2000 JAN 05"s },
        { "ForwardScript", "ForwardScript2"s },
        { "BackwardScript", "BackwardScript2"s }
    };

    scheduler.progressTo(openspace::Time::convertTime("2000 JAN 01"));
    scheduler.loadScripts({ { "1", testDictionary1 } });
    scheduler.loadScripts({ { "1", testDictionary2 } });

    auto res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 07"));
    REQUIRE(std::distance(res.first, res.second) == 2);

    scheduler.rewind();

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 04"));
    REQUIRE(std::distance(res.first, res.second) == 1);
    REQUIRE(*(res.first) == "ForwardScript1");

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: All Scripts Multiple Load", "[scriptscheduler]") {
    openspace::SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    using namespace openspace::scripting;
    using namespace std::string_literals;

    ScriptScheduler scheduler;


    ghoul::Dictionary testDictionary1 = {
        { "Time", "2000 JAN 03"s },
        { "ForwardScript", "ForwardScript1"s },
        { "BackwardScript", "BackwardScript1"s }
    };

    ghoul::Dictionary testDictionary2 = {
        { "Time", "2000 JAN 05"s },
        { "ForwardScript", "ForwardScript2"s },
        { "BackwardScript", "BackwardScript2"s }
    };

    ghoul::Dictionary testDictionary3 = {
        { "Time", "2000 JAN 10"s },
        { "ForwardScript", "ForwardScript3"s },
        { "BackwardScript", "BackwardScript3"s }
    };

    scheduler.loadScripts({ { "1", testDictionary1 } });
    scheduler.loadScripts({ { "1", testDictionary2 } });
    scheduler.loadScripts({ { "1", testDictionary3 } });

    auto allScripts = scheduler.allScripts();
    REQUIRE(allScripts.size() == 3);

    REQUIRE(allScripts[0].time < allScripts[1].time);
    REQUIRE(allScripts[1].time < allScripts[2].time);

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("ScriptScheduler: All Scripts Mixed Load", "[scriptscheduler]") {
    openspace::SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    using namespace openspace::scripting;
    using namespace std::string_literals;

    ScriptScheduler scheduler;


    ghoul::Dictionary testDictionary1 = {
        { "Time", "2000 JAN 03"s },
        { "ForwardScript", "ForwardScript1"s },
        { "BackwardScript", "BackwardScript1"s }
    };

    ghoul::Dictionary testDictionary2 = {
        { "Time", "2000 JAN 05"s },
        { "ForwardScript", "ForwardScript2"s },
        { "BackwardScript", "BackwardScript2"s }
    };

    ghoul::Dictionary testDictionary3 = {
        { "Time", "2000 JAN 10"s },
        { "ForwardScript", "ForwardScript3"s },
        { "BackwardScript", "BackwardScript3"s }
    };

    scheduler.loadScripts({ { "1", testDictionary1 } });
    scheduler.loadScripts({ { "1", testDictionary2 }, { "2", testDictionary3 } });

    auto allScripts = scheduler.allScripts();
    REQUIRE(allScripts.size() == 3);

    REQUIRE(allScripts[0].time < allScripts[1].time);
    REQUIRE(allScripts[1].time < allScripts[2].time);

    openspace::SpiceManager::deinitialize();
}
