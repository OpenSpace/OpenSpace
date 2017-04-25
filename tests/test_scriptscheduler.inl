/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include "gtest/gtest.h"

#include <openspace/scripting/scriptscheduler.h>

// This include should be removed after the time class is not dependent on
// Spice anymore
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <ghoul/misc/dictionary.h>

#include <limits>

class ScriptSchedulerTest : public testing::Test {
protected:
    void SetUp() override {
        openspace::SpiceManager::initialize();
        openspace::SpiceManager::ref().loadKernel(
            "${TESTDIR}/SpiceTest/spicekernels/naif0008.tls"
        );
    }
    
    void TearDown() override {
        openspace::SpiceManager::deinitialize();
    }
};

TEST_F(ScriptSchedulerTest, SimpleForward) {
    using namespace openspace::scripting;
    using namespace std::string_literals;
    
    ScriptScheduler scheduler;

    ghoul::Dictionary testDictionary = {
        { "Time", "2000 JAN 03"s },
        { "ForwardScript", "ForwardScript1"s },
        { "BackwardScript", "BackwardScript1"s }
    };
    
    scheduler.progressTo(openspace::Time::convertTime("2000 JAN 01"));
    scheduler.loadScripts({
        { "1", testDictionary }
    });
    
    auto res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 02"));
    ASSERT_EQ(res.first, res.second);
    
    res = scheduler.progressTo(
        openspace::Time::convertTime("2000 JAN 03")
    );
    ASSERT_EQ(1, std::distance(res.first, res.second));
    EXPECT_EQ("ForwardScript1", *(res.first));
}

TEST_F(ScriptSchedulerTest, MultipleForwardSingleJump) {
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
    ASSERT_EQ(res.first, res.second);
    
    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 04"));
    ASSERT_EQ(1, std::distance(res.first, res.second));
    EXPECT_EQ("ForwardScript1", *(res.first));
    
    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 06"));
    ASSERT_EQ(1, std::distance(res.first, res.second));
    EXPECT_EQ("ForwardScript2", *(res.first));
}

TEST_F(ScriptSchedulerTest, MultipleForwardOrdering) {
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
    ASSERT_EQ(res.first, res.second);

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 06"));
    ASSERT_EQ(2, std::distance(res.first, res.second));
    EXPECT_EQ("ForwardScript1", *(res.first));
    EXPECT_EQ("ForwardScript2", *(std::next(res.first)));
}

TEST_F(ScriptSchedulerTest, SimpleBackward) {
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
    ASSERT_EQ(res.first, res.second);

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 02"));
    ASSERT_EQ(1, std::distance(res.first, res.second));
    EXPECT_EQ("BackwardScript1", *(res.first));
}

TEST_F(ScriptSchedulerTest, MultipleBackwardSingleJump) {
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
    ASSERT_EQ(res.first, res.second);
    
    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 04"));
    ASSERT_EQ(1, std::distance(res.first, res.second));
    EXPECT_EQ("BackwardScript2", *(res.first));
    
    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 01"));
    ASSERT_EQ(1, std::distance(res.first, res.second));
    EXPECT_EQ("BackwardScript1", *(res.first));
}

TEST_F(ScriptSchedulerTest, MultipleBackwardOrdering) {
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
    ASSERT_EQ(res.first, res.second);

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 01"));
    ASSERT_EQ(2, std::distance(res.first, res.second));
    EXPECT_EQ("BackwardScript2", *(res.first));
    EXPECT_EQ("BackwardScript1", *(std::next(res.first)));
}

TEST_F(ScriptSchedulerTest, Empty) {
    using namespace openspace::scripting;
    
    static const std::vector<double> TestTimes = {
        0.0, 1.0, -1.0, std::numeric_limits<double>::min(),
        -std::numeric_limits<double>::max(), std::numeric_limits<double>::max()
    };
    
    // First test if a new ScriptScheduler will return an empty list
    for (double t : TestTimes) {
        ScriptScheduler scheduler;
        auto res = scheduler.progressTo(t);
        EXPECT_EQ(res.first, res.second);
    }
    
    // Then test the same thing but keeping the same ScriptScheduler
    ScriptScheduler scheduler;
    for (double t : TestTimes) {
        auto res = scheduler.progressTo(t);
        EXPECT_EQ(res.first, res.second);
    }
}

TEST_F(ScriptSchedulerTest, ForwardBackwards) {
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
    ASSERT_EQ(1, std::distance(res.first, res.second));
    EXPECT_EQ("ForwardScript1", *(res.first));
    
    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 01"));
    ASSERT_EQ(1, std::distance(res.first, res.second));
    EXPECT_EQ("BackwardScript1", *(res.first));
    
    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 07"));
    ASSERT_EQ(2, std::distance(res.first, res.second));
    
    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 04"));
    ASSERT_EQ(1, std::distance(res.first, res.second));
    EXPECT_EQ("BackwardScript2", *(res.first));
}

TEST_F(ScriptSchedulerTest, Rewind) {
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
    ASSERT_EQ(2, std::distance(res.first, res.second));
    
    scheduler.rewind();
    
    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 04"));
    ASSERT_EQ(1, std::distance(res.first, res.second));
    EXPECT_EQ("ForwardScript1", *(res.first));
}

TEST_F(ScriptSchedulerTest, CurrentTime) {
    using namespace openspace::scripting;
    
    static const std::vector<double> TestValues = {
        0.0, 1.0, 42.0, std::numeric_limits<double>::min(),
        -std::numeric_limits<double>::max(), std::numeric_limits<double>::max()
    };
    
    for (double t : TestValues) {
        ScriptScheduler scheduler;
        scheduler.progressTo(t);
        EXPECT_EQ(t, scheduler.currentTime());
    }
}

TEST_F(ScriptSchedulerTest, AllScripts) {
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
    ASSERT_EQ(3, allScripts.size());
    
    EXPECT_LE(allScripts[0].time, allScripts[1].time);
    EXPECT_LE(allScripts[1].time, allScripts[2].time);
}

TEST_F(ScriptSchedulerTest, JumpEqual) {
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

    auto res = scheduler.progressTo(
        openspace::Time::convertTime("2000 JAN 03 11:00:00")
    );
    ASSERT_EQ(res.first, res.second);

    res = scheduler.progressTo(
        openspace::Time::convertTime("2000 JAN 03 12:00:00")
    );
    ASSERT_EQ(1, std::distance(res.first, res.second));
    EXPECT_EQ("ForwardScript1", *(res.first));

    res = scheduler.progressTo(
        openspace::Time::convertTime("2000 JAN 03 12:01:00")
    );
    ASSERT_EQ(res.first, res.second);

    res = scheduler.progressTo(
        openspace::Time::convertTime("2000 JAN 03 12:00:00")
    );
    ASSERT_EQ(1, std::distance(res.first, res.second));
    EXPECT_EQ("BackwardScript1", *(res.first));
}

TEST_F(ScriptSchedulerTest, SameTime) {
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

    auto res = scheduler.progressTo(
        openspace::Time::convertTime("2000 JAN 03 12:00:00")
    );
    ASSERT_EQ(1, std::distance(res.first, res.second));
    EXPECT_EQ("ForwardScript1", *(res.first));

    res = scheduler.progressTo(
        openspace::Time::convertTime("2000 JAN 03 12:00:00")
    );
    ASSERT_EQ(res.first, res.second);
}

TEST_F(ScriptSchedulerTest, MultiInnerJump) {
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

    auto res = scheduler.progressTo(
        openspace::Time::convertTime("2000 JAN 03 10:00:00")
    );
    ASSERT_EQ(res.first, res.second);

    res = scheduler.progressTo(
        openspace::Time::convertTime("2000 JAN 03 11:00:00")
    );
    ASSERT_EQ(res.first, res.second);

    res = scheduler.progressTo(
        openspace::Time::convertTime("2000 JAN 03 13:00:00")
    );
    ASSERT_EQ(1, std::distance(res.first, res.second));
    EXPECT_EQ("ForwardScript1", *(res.first));

    res = scheduler.progressTo(
        openspace::Time::convertTime("2000 JAN 03 12:30:00")
    );
    ASSERT_EQ(res.first, res.second);
}

TEST_F(ScriptSchedulerTest, MultipleForwardSingleJumpMultipleLoad) {
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
        { "1", testDictionary1 }
    });

    scheduler.loadScripts({
        { "1", testDictionary2 }
    });

    auto res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 02"));
    ASSERT_EQ(res.first, res.second);

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 04"));
    ASSERT_EQ(1, std::distance(res.first, res.second));
    EXPECT_EQ("ForwardScript1", *(res.first));

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 06"));
    ASSERT_EQ(1, std::distance(res.first, res.second));
    EXPECT_EQ("ForwardScript2", *(res.first));
}

TEST_F(ScriptSchedulerTest, MultipleForwardOrderingMultipleLoad) {
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
        { "1", testDictionary1 }
    });
    scheduler.loadScripts({
        { "1", testDictionary2 }
    });

    auto res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 02"));
    ASSERT_EQ(res.first, res.second);

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 06"));
    ASSERT_EQ(2, std::distance(res.first, res.second));
    EXPECT_EQ("ForwardScript1", *(res.first));
    EXPECT_EQ("ForwardScript2", *(std::next(res.first)));
}

TEST_F(ScriptSchedulerTest, MultipleBackwardSingleJumpMultipleLoad) {
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
        { "1", testDictionary1 }
    });
    scheduler.loadScripts({
        { "1", testDictionary2 }
    });

    auto res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 06"));
    ASSERT_EQ(res.first, res.second);

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 04"));
    ASSERT_EQ(1, std::distance(res.first, res.second));
    EXPECT_EQ("BackwardScript2", *(res.first));

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 01"));
    ASSERT_EQ(1, std::distance(res.first, res.second));
    EXPECT_EQ("BackwardScript1", *(res.first));
}

TEST_F(ScriptSchedulerTest, MultipleBackwardOrderingMultipleLoad) {
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
        { "1", testDictionary1 }
    });
    scheduler.loadScripts({
        { "1", testDictionary2 }
    });

    auto res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 06"));
    ASSERT_EQ(res.first, res.second);

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 01"));
    ASSERT_EQ(2, std::distance(res.first, res.second));
    EXPECT_EQ("BackwardScript2", *(res.first));
    EXPECT_EQ("BackwardScript1", *(std::next(res.first)));
}

TEST_F(ScriptSchedulerTest, ForwardBackwardsMultipleLoad) {
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
        { "1", testDictionary1 }
    });
    scheduler.loadScripts({
        { "1", testDictionary2 }
    });

    auto res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 04"));
    ASSERT_EQ(1, std::distance(res.first, res.second));
    EXPECT_EQ("ForwardScript1", *(res.first));

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 01"));
    ASSERT_EQ(1, std::distance(res.first, res.second));
    EXPECT_EQ("BackwardScript1", *(res.first));

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 07"));
    ASSERT_EQ(2, std::distance(res.first, res.second));

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 04"));
    ASSERT_EQ(1, std::distance(res.first, res.second));
    EXPECT_EQ("BackwardScript2", *(res.first));
}

TEST_F(ScriptSchedulerTest, RewindMultipleLoad) {
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
        { "1", testDictionary1 }
    });
    scheduler.loadScripts({
        { "1", testDictionary2 }
    });

    auto res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 07"));
    ASSERT_EQ(2, std::distance(res.first, res.second));

    scheduler.rewind();

    res = scheduler.progressTo(openspace::Time::convertTime("2000 JAN 04"));
    ASSERT_EQ(1, std::distance(res.first, res.second));
    EXPECT_EQ("ForwardScript1", *(res.first));
}

TEST_F(ScriptSchedulerTest, AllScriptsMultipleLoad) {
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
        { "1", testDictionary1 }
    });

    scheduler.loadScripts({
        { "1", testDictionary2 }
    });

    scheduler.loadScripts({
        { "1", testDictionary3 }
    });

    auto allScripts = scheduler.allScripts();
    ASSERT_EQ(3, allScripts.size());

    EXPECT_LE(allScripts[0].time, allScripts[1].time);
    EXPECT_LE(allScripts[1].time, allScripts[2].time);
}

TEST_F(ScriptSchedulerTest, AllScriptsMixedLoad) {
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
        { "1", testDictionary1 }
    });

    scheduler.loadScripts({
        { "1", testDictionary2 },
        { "2", testDictionary3 }
    });

    auto allScripts = scheduler.allScripts();
    ASSERT_EQ(3, allScripts.size());

    EXPECT_LE(allScripts[0].time, allScripts[1].time);
    EXPECT_LE(allScripts[1].time, allScripts[2].time);
}
