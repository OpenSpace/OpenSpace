/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2015                                                               *
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
#include <ghoul/glm.h>
#include <fstream>
#include <random>
#include <ghoul/misc/configurationmanager.h>
#include <ghoul/filesystem/filesystem>

namespace {
    // A non-existing configuration file
    const std::string _configuration0 = "${TEST_DIR}/configurationmanager/test0.cfg";

    // The configuration1 test configuration has one key "t" = 1
    const std::string _configuration1 = "${TEST_DIR}/configurationmanager/test1.cfg";

    // The configuration1 test configuration has two keys "t" and "s"
    const std::string _configuration2 = "${TEST_DIR}/configurationmanager/test2.cfg";

    // More complicated configuration file with nested tables
    const std::string _configuration3 = "${TEST_DIR}/configurationmanager/test3.cfg";

    // Deeply nested configuration file with 12 level
    const std::string _configuration4 = "${TEST_DIR}/configurationmanager/test4.cfg";

    // Testfile with glm::vecX, glm::matX
    const std::string _configuration5 = "${TEST_DIR}/configurationmanager/test5.cfg";
}

/*
Test checklist:
--- loadConfiguration: existing file
--- loadConfiguration: non-existing file
--- getValue: key does not exist
--- getValue: subtable does not exist
--- getValue: overriding previous configuration
--- getValue: function does not change passed value on error
--- getValue: nested keys
--- getValue: deep nesting of keys
--- getValue: correct values returned for each type
--- getValue: are all basic types implemented
--- getValue: glm::vec2, glm::vec3, glm::vec4 implemented
--- getValue: glm::matXxY implemented
--- getValue: valid conversions
--- setValue: all types implemented
--- setValue: create subtables on the way
--- setValue: value gets set correctly for each type
--- setValue: value overwrites setting in configuration file
--- setValue: deep nesting of keys
--- setValue: nested keys
--- setValue: glm::vec2, glm::vec3, glm::vec4, glm::mat3, glm::mat4 implemented
--- hasKeys: deep nesting of keys
--- hasKeys: subtables on the way do not exist
--- hasKeys: correct values for all types
--- hasKeys: nestedKeys
--- timing
*/

class ConfigurationManagerTest : public testing::Test {
protected:
    ConfigurationManagerTest() {
        _m = new ghoul::ConfigurationManager;
    }

    ~ConfigurationManagerTest() {
        if (_m) {
            delete _m;
            _m = nullptr;
        }
    }

    void reset() {
        _m->clear();
    }

    ghoul::ConfigurationManager* _m;
};

#ifdef GHL_TIMING_TESTS

TEST_F(ConfigurationManagerTest, TimingTest) {
    std::ofstream logFile("ConfigurationManagerTest.timing");

    START_TIMER(loadConfiguration1, logFile, 25);
    _m->loadConfiguration(_configuration1);
    FINISH_TIMER(loadConfiguration1, logFile);

    START_TIMER(loadConfiguration2, logFile, 25);
    _m->loadConfiguration(_configuration2);
    FINISH_TIMER(loadConfiguration2, logFile);

    START_TIMER(loadConfiguration3, logFile, 25);
    _m->loadConfiguration(_configuration3);
    FINISH_TIMER(loadConfiguration3, logFile);

    START_TIMER(loadConfiguration4, logFile, 25);
    _m->loadConfiguration(_configuration4);
    FINISH_TIMER(loadConfiguration4, logFile);

    START_TIMER(loadConfiguration5, logFile, 25);
    _m->loadConfiguration(_configuration5);
    FINISH_TIMER(loadConfiguration5, logFile);

    START_TIMER(loadConfiguration12, logFile, 25);
    _m->loadConfiguration(_configuration1);
    _m->loadConfiguration(_configuration2);
    FINISH_TIMER(loadConfiguration12, logFile);

    START_TIMER(loadConfiguration123, logFile, 25);
    _m->loadConfiguration(_configuration1);
    _m->loadConfiguration(_configuration2);
    _m->loadConfiguration(_configuration3);
    FINISH_TIMER(loadConfiguration123, logFile);

    START_TIMER(loadConfiguration1234, logFile, 25);
    _m->loadConfiguration(_configuration1);
    _m->loadConfiguration(_configuration2);
    _m->loadConfiguration(_configuration3);
    _m->loadConfiguration(_configuration4);
    FINISH_TIMER(loadConfiguration1234, logFile);

    START_TIMER(loadConfiguration12345, logFile, 25);
    _m->loadConfiguration(_configuration1);
    _m->loadConfiguration(_configuration2);
    _m->loadConfiguration(_configuration3);
    _m->loadConfiguration(_configuration4);
    _m->loadConfiguration(_configuration5);
    FINISH_TIMER(loadConfiguration12345, logFile);

    START_TIMER(setValueLevel0Int, logFile, 25);
    _m->setValue("t", 1);
    FINISH_TIMER(setValueLevel0Int, logFile);

    START_TIMER(setValueLevel1Int, logFile, 25);
    _m->setValue("t.t", 1);
    FINISH_TIMER(setValueLevel1Int, logFile);

    START_TIMER(setValueLevel10Int, logFile, 25);
    _m->setValue("t.t.t.t.t.t.t.t.t.t", 1);
    FINISH_TIMER(setValueLevel10Int, logFile);

    START_TIMER(setValueLevel0dvec4, logFile, 25);
    _m->setValue("t", glm::dvec4(1.0));
    FINISH_TIMER(setValueLevel0dvec4, logFile);

    START_TIMER(setValueLevel0dmat4, logFile, 25);
    _m->setValue("t", glm::dmat4(1.0));
    FINISH_TIMER(setValueLevel0dmat4, logFile);
    
    {
        int i;
        START_TIMER_PREPARE(getValueLevel0Int, logFile, 25, {_m->setValue("t", 1);});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel0Int, logFile);

        START_TIMER(getValueLevel0IntEmpty, logFile, 25);
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel0IntEmpty, logFile);

        START_TIMER_PREPARE(getValueLevel1Int, logFile, 25, {_m->setValue("t.t", 1);});
        _m->getValue("t.t", i);
        FINISH_TIMER(getValueLevel1Int, logFile);

        START_TIMER_PREPARE(getValueLevel10Int, logFile, 25,
        {_m->setValue("t.t.t.t.t.t.t.t.t.t", 1);});
        _m->getValue("t.t.t.t.t.t.t.t.t.t", i);
        FINISH_TIMER(getValueLevel10Int, logFile);

        START_TIMER(getValueLevel10IntEmpty, logFile, 25);
        _m->getValue("t.t.t.t.t.t.t.t.t.t", i);
        FINISH_TIMER(getValueLevel10IntEmpty, logFile);
    }

    {
        START_TIMER(setValueLevel1vec2, logFile, 25);
        _m->setValue("t", glm::vec2(1.0));
        FINISH_TIMER(setValueLevel1vec2, logFile);
    }
    {
        START_TIMER(setValueLevel1vec3, logFile, 25);
        _m->setValue("t", glm::vec3(1.0));
        FINISH_TIMER(setValueLevel1vec3, logFile);
    }
    {
        START_TIMER(setValueLevel1vec4, logFile, 25);
        _m->setValue("t", glm::vec4(1.0));
        FINISH_TIMER(setValueLevel1vec4, logFile);
    }
    {
        START_TIMER(setValueLevel1dvec2, logFile, 25);
        _m->setValue("t", glm::dvec2(1.0));
        FINISH_TIMER(setValueLevel1dvec2, logFile);
    }
    {
        START_TIMER(setValueLevel1dvec3, logFile, 25);
        _m->setValue("t", glm::dvec3(1.0));
        FINISH_TIMER(setValueLevel1dvec3, logFile);
    }
    {
        START_TIMER(setValueLevel1dvec4, logFile, 25);
        _m->setValue("t", glm::dvec4(1.0));
        FINISH_TIMER(setValueLevel1dvec4, logFile);
    }
    {
        START_TIMER(setValueLevel1ivec2, logFile, 25);
        _m->setValue("t", glm::ivec2(1.0));
        FINISH_TIMER(setValueLevel1ivec2, logFile);
    }
    {
        START_TIMER(setValueLevel1ivec3, logFile, 25);
        _m->setValue("t", glm::ivec3(1.0));
        FINISH_TIMER(setValueLevel1ivec3, logFile);
    }
    {
        START_TIMER(setValueLevel1ivec4, logFile, 25);
        _m->setValue("t", glm::ivec4(1.0));
        FINISH_TIMER(setValueLevel1ivec4, logFile);
    }
    {
        START_TIMER(setValueLevel1bvec2, logFile, 25);
        _m->setValue("t", glm::bvec2(true));
        FINISH_TIMER(setValueLevel1bvec2, logFile);
    }
    {
        START_TIMER(setValueLevel1bvec3, logFile, 25);
        _m->setValue("t", glm::bvec3(true));
        FINISH_TIMER(setValueLevel1bvec3, logFile);
    }
    {
        START_TIMER(setValueLevel1bvec4, logFile, 25);
        _m->setValue("t", glm::bvec4(true));
        FINISH_TIMER(setValueLevel1bvec4, logFile);
    }
    {
        START_TIMER(setValueLevel1uvec2, logFile, 25);
        _m->setValue("t", glm::uvec2(1.0));
        FINISH_TIMER(setValueLevel1uvec2, logFile);
    }
    {
        START_TIMER(setValueLevel1uvec3, logFile, 25);
        _m->setValue("t", glm::uvec3(1.0));
        FINISH_TIMER(setValueLevel1uvec3, logFile);
    }
    {
        START_TIMER(setValueLevel1uvec4, logFile, 25);
        _m->setValue("t", glm::uvec4(1.0));
        FINISH_TIMER(setValueLevel1uvec4, logFile);
    }
    {
        START_TIMER(setValueLevel1dmat2x4, logFile, 25);
        _m->setValue("t", glm::dmat2x4(1.0));
        FINISH_TIMER(setValueLevel1dmat2x4, logFile);
    }
    {
        START_TIMER(setValueLevel1dmat2x3, logFile, 25);
        _m->setValue("t", glm::dmat2x3(1.0));
        FINISH_TIMER(setValueLevel1dmat2x3, logFile);
    }
    {
        START_TIMER(setValueLevel1dmat2, logFile, 25);
        _m->setValue("t", glm::dmat2(1.0));
        FINISH_TIMER(setValueLevel1dmat2, logFile);
    }
    {
        START_TIMER(setValueLevel1dmat3x2, logFile, 25);
        _m->setValue("t", glm::dmat3x2(1.0));
        FINISH_TIMER(setValueLevel1dmat3x2, logFile);
    }
    {
        START_TIMER(setValueLevel1dmat3x4, logFile, 25);
        _m->setValue("t", glm::dmat3x4(1.0));
        FINISH_TIMER(setValueLevel1dmat3x4, logFile);
    }
    {
        START_TIMER(setValueLevel1dmat3, logFile, 25);
        _m->setValue("t", glm::dmat3(1.0));
        FINISH_TIMER(setValueLevel1dmat3, logFile);
    }
    {
        START_TIMER(setValueLevel1dmat4x2, logFile, 25);
        _m->setValue("t", glm::dmat4x3(1.0));
        FINISH_TIMER(setValueLevel1dmat4x2, logFile);
    }
    {
        START_TIMER(setValueLevel1dmat4x3, logFile, 25);
        _m->setValue("t", glm::dmat4x3(1.0));
        FINISH_TIMER(setValueLevel1dmat4x3, logFile);
    }
    {
        START_TIMER(setValueLevel1dmat4, logFile, 25);
        _m->setValue("t", glm::dmat4(1.0));
        FINISH_TIMER(setValueLevel1dmat4, logFile);
    }

    {
        glm::vec2 i;
        START_TIMER_PREPARE(getValueLevel1vec2, logFile, 25,
        {_m->setValue("t", glm::vec2(1.0));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1vec2, logFile);
    }
    {
        glm::vec3 i;
        START_TIMER_PREPARE(getValueLevel1vec3, logFile, 25,
        {_m->setValue("t", glm::vec3(1.0));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1vec3, logFile);
    }
    {
        glm::vec4 i;
        START_TIMER_PREPARE(getValueLevel1vec4, logFile, 25,
        {_m->setValue("t", glm::vec4(1.0));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1vec4, logFile);
    }
    {
        glm::dvec2 i;
        START_TIMER_PREPARE(getValueLevel1dvec2, logFile, 25,
        {_m->setValue("t", glm::dvec2(1.0));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1dvec2, logFile);
    }
    {
        glm::dvec3 i;
        START_TIMER_PREPARE(getValueLevel1dvec3, logFile, 25,
        {_m->setValue("t", glm::dvec3(1.0));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1dvec3, logFile);
    }
    {
        glm::dvec4 i;
        START_TIMER_PREPARE(getValueLevel1dvec4, logFile, 25,
        {_m->setValue("t", glm::dvec4(1.0));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1dvec4, logFile);
    }
    {
        glm::ivec2 i;
        START_TIMER_PREPARE(getValueLevel1ivec2, logFile, 25,
        {_m->setValue("t", glm::ivec2(1.0));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1ivec2, logFile);
    }
    {
        glm::ivec3 i;
        START_TIMER_PREPARE(getValueLevel1ivec3, logFile, 25,
        {_m->setValue("t", glm::ivec3(1.0));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1ivec3, logFile);
    }
    {
        glm::ivec4 i;
        START_TIMER_PREPARE(getValueLevel1ivec4, logFile, 25,
        {_m->setValue("t", glm::ivec4(1.0));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1ivec4, logFile);
    }
    {
        glm::bvec2 i;
        START_TIMER_PREPARE(getValueLevel1bvec2, logFile, 25,
        {_m->setValue("t", glm::bvec2(true));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1bvec2, logFile);
    }
    {
        glm::bvec3 i;
        START_TIMER_PREPARE(getValueLevel1bvec3, logFile, 25,
        {_m->setValue("t", glm::bvec3(true));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1bvec3, logFile);
    }
    {
        glm::bvec4 i;
        START_TIMER_PREPARE(getValueLevel1bvec4, logFile, 25,
        {_m->setValue("t", glm::bvec4(true));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1bvec4, logFile);
    }
    {
        glm::uvec2 i;
        START_TIMER_PREPARE(getValueLevel1uvec2, logFile, 25,
        {_m->setValue("t", glm::uvec2(1.0));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1uvec2, logFile);
    }
    {
        glm::uvec3 i;
        START_TIMER_PREPARE(getValueLevel1uvec3, logFile, 25,
        {_m->setValue("t", glm::uvec3(1.0));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1uvec3, logFile);
    }
    {
        glm::uvec4 i;
        START_TIMER_PREPARE(getValueLevel1uvec4, logFile, 25,
        {_m->setValue("t", glm::uvec4(1.0));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1uvec4, logFile);
    }
    {
        glm::dmat2x4 i;
        START_TIMER_PREPARE(getValueLevel1dmat2x4, logFile, 25,
        {_m->setValue("t", glm::dmat2x4(1.0));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1dmat2x4, logFile);
    }
    {
        glm::dmat2x3 i;
        START_TIMER_PREPARE(getValueLevel1dmat2x3, logFile, 25,
        {_m->setValue("t", glm::dmat2x3(1.0));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1dmat2x3, logFile);
    }
    {
        glm::dmat2 i;
        START_TIMER_PREPARE(getValueLevel1dmat2, logFile, 25,
        {_m->setValue("t", glm::dmat2(1.0));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1dmat2, logFile);
    }
    {
        glm::dmat3x2 i;
        START_TIMER_PREPARE(getValueLevel1dmat3x2, logFile, 25,
        {_m->setValue("t", glm::dmat3x2(1.0));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1dmat3x2, logFile);
    }
    {
        glm::dmat3x4 i;
        START_TIMER_PREPARE(getValueLevel1dmat3x4, logFile, 25,
        {_m->setValue("t", glm::dmat3x4(1.0));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1dmat3x4, logFile);
    }
    {
        glm::dmat3 i;
        START_TIMER_PREPARE(getValueLevel1dmat3, logFile, 25,
        {_m->setValue("t", glm::dmat3(1.0));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1dmat3, logFile);
    }
    {
        glm::dmat4x2 i;
        START_TIMER_PREPARE(getValueLevel1dmat4x2, logFile, 25,
        {_m->setValue("t", glm::dmat4x2(1.0));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1dmat4x2, logFile);
    }
    {
        glm::dmat4x3 i;
        START_TIMER_PREPARE(getValueLevel1dmat4x3, logFile, 25,
        {_m->setValue("t", glm::dmat4x3(1.0));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1dmat4x3, logFile);
    }
    {
        glm::dmat4 i;
        START_TIMER_PREPARE(getValueLevel1dmat4, logFile, 25,
        {_m->setValue("t", glm::dmat4(1.0));});
        _m->getValue("t", i);
        FINISH_TIMER(getValueLevel1dmat4, logFile);
    }

    START_TIMER(hasKeyLevel0Empty, logFile, 25);
    _m->hasKey("t");
    FINISH_TIMER(hasKeyLevel0Empty, logFile);

    START_TIMER(hasKeyLevel10Empty, logFile, 25);
    _m->hasKey("t.t.t.t.t.t.t.t.t.t");
    FINISH_TIMER(hasKeyLevel10Empty, logFile);

    START_TIMER_PREPARE(hasKeyLevel0, logFile, 25, {_m->setValue("t", 1);});
    _m->hasKey("t");
    FINISH_TIMER(hasKeyLevel0, logFile);

    START_TIMER_PREPARE(hasKeyLevel10, logFile, 25,
                            {_m->setValue("t.t.t.t.t.t.t.t.t.t", 1);});
    _m->hasKey("t.t.t.t.t.t.t.t.t.t");
    FINISH_TIMER(hasKeyLevel10, logFile);
}

#endif // GHL_TIMING_TESTS

TEST_F(ConfigurationManagerTest, ReinitTest) {
    _m->setValue("t", int(2));
    _m->clear();
    const bool success = _m->hasKey("t");
    EXPECT_EQ(false, success);
} 

TEST_F(ConfigurationManagerTest, LoadConfigurationTest) {
    const bool success0 = _m->loadConfiguration(_configuration0);
    ASSERT_EQ(false, success0) << "Loading a non-existing file should fail gracefully";
    const bool success1 = _m->loadConfiguration(_configuration1);
    ASSERT_EQ(true, success1) << "Loading of configuration file 'test1.cfg'";
    const bool success2 = _m->loadConfiguration(_configuration2);
    ASSERT_EQ(true, success2) << "Loading of configuration file 'test2.cfg'";
    const bool success3 = _m->loadConfiguration(_configuration3);
    ASSERT_EQ(true, success3) << "Loading of configuration file 'test3.cfg'";
    const bool success4 = _m->loadConfiguration(_configuration4);
    ASSERT_EQ(true, success4) << "Loading of configuration file 'test4.cfg'";
}

TEST_F(ConfigurationManagerTest, KeysFunction) {
    // The empty configuration should not have any keys
    size_t nKeys = _m->keys().size();
    EXPECT_EQ(0, nKeys) << "The empty configuration should not have any keys";

    _m->loadConfiguration(_configuration1);
    nKeys = _m->keys().size();
    EXPECT_EQ(1, nKeys) << "test1";

    _m->loadConfiguration(_configuration3);
    nKeys = _m->keys().size();
    EXPECT_EQ(3, nKeys) << "base: test1 + test3";

    nKeys = _m->keys("s").size();
    EXPECT_EQ(3, nKeys) << "s: test1 + test3";

    nKeys = _m->keys("s.3").size();
    EXPECT_EQ(2, nKeys) << "s.3: test1 + test3";

    _m->loadConfiguration(_configuration4);

    const char* keys[] = {
        "a", "a.a", "a.a.a", "a.a.a.a", "a.a.a.a.a", "a.a.a.a.a.a", "a.a.a.a.a.a.a",
        "a.a.a.a.a.a.a.a", "a.a.a.a.a.a.a.a.a", "a.a.a.a.a.a.a.a.a.a",
        "a.a.a.a.a.a.a.a.a.a.a", "a.a.a.a.a.a.a.a.a.a.a.a"
    };

    for (int i = 0; i < 12; ++i) {
        nKeys = _m->keys(keys[i]).size();
        EXPECT_EQ(2, nKeys) << keys[i] <<": test1 + test3";
    }

    for (int i = 0; i < 12; ++i) {
        const bool hasKey = _m->hasKey(keys[i]);
        EXPECT_EQ(true, hasKey) << keys[i] <<": test1 + test3";
    }


    const char* keysB[] = {
        "b", "b.b", "b.b.b", "b.b.b.b", "b.b.b.b.b", "b.b.b.b.b.b", "b.b.b.b.b.b.b",
        "b.b.b.b.b.b.b.b", "b.b.b.b.b.b.b.b.b", "b.b.b.b.b.b.b.b.b.b",
        "b.b.b.b.b.b.b.b.b.b.b", "b.b.b.b.b.b.b.b.b.b.b.b"
    };
    _m->setValue(keysB[11], int(0), true);
    for (int i = 0; i < 12; ++i)
        EXPECT_EQ(true, _m->hasKey(keysB[i])) << keysB[i] <<": test1 + test3";
}

TEST_F(ConfigurationManagerTest, HasKeySubtable) {
    _m->loadConfiguration(_configuration1);
    const bool tSuccess = _m->hasKey("t");
    ASSERT_EQ(true, tSuccess) << "t";

    const bool tsSuccess = _m->hasKey("t.s");
    EXPECT_EQ(false, tsSuccess) << "t.s";

    const bool sSuccess = _m->hasKey("s");
    EXPECT_EQ(false, sSuccess) << "s";

    const bool sxSuccess = _m->hasKey("s.x");
    EXPECT_EQ(false, sxSuccess) << "s.x";
}

TEST_F(ConfigurationManagerTest, HasKeyTypes) {
    _m->setValue("t", ghoul::Dictionary());
    _m->setValue("t.bool", bool(1));
    _m->setValue("t.char", char(1));
    _m->setValue("t.unsignedchar", (unsigned char)(1));
    _m->setValue("t.signedchar", (signed char)(1));
    _m->setValue("t.wchar", wchar_t(1));
    _m->setValue("t.short", short(1));
    _m->setValue("t.unsignedshort", (unsigned short)(1));
    _m->setValue("t.int", int(1));
    _m->setValue("t.unsignedint", (unsigned int)(1));
    _m->setValue("t.long", long(1));
    _m->setValue("t.unsignedlong", (unsigned long)(1));
    _m->setValue("t.longlong", (long long)(1));
    _m->setValue("t.unsignedlonglong", (unsigned long long)(1));
    _m->setValue("t.float", float(1));
    _m->setValue("t.double", double(1));
    _m->setValue("t.longdouble", (long double)(1));
    _m->setValue("t.string", "1");

    bool success = _m->hasKey("t.bool");
    EXPECT_EQ(true, success) << "t.bool";
    success = _m->hasKey("t.char");
    EXPECT_EQ(true, success) << "t.char";
    success = _m->hasKey("t.unsignedchar");
    EXPECT_EQ(true, success) << "t.unsignedchar";
    success = _m->hasKey("t.signedchar");
    EXPECT_EQ(true, success) << "t.signedchar";
    success = _m->hasKey("t.wchar");
    EXPECT_EQ(true, success) << "t.wchar";
    success = _m->hasKey("t.short");
    EXPECT_EQ(true, success) << "t.short";
    success = _m->hasKey("t.unsignedshort");
    EXPECT_EQ(true, success) << "t.unsignedshort";
    success = _m->hasKey("t.int");
    EXPECT_EQ(true, success) << "t.int";
    success = _m->hasKey("t.unsignedint");
    EXPECT_EQ(true, success) << "t.unsignedint";
    success = _m->hasKey("t.long");
    EXPECT_EQ(true, success) << "t.long";
    success = _m->hasKey("t.unsignedlong");
    EXPECT_EQ(true, success) << "t.unsignedlong";
    success = _m->hasKey("t.longlong");
    EXPECT_EQ(true, success) << "t.longlong";
    success = _m->hasKey("t.unsignedlonglong");
    EXPECT_EQ(true, success) << "t.unsignedlonglong";
    success = _m->hasKey("t.float");
    EXPECT_EQ(true, success) << "t.float";
    success = _m->hasKey("t.double");
    EXPECT_EQ(true, success) << "t.double";
    success = _m->hasKey("t.longdouble");
    EXPECT_EQ(true, success) << "t.longdouble";
    success = _m->hasKey("t.string");
    EXPECT_EQ(true, success) << "t.string";
}

TEST_F(ConfigurationManagerTest, GetValueFunction) {
    std::string test;
    bool success = _m->getValue("key", test);
    EXPECT_EQ(false, success) << "Empty configuration";

    success = _m->getValue("key.key", test);
    EXPECT_EQ(false, success) << "Empty configuration recursive";

    _m->loadConfiguration(_configuration1);
    _m->loadConfiguration(_configuration3);
    int testInt;
    success = _m->getValue("t", testInt);
    EXPECT_EQ(true, success) << "test1+test3 (t)";
    EXPECT_EQ(1, testInt) << "test1+test3 (t)";

    success = _m->getValue("s.a", test);
    EXPECT_EQ(false, success) << "test1+test3 (s.a)";

    success = _m->getValue("s.1", test);
    EXPECT_EQ(true, success) << "test1+test3 (s.1)";

    success = _m->getValue("s.1.a", test);
    EXPECT_EQ(false, success) << "test1+test3 (s.1.a)";

    success = _m->getValue("s.3.a", test);
    EXPECT_EQ(true, success) << "test1+test3 (s.3.a)";

    std::vector<int> testVec;
    success = _m->getValue("key", testVec);
    EXPECT_EQ(false, success) << "test1+test3: Vector access";
}

template <class T>
void correctnessHelperGetValue(ghoul::ConfigurationManager* m, const std::string& key) {
    T value = T(0);
    const bool success = m->getValue(key, value);
    EXPECT_EQ(true, success) << "Type: " << typeid(T).name();
    EXPECT_EQ(T(1), value) << "Type: " << typeid(T).name();
}

TEST_F(ConfigurationManagerTest, GetValueCorrectness) {
    _m->loadConfiguration(_configuration1);

    correctnessHelperGetValue<bool>(_m, "t");
    correctnessHelperGetValue<char>(_m, "t");
    correctnessHelperGetValue<signed char>(_m, "t");
    correctnessHelperGetValue<unsigned char>(_m, "t");
    correctnessHelperGetValue<wchar_t>(_m, "t");
    correctnessHelperGetValue<short>(_m, "t");
    correctnessHelperGetValue<unsigned short>(_m, "t");
    correctnessHelperGetValue<int>(_m, "t");
    correctnessHelperGetValue<unsigned int>(_m, "t");
    correctnessHelperGetValue<long>(_m, "t");
    correctnessHelperGetValue<unsigned long>(_m, "t");
    correctnessHelperGetValue<long long>(_m, "t");
    correctnessHelperGetValue<unsigned long long>(_m, "t");
    correctnessHelperGetValue<float>(_m, "t");
    correctnessHelperGetValue<double>(_m, "t");
    correctnessHelperGetValue<long double>(_m, "t");

    std::string value;
    const bool success = _m->getValue("t", value);
    EXPECT_EQ(true, success) << "Type: " << typeid(std::string).name();
    EXPECT_STREQ("1", value.c_str()) << "Type: " << typeid(std::string).name();
}

TEST_F(ConfigurationManagerTest, SetValueRecursive) {
    _m->setValue("t.a.b.c", 1);
    EXPECT_EQ(true, _m->hasKey("t"));
    EXPECT_EQ(true, _m->hasKey("t.a"));
    EXPECT_EQ(true, _m->hasKey("t.a.b"));
    EXPECT_EQ(true, _m->hasKey("t.a.b.c"));
}

TEST_F(ConfigurationManagerTest, SetValueCorrectness) {
    _m->setValue("t.bool", bool(1));
    _m->setValue("t.char", char(1));
    _m->setValue("t.unsignedchar", (unsigned char)(1));
    _m->setValue("t.signedchar", (signed char)(1));
    _m->setValue("t.wchar", wchar_t(1));
    _m->setValue("t.short", short(1));
    _m->setValue("t.unsignedshort", (unsigned short)(1));
    _m->setValue("t.int", int(1));
    _m->setValue("t.unsignedint", (unsigned int)(1));
    _m->setValue("t.long", long(1));
    _m->setValue("t.unsignedlong", (unsigned long)(1));
    _m->setValue("t.longlong", (long long)(1));
    _m->setValue("t.unsignedlonglong", (unsigned long long)(1));
    _m->setValue("t.float", float(1));
    _m->setValue("t.double", double(1));
    _m->setValue("t.longdouble", (long double)(1));
    _m->setValue("t.string", "1");

    correctnessHelperGetValue<bool>(_m, "t.bool");
    correctnessHelperGetValue<char>(_m, "t.char");
    correctnessHelperGetValue<signed char>(_m, "t.unsignedchar");
    correctnessHelperGetValue<unsigned char>(_m, "t.signedchar");
    correctnessHelperGetValue<wchar_t>(_m, "t.wchar");
    correctnessHelperGetValue<short>(_m, "t.short");
    correctnessHelperGetValue<unsigned short>(_m, "t.unsignedshort");
    correctnessHelperGetValue<int>(_m, "t.int");
    correctnessHelperGetValue<unsigned int>(_m, "t.unsignedint");
    correctnessHelperGetValue<long>(_m, "t.long");
    correctnessHelperGetValue<unsigned long>(_m, "t.unsignedlong");
    correctnessHelperGetValue<long long>(_m, "t.longlong");
    correctnessHelperGetValue<unsigned long long>(_m, "t.unsignedlonglong");
    correctnessHelperGetValue<float>(_m, "t.float");
    correctnessHelperGetValue<double>(_m, "t.double");
    correctnessHelperGetValue<long double>(_m, "t.longdouble");

    std::string value;
    const bool success = _m->getValue("t.string", value);
    EXPECT_EQ(true, success) << "Type: " << typeid(std::string).name();
    EXPECT_STREQ("1", value.c_str()) << "Type: " << typeid(std::string).name();
}

TEST_F(ConfigurationManagerTest, SetValueOverridesConfiguration) {
    _m->loadConfiguration(_configuration1);
    int v = 0;
    bool success = _m->getValue<int>("t", v);
    ASSERT_EQ(true, success) << "t";
    ASSERT_EQ(1, v) << "t";

    _m->setValue("t", int(2));
    success = _m->getValue<int>("t", v);
    ASSERT_EQ(true, success) << "t";
    ASSERT_EQ(2, v) << "t";
}

TEST_F(ConfigurationManagerTest, GetValueConversions) {
    // converting from 1 -> all types is done in GetValueCorrectness
    _m->loadConfiguration(_configuration2);

    correctnessHelperGetValue<bool>(_m, "s.a1");
    correctnessHelperGetValue<char>(_m, "s.a1");
    correctnessHelperGetValue<signed char>(_m, "s.a1");
    correctnessHelperGetValue<unsigned char>(_m, "s.a1");
    correctnessHelperGetValue<wchar_t>(_m, "s.a1");
    correctnessHelperGetValue<short>(_m, "s.a1");
    correctnessHelperGetValue<unsigned short>(_m, "s.a1");
    correctnessHelperGetValue<int>(_m, "s.a1");
    correctnessHelperGetValue<unsigned int>(_m, "s.a1");
    correctnessHelperGetValue<long>(_m, "s.a1");
    correctnessHelperGetValue<unsigned long>(_m, "s.a1");
    correctnessHelperGetValue<long long>(_m, "s.a1");
    correctnessHelperGetValue<unsigned long long>(_m, "s.a1");
    correctnessHelperGetValue<float>(_m, "s.a1");
    correctnessHelperGetValue<double>(_m, "s.a1");
    correctnessHelperGetValue<long double>(_m, "s.a1");

    std::string value;
    const bool success = _m->getValue("s.a1", value);
    EXPECT_EQ(true, success) << "Type: " << typeid(std::string).name();
    EXPECT_STREQ("1", value.c_str()) << "Type: " << typeid(std::string).name();
}

TEST_F(ConfigurationManagerTest, StringKeyVsIntKey) {
    _m->loadConfiguration(_configuration3);

    int v = 0;
     bool success = _m->getValue("tt[\"1\"]", v);
    ASSERT_EQ(true, success) << "tt.1";
    EXPECT_EQ(2, v) << "tt.1";

    success = _m->getValue("tt[1]", v);
    ASSERT_EQ(true, success) << "tt[1]";
    EXPECT_EQ(1, v) << "tt[1]";
}

TEST_F(ConfigurationManagerTest, InvalidKeyAccessInvariant) {
    // Accessing an invalid key should not change the tested argument
    std::mt19937 rd;
    {
        std::uniform_int_distribution<int> dist;
        for (int i = 0; i < 10; ++i) {
            const int testValue = dist(rd);
            int test = testValue;
            _m->getValue("key", test);
            ASSERT_EQ(testValue, test) << "invariant int";
        }
    }

    {
        std::uniform_real_distribution<float> dist;
        for (int i = 0; i < 10; ++i) {
            const float testValue = dist(rd);
            float test = testValue;
            _m->getValue("key", test);
            ASSERT_EQ(testValue, test) << "invariant float";
        }
    }
}

TEST_F(ConfigurationManagerTest, HasKeyFunction) {
    bool success = _m->hasKey("key");
    EXPECT_EQ(false, success) << "empty configuration";

    _m->loadConfiguration(_configuration1);
    success = _m->hasKey("t");
    EXPECT_EQ(true, success) << "test1 (t)";

    success = _m->hasKey("s");
    EXPECT_EQ(false, success) << "test1 (s)";

    _m->loadConfiguration(_configuration2);
    success = _m->hasKey("s.a");
    EXPECT_EQ(true, success) << "test1+test2 (s.a)";

    success = _m->hasKey("s.c");
    EXPECT_EQ(false, success) << "test1+test2 (s.c)";
}


TEST_F(ConfigurationManagerTest, MultipleKeyLoadOverwrite) {
    _m->loadConfiguration(_configuration1);
    int value;
    _m->getValue("t", value);
    EXPECT_EQ(1, value);

    _m->loadConfiguration(_configuration2);

    // configuration2 should overwrite the value t in configuration1
    _m->getValue("t", value);
    EXPECT_EQ(2, value);
}

template <class T>
void vectorClassHelper(ghoul::ConfigurationManager* m, const std::string& key) {
    T value = T(0);
    const bool success = m->getValue(key, value);
    EXPECT_EQ(true, success) << "Type: " << typeid(T).name() << " | Key: " << key;
    EXPECT_EQ(T(glm::vec4(5, 6, 7, 8)), value) << "Type: " << typeid(T).name()  <<
        " | Key: " << key;
}

template <>
void vectorClassHelper<glm::bvec2>(ghoul::ConfigurationManager* m, const std::string& key) {
    glm::bvec2 value = glm::bvec2(false);
    const bool success = m->getValue(key, value);
    EXPECT_EQ(true, success) << "Type: bvec2 | Key: " << key;
    EXPECT_EQ(true, value.x) << "Type: bvec2 | Key: " << key;
    EXPECT_EQ(true, value.y) << "Type: bvec2 | Key: " << key;
}

template <>
void vectorClassHelper<glm::bvec3>(ghoul::ConfigurationManager* m, const std::string& key) {
    glm::bvec3 value = glm::bvec3(false);
    const bool success = m->getValue(key, value);
    EXPECT_EQ(true, success) << "Type: bvec3 | Key: " << key;
    EXPECT_EQ(true, value.x) << "Type: bvec3 | Key: " << key;
    EXPECT_EQ(true, value.y) << "Type: bvec3 | Key: " << key;
    EXPECT_EQ(true, value.z) << "Type: bvec3 | Key: " << key;
}

template <>
void vectorClassHelper<glm::bvec4>(ghoul::ConfigurationManager* m, const std::string& key) {
    glm::bvec4 value = glm::bvec4(false);
    const bool success = m->getValue(key, value);
    EXPECT_EQ(true, success) << "Type: bvec4 | Key: " << key;
    EXPECT_EQ(true, value.x) << "Type: bvec4 | Key: " << key;
    EXPECT_EQ(true, value.y) << "Type: bvec4 | Key: " << key;
    EXPECT_EQ(true, value.z) << "Type: bvec4 | Key: " << key;
    EXPECT_EQ(true, value.w) << "Type: bvec4 | Key: " << key;
}

TEST_F(ConfigurationManagerTest, VectorClassesGet) {
    _m->loadConfiguration(_configuration5);
    vectorClassHelper<glm::vec2>(_m, "n2");
    vectorClassHelper<glm::vec2>(_m, "num2");
    vectorClassHelper<glm::vec2>(_m, "xy");
    vectorClassHelper<glm::vec2>(_m, "rg");
    vectorClassHelper<glm::vec2>(_m, "st");
    vectorClassHelper<glm::dvec2>(_m, "n2");
    vectorClassHelper<glm::dvec2>(_m, "num2");
    vectorClassHelper<glm::dvec2>(_m, "xy");
    vectorClassHelper<glm::dvec2>(_m, "rg");
    vectorClassHelper<glm::dvec2>(_m, "st");
    vectorClassHelper<glm::ivec2>(_m, "n2");
    vectorClassHelper<glm::ivec2>(_m, "num2");
    vectorClassHelper<glm::ivec2>(_m, "xy");
    vectorClassHelper<glm::ivec2>(_m, "rg");
    vectorClassHelper<glm::ivec2>(_m, "st");
    vectorClassHelper<glm::uvec2>(_m, "n2");
    vectorClassHelper<glm::uvec2>(_m, "num2");
    vectorClassHelper<glm::uvec2>(_m, "xy");
    vectorClassHelper<glm::uvec2>(_m, "rg");
    vectorClassHelper<glm::uvec2>(_m, "st");
    vectorClassHelper<glm::bvec2>(_m, "n2");
    vectorClassHelper<glm::bvec2>(_m, "num2");
    vectorClassHelper<glm::bvec2>(_m, "xy");
    vectorClassHelper<glm::bvec2>(_m, "rg");
    vectorClassHelper<glm::bvec2>(_m, "st");

    vectorClassHelper<glm::vec3>(_m, "n3");
    vectorClassHelper<glm::vec3>(_m, "num3");
    vectorClassHelper<glm::vec3>(_m, "xyz");
    vectorClassHelper<glm::vec3>(_m, "rgb");
    vectorClassHelper<glm::vec3>(_m, "stp");
    vectorClassHelper<glm::dvec3>(_m, "n3");
    vectorClassHelper<glm::dvec3>(_m, "num3");
    vectorClassHelper<glm::dvec3>(_m, "xyz");
    vectorClassHelper<glm::dvec3>(_m, "rgb");
    vectorClassHelper<glm::dvec3>(_m, "stp");
    vectorClassHelper<glm::ivec3>(_m, "n3");
    vectorClassHelper<glm::ivec3>(_m, "num3");
    vectorClassHelper<glm::ivec3>(_m, "xyz");
    vectorClassHelper<glm::ivec3>(_m, "rgb");
    vectorClassHelper<glm::ivec3>(_m, "stp");
    vectorClassHelper<glm::uvec3>(_m, "n3");
    vectorClassHelper<glm::uvec3>(_m, "num3");
    vectorClassHelper<glm::uvec3>(_m, "xyz");
    vectorClassHelper<glm::uvec3>(_m, "rgb");
    vectorClassHelper<glm::uvec3>(_m, "stp");
    vectorClassHelper<glm::bvec3>(_m, "n3");
    vectorClassHelper<glm::bvec3>(_m, "num3");
    vectorClassHelper<glm::bvec3>(_m, "xyz");
    vectorClassHelper<glm::bvec3>(_m, "rgb");
    vectorClassHelper<glm::bvec3>(_m, "stp");

    vectorClassHelper<glm::vec4>(_m, "n4");
    vectorClassHelper<glm::vec4>(_m, "num4");
    vectorClassHelper<glm::vec4>(_m, "xyzw");
    vectorClassHelper<glm::vec4>(_m, "rgba");
    vectorClassHelper<glm::vec4>(_m, "stpq");
    vectorClassHelper<glm::dvec4>(_m, "n4");
    vectorClassHelper<glm::dvec4>(_m, "num4");
    vectorClassHelper<glm::dvec4>(_m, "xyzw");
    vectorClassHelper<glm::dvec4>(_m, "rgba");
    vectorClassHelper<glm::dvec4>(_m, "stpq");
    vectorClassHelper<glm::ivec4>(_m, "num4");
    vectorClassHelper<glm::ivec4>(_m, "n4");
    vectorClassHelper<glm::ivec4>(_m, "xyzw");
    vectorClassHelper<glm::ivec4>(_m, "rgba");
    vectorClassHelper<glm::ivec4>(_m, "stpq");
    vectorClassHelper<glm::uvec4>(_m, "num4");
    vectorClassHelper<glm::uvec4>(_m, "n4");
    vectorClassHelper<glm::uvec4>(_m, "xyzw");
    vectorClassHelper<glm::uvec4>(_m, "rgba");
    vectorClassHelper<glm::uvec4>(_m, "stpq");
    vectorClassHelper<glm::bvec4>(_m, "num4");
    vectorClassHelper<glm::bvec4>(_m, "n4");
    vectorClassHelper<glm::bvec4>(_m, "xyzw");
    vectorClassHelper<glm::bvec4>(_m, "rgba");
    vectorClassHelper<glm::bvec4>(_m, "stpq");

    glm::vec3 value = glm::vec3(0.f);
    const bool success = _m->getValue("mix", value);
    EXPECT_EQ(false, success) << "Type: mixed";
    EXPECT_EQ(glm::vec3(0.f), value) << "Type: mixed";
}

TEST_F(ConfigurationManagerTest, VectorClassesSet) {
    _m->setValue("t.vec2", glm::vec2(5,6));
    _m->setValue("t.vec3", glm::vec3(5,6,7));
    _m->setValue("t.vec4", glm::vec4(5,6,7,8));
    _m->setValue("t.dvec2", glm::dvec2(5,6));
    _m->setValue("t.dvec3", glm::dvec3(5,6,7));
    _m->setValue("t.dvec4", glm::dvec4(5,6,7,8));
    _m->setValue("t.ivec2", glm::ivec2(5,6));
    _m->setValue("t.ivec3", glm::ivec3(5,6,7));
    _m->setValue("t.ivec4", glm::ivec4(5,6,7,8));
    _m->setValue("t.uvec2", glm::uvec2(5,6));
    _m->setValue("t.uvec3", glm::uvec3(5,6,7));
    _m->setValue("t.uvec4", glm::uvec4(5,6,7,8));
    _m->setValue("t.bvec2", glm::bvec2(true));
    _m->setValue("t.bvec3", glm::bvec3(true));
    _m->setValue("t.bvec4", glm::bvec4(true));

    vectorClassHelper<glm::vec2>(_m, "t.vec2");
    vectorClassHelper<glm::vec3>(_m, "t.vec3");
    vectorClassHelper<glm::vec4>(_m, "t.vec4");
    vectorClassHelper<glm::dvec2>(_m, "t.dvec2");
    vectorClassHelper<glm::dvec3>(_m, "t.dvec3");
    vectorClassHelper<glm::dvec4>(_m, "t.dvec4");
    vectorClassHelper<glm::ivec2>(_m, "t.ivec2");
    vectorClassHelper<glm::ivec3>(_m, "t.ivec3");
    vectorClassHelper<glm::ivec4>(_m, "t.ivec4");
    vectorClassHelper<glm::uvec2>(_m, "t.uvec2");
    vectorClassHelper<glm::uvec3>(_m, "t.uvec3");
    vectorClassHelper<glm::uvec4>(_m, "t.uvec4");
    vectorClassHelper<glm::bvec2>(_m, "t.bvec2");
    vectorClassHelper<glm::bvec3>(_m, "t.bvec3");
    vectorClassHelper<glm::bvec4>(_m, "t.bvec4");
}

template <class T, typename U>
void matrixClassHelper(ghoul::ConfigurationManager* m, const std::string& key) {
    T value = T(0);
    const bool success = m->getValue(key, value);
    EXPECT_EQ(success, true) << "Type: " << typeid(T).name();

    const glm::detail::tmat4x4<U> res4 = glm::detail::tmat4x4<U>(
        5.0,  6.0, 7.0, 8.0,
        9.0, 10.0,11.0,12.0,
        13.0,14.0,15.0,16.0,
        17.0,18.0,19.0,20.0
        );

    const T res = T(res4);

    EXPECT_EQ(res, value) << "Type: " << typeid(T).name();
}

TEST_F(ConfigurationManagerTest, MatrixClassesGet) {
    _m->loadConfiguration(_configuration5);
    matrixClassHelper<glm::mat2x2, float>(_m, "m2x2");
    matrixClassHelper<glm::mat2x3, float>(_m, "m2x3");
    matrixClassHelper<glm::mat2x4, float>(_m, "m2x4");
    matrixClassHelper<glm::mat3x2, float>(_m, "m3x2");
    matrixClassHelper<glm::mat3x3, float>(_m, "m3x3");
    matrixClassHelper<glm::mat3x4, float>(_m, "m3x4");
    matrixClassHelper<glm::mat4x2, float>(_m, "m4x2");
    matrixClassHelper<glm::mat4x3, float>(_m, "m4x3");
    matrixClassHelper<glm::mat4x4, float>(_m, "m4x4");

    matrixClassHelper<glm::dmat2x2, double>(_m, "m2x2");
    matrixClassHelper<glm::dmat2x3, double>(_m, "m2x3");
    matrixClassHelper<glm::dmat2x4, double>(_m, "m2x4");
    matrixClassHelper<glm::dmat3x2, double>(_m, "m3x2");
    matrixClassHelper<glm::dmat3x3, double>(_m, "m3x3");
    matrixClassHelper<glm::dmat3x4, double>(_m, "m3x4");
    matrixClassHelper<glm::dmat4x2, double>(_m, "m4x2");
    matrixClassHelper<glm::dmat4x3, double>(_m, "m4x3");
    matrixClassHelper<glm::dmat4x4, double>(_m, "m4x4");
}

TEST_F(ConfigurationManagerTest, MatrixClassSet) {
    _m->setValue("f.m2x2", glm::mat2x2(5, 6, 9, 10));
    _m->setValue("f.m2x3", glm::mat2x3(5, 6, 7, 9, 10, 11));
    _m->setValue("f.m2x4", glm::mat2x4(5, 6, 7, 8, 9, 10, 11, 12));
    _m->setValue("f.m3x2", glm::mat3x2(5, 6, 9, 10, 13, 14));
    _m->setValue("f.m3x3", glm::mat3x3(5, 6, 7, 9, 10, 11, 13, 14, 15));
    _m->setValue("f.m3x4", glm::mat3x4(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16));
    _m->setValue("f.m4x2", glm::mat4x2(5, 6, 9, 10, 13, 14, 17, 18));
    _m->setValue("f.m4x3", glm::mat4x3(5, 6, 7, 9, 10, 11, 13, 14, 15, 17, 18, 19));
    _m->setValue("f.m4x4", glm::mat4x4(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20));

    matrixClassHelper<glm::mat2x2, float>(_m, "f.m2x2");
    matrixClassHelper<glm::mat2x3, float>(_m, "f.m2x3");
    matrixClassHelper<glm::mat2x4, float>(_m, "f.m2x4");
    matrixClassHelper<glm::mat3x2, float>(_m, "f.m3x2");
    matrixClassHelper<glm::mat3x3, float>(_m, "f.m3x3");
    matrixClassHelper<glm::mat3x4, float>(_m, "f.m3x4");
    matrixClassHelper<glm::mat4x2, float>(_m, "f.m4x2");
    matrixClassHelper<glm::mat4x3, float>(_m, "f.m4x3");
    matrixClassHelper<glm::mat4x4, float>(_m, "f.m4x4");

    _m->setValue("d.m2x2", glm::dmat2x2(5, 6, 9, 10));
    _m->setValue("d.m2x3", glm::dmat2x3(5, 6, 7, 9, 10, 11));
    _m->setValue("d.m2x4", glm::dmat2x4(5, 6, 7, 8, 9, 10, 11, 12));
    _m->setValue("d.m3x2", glm::dmat3x2(5, 6, 9, 10, 13, 14));
    _m->setValue("d.m3x3", glm::dmat3x3(5, 6, 7, 9, 10, 11, 13, 14, 15));
    _m->setValue("d.m3x4", glm::dmat3x4(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16));
    _m->setValue("d.m4x2", glm::dmat4x2(5, 6, 9, 10, 13, 14, 17, 18));
    _m->setValue("d.m4x3", glm::dmat4x3(5, 6, 7, 9, 10, 11, 13, 14, 15, 17, 18, 19));
    _m->setValue("d.m4x4", glm::dmat4x4(
                                        5,   6,  7,  8,
                                        9,  10, 11, 12,
                                        13, 14, 15, 16,
                                        17, 18, 19, 20));


    matrixClassHelper<glm::dmat2x2, double>(_m, "d.m2x2");
    matrixClassHelper<glm::dmat2x3, double>(_m, "d.m2x3");
    matrixClassHelper<glm::dmat2x4, double>(_m, "d.m2x4");
    matrixClassHelper<glm::dmat3x2, double>(_m, "d.m3x2");
    matrixClassHelper<glm::dmat3x3, double>(_m, "d.m3x3");
    matrixClassHelper<glm::dmat3x4, double>(_m, "d.m3x4");
    matrixClassHelper<glm::dmat4x2, double>(_m, "d.m4x2");
    matrixClassHelper<glm::dmat4x3, double>(_m, "d.m4x3");
    matrixClassHelper<glm::dmat4x4, double>(_m, "d.m4x4");
}
