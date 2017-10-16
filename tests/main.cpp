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

#if defined(WIN32)
#pragma warning (push)
#pragma warning (disable : 4619) // #pragma warning: there is no warning number '4800'
#elif defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wundef"
#pragma clang diagnostic ignored "-Wmissing-noreturn"
#pragma clang diagnostic ignored "-Wshift-sign-overflow"
#elif defined(__GNUC__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wundef"
#pragma GCC diagnostic ignored "-Wmissing-noreturn" 
#pragma GCC diagnostic ignored "-Wsign-compare"
#pragma GCC diagnostic ignored "-Wzero-as-null-pointer-constant"
#pragma GCC diagnostic ignored "-Wsuggest-override"
#endif // __GNUC__

#include "gtest/gtest.h"

// When running the unit tests we don't want to be asked what to do in the case of an
// assertion
#ifndef GHL_THROW_ON_ASSERT
#define GHL_THROW_ON_ASSERT
#endif // GHL_THROW_ON_ASSERTGHL_THROW_ON_ASSERT

#include <ghoul/cmdparser/cmdparser>
#include <ghoul/filesystem/filesystem>
#include <ghoul/logging/logging>
#include <ghoul/misc/dictionary.h>
#include <ghoul/lua/ghoul_lua.h>

// test files
#include <test_common.inl>
#include <test_spicemanager.inl>
#include <test_assetloader.inl>
#include <test_timeline.inl>

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
//#include <test_chunknode.inl>
#include <test_lrucache.inl>
#include <test_aabb.inl>

#include <test_angle.inl>
//#include <test_latlonpatch.inl>
#include <test_gdalwms.inl>
//#include <test_patchcoverageprovider.inl>

#include <test_concurrentqueue.inl>
#include <test_concurrentjobmanager.inl>
#endif

#include <test_luaconversions.inl>
#include <test_powerscalecoordinates.inl>

#ifdef OPENSPACE_MODULE_ISWA_ENABLED
#include <test_screenspaceimage.inl>
//#include <test_iswamanager.inl>
#endif

#ifdef OPENSPACE_MODULE_VOLUME_ENABLED
#include <test_rawvolumeio.inl>
#endif

#include <test_scriptscheduler.inl>

#include <test_documentation.inl>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/engine/configurationmanager.h>

#include <openspace/util/factorymanager.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>

#include <iostream>

using namespace ghoul::cmdparser;
using namespace ghoul::filesystem;
using namespace ghoul::logging;

//#define PRINT_OUTPUT

namespace {
    std::string _loggerCat = "OpenSpaceTest";
}

int main(int argc, char** argv) {
    std::vector<std::string> args;
    bool close;
    bool consoleLog = false;


    // Workaround for Visual Studio Google Test Adapter:
    // Do not try to initialize osengine if gtest is just listing tests
    bool skipOsEng = false;
    std::vector<std::string> gtestArgs(argv, argv + argc);
    if (std::find(gtestArgs.begin(), gtestArgs.end(), "--gtest_list_tests") != gtestArgs.end()) {
        skipOsEng = true;
    }

    if (!skipOsEng) {
        openspace::OpenSpaceEngine::create(argc, argv, std::make_unique<openspace::WindowWrapper>(), args, close, consoleLog);
    }

    testing::InitGoogleTest(&argc, argv);

#ifdef PRINT_OUTPUT
    testing::internal::CaptureStdout();
    testing::internal::CaptureStderr();
#endif

#ifdef PRINT_OUTPUT

    // Stop capturing std out
    std::string output = testing::internal::GetCapturedStdout();
    std::string error = testing::internal::GetCapturedStderr();

    //std::cout << output;
    //std::cerr << error;
#endif
        
    //openspace::SpiceManager::deinitialize();

    bool b = RUN_ALL_TESTS();

#ifdef PRINT_OUTPUT
    std::string output = testing::internal::GetCapturedStdout();
    std::string error = testing::internal::GetCapturedStderr();

    std::ofstream o("output.txt");
    o << output;

    std::ofstream e("error.txt");
    e << error;
#endif   
    return b;
}

#ifdef WIN32
#pragma warning (pop)
#elif defined(__clang__)
#pragma clang diagnostic pop
#elif defined(__GNUC__)
#pragma GCC diagnostic pop
#endif // __GNUC__
