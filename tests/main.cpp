/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <ghoul/cmdparser/cmdparser>
#include <ghoul/filesystem/filesystem>
#include <ghoul/logging/logging>
#include <ghoul/misc/dictionary.h>
#include <ghoul/lua/ghoul_lua.h>

// test files
#include <test_common.inl>
#include <test_spicemanager.inl>
#include <test_sceneloader.inl>

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
//#include <test_chunknode.inl>
#include <test_lrucache.inl>
#include <test_aabb.inl>
#include <test_convexhull.inl>

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
    openspace::OpenSpaceEngine::create(argc, argv, std::make_unique<openspace::WindowWrapper>(), args);

    testing::InitGoogleTest(&argc, argv);

#ifdef PRINT_OUTPUT
    testing::internal::CaptureStdout();
    testing::internal::CaptureStderr();
#endif
    
    openspace::SpiceManager::deinitialize();

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
