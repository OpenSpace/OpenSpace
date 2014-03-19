/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2014                                                               *
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

#include <openspace/tests/test_common.inl>
#include <openspace/tests/test_scenegraph.inl>
#include <openspace/tests/test_powerscalecoordinates.inl>

#include <ghoul/misc/dictionary.h>
#include <iostream>

using namespace ghoul::cmdparser;
using namespace ghoul::filesystem;
using namespace ghoul::logging;

int main(int argc, char** argv) {
    LogManager::initialize(LogManager::LogLevel::None);
    LogMgr.addLog(new ConsoleLog);

    FileSystem::initialize();

    const bool extDir = FileSys.directoryExists("../../../ext/ghoul/tests");
    if (extDir) {
        FileSys.registerPathToken("${SCRIPTS_DIR}", "../../../ext/ghoul/scripts");
        FileSys.registerPathToken("${TEST_DIR}", "../../../ext/ghoul/tests");
    }
    else {
        LFATALC("main", "Fix me");
    }
    
    openspace::OpenSpaceEngine::registerFilePaths();
    FileSys.registerPathToken("${TESTDIR}", "${BASE_PATH}/src/tests");
    
    openspace::Time::init();
    openspace::Spice::init();
    openspace::Spice::ref().loadDefaultKernels();
    openspace::FactoryManager::initialize();
    
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
