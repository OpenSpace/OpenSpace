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

#include <catch2/catch_session.hpp>

#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/ghoul.h>
#include <filesystem>
#include <iostream>

int main(int argc, char** argv) {
    using namespace openspace;

    ghoul::logging::LogManager::initialize(
        ghoul::logging::LogLevel::Info,
        ghoul::logging::LogManager::ImmediateFlush::Yes
    );
    ghoul::initialize();
    global::create();

    // Register the path of the executable,
    // to make it possible to find other files in the same directory.
    FileSys.registerPathToken(
        "${BIN}",
        std::filesystem::path(argv[0]).parent_path(),
        ghoul::filesystem::FileSystem::Override::Yes
    );

    const std::filesystem::path configFile = findConfiguration();
    // Register the base path as the directory where 'filename' lives
    const std::filesystem::path base = configFile.parent_path();
    FileSys.registerPathToken("${BASE}", base);

    *global::configuration = loadConfigurationFromFile(configFile, "", glm::ivec2(0));
    global::openSpaceEngine->registerPathTokens();
    global::openSpaceEngine->initialize();

    ghoul::logging::LogManager::deinitialize();
    ghoul::logging::LogManager::initialize(
        ghoul::logging::LogLevel::Info,
        ghoul::logging::LogManager::ImmediateFlush::Yes
    );

    FileSys.registerPathToken("${TESTDIR}", "${BASE}/tests");

    // All of the relevant tests initialize the SpiceManager
    openspace::SpiceManager::deinitialize();


    const int result = Catch::Session().run(argc, argv);

    // And the deinitialization needs the SpiceManager to be initialized
    openspace::SpiceManager::initialize();
    global::openSpaceEngine->deinitialize();
    return result;
}
