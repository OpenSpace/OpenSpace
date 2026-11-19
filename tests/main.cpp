/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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
#include <openspace/filesystem/file.h>
#include <openspace/filesystem/filesystem.h>
#include <openspace/logging/logmanager.h>
#include <openspace/lua/lua.h>
#include <openspace/openspace.h>
#include <openspace/misc/dictionary.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <filesystem>
#include <iostream>

using namespace openspace;

int main(int argc, char** argv) {
    logging::LogManager::initialize(
        logging::LogLevel::Info,
        logging::LogManager::ImmediateFlush::Yes
    );
    initialize();
    global::create();

    // Register the path of the executable,
    // to make it possible to find other files in the same directory.
    FileSys.registerPathToken(
        "${BIN}",
        std::filesystem::path(argv[0]).parent_path(),
        filesystem::FileSystem::Override::Yes
    );

    const std::filesystem::path configFile = findConfiguration();
    // Register the base path as the directory where 'filename' lives
    const std::filesystem::path base = configFile.parent_path();
    FileSys.registerPathToken("${BASE}", base);

    *global::configuration = loadConfigurationFromFile(configFile, "");
    registerPathTokens(*global::configuration);
    global::openSpaceEngine->initialize();

    logging::LogManager::deinitialize();
    logging::LogManager::initialize(
        logging::LogLevel::Info,
        logging::LogManager::ImmediateFlush::Yes
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
