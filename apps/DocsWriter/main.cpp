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

#include <openspace/documentation/documentationengine.h>
#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/settings.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/ghoul.h>
#include <ghoul/logging/logmanager.h>

int main(int argc, char** argv) {
    using namespace openspace;

    ghoul::logging::LogManager::initialize(
        ghoul::logging::LogLevel::Debug,
        ghoul::logging::LogManager::ImmediateFlush::Yes
    );

    ghoul::initialize();
    global::create();

    // In order to initialize the engine, we need to specify the tokens
    // We start by registering the path of the executable,
    // to make it possible to find other files in the same directory
    FileSys.registerPathToken(
        "${BIN}",
        std::filesystem::path(argv[0]).parent_path(),
        ghoul::filesystem::FileSystem::Override::Yes
    );

    std::filesystem::path configFile = findConfiguration();

    // Register the base path as the directory where the configuration file lives
    std::filesystem::path base = configFile.parent_path();
    FileSys.registerPathToken("${BASE}", base);

    *global::configuration = loadConfigurationFromFile(
        configFile.string(),
        "",
        glm::ivec2(0)
    );
    openspace::global::openSpaceEngine->registerPathTokens();

    // Now that we have the tokens we can initialize the engine
    global::openSpaceEngine->initialize();

    // Print out the documentation to the documentation folder
    // @TODO (ylvse, 2024-05-02) change this directory when integrating with jenkins?
    DocEng.writeJsonDocumentation();

    return 0;
};
