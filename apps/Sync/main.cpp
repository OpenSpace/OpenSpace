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

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/progressbar.h>
#include <openspace/util/resourcesynchronization.h>
#include <openspace/util/taskloader.h>

#include <ghoul/ghoul.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/logging/consolelog.h>

// #include <iostream>
// #include <string>
// #include <ghoul/glm.h>

// #include <ghoul/opengl/ghoul_gl.h>
// #include <ghoul/io/texture/texturereader.h>
// #include <ghoul/io/texture/texturereaderdevil.h>
// #include <ghoul/io/texture/texturereaderfreeimage.h>
// #include <ghoul/filesystem/directory.h>
// #include <ghoul/cmdparser/commandlineparser.h>
// #include <ghoul/cmdparser/singlecommand.h>

// #include <openspace/scripting/scriptengine.h>
// #include <openspace/engine/openspaceengine.h>
// #include <openspace/engine/configurationmanager.h>
// #include <openspace/util/task.h>

namespace {
    constexpr const char* ConfigurationFile = "openspace.cfg";
    constexpr const char* _loggerCat = "Sync";
}

int main(int argc, char** argv) {
    using namespace openspace;

//    ghoul::initialize();
//
//    ghoul::logging::LogManager::initialize(
//        ghoul::logging::LogLevel::Debug,
//        ghoul::logging::LogManager::ImmediateFlush::Yes
//    );
//    LogMgr.addLog(std::make_unique<ghoul::logging::ConsoleLog>());
//
//    LDEBUG("Initialize FileSystem");
//
//    ghoul::filesystem::Directory launchDirectory = FileSys.currentDirectory();
//
//#ifdef __APPLE__
//    ghoul::filesystem::File app(argv[0]);
//    std::string dirName = app.directoryName();
//    LINFO("Setting starting directory to '" << dirName << "'");
//    FileSys.setCurrentDirectory(dirName);
//#endif

    // initTextureReaders();

    std::vector<std::string> unusedStringlist;
    bool unusedBool;
    OpenSpaceEngine::create(argc, argv, nullptr, unusedStringlist, unusedBool);

    /*openspace::ConfigurationManager configuration;
    std::string configurationFile = configuration.findConfiguration(ConfigurationFile);
    configuration.loadFromFile(configurationFile);

    ghoul::Dictionary moduleConfigurations;
    if (configuration.hasKeyAndValue<ghoul::Dictionary>(
        ConfigurationManager::KeyModuleConfigurations))
    {
        configuration.getValue<ghoul::Dictionary>(
            ConfigurationManager::KeyModuleConfigurations,
            moduleConfigurations
        );
    }*/

    //ModuleEngine moduleEngine;
    //moduleEngine.initialize(moduleConfigurations);
    //LINFO("Initialization done.");

    TaskLoader taskLoader;
    std::vector<std::unique_ptr<Task>> tasks = taskLoader.tasksFromFile(
        absPath("${TASKS}/full_sync.task")
    );

    for (size_t i = 0; i < tasks.size(); i++) {
        Task& task = *tasks[i].get();
        LINFO(
            "Synchronizing scene " << (i + 1) << " out of " <<
            tasks.size() << ": " << task.description()
        );
        ProgressBar progressBar(100);
        auto onProgress = [&progressBar](float progress) {
            progressBar.print(static_cast<int>(progress * 100.f));
        };
        task.perform(onProgress);
    }
    std::cout << "Done synchronizing." << std::endl;

    return 0;
};
