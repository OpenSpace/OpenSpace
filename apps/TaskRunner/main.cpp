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

#include <iostream>
#include <string>
#include <ghoul/glm.h>

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/io/texture/texturereaderdevil.h>
#include <ghoul/io/texture/texturereaderfreeimage.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/directory.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/logging/consolelog.h>
#include <ghoul/ghoul.h>
#include <ghoul/cmdparser/commandlineparser.h>
#include <ghoul/cmdparser/singlecommand.h>

#include <openspace/scripting/scriptengine.h>
#include <openspace/rendering/renderable.h>
#include <openspace/util/progressbar.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/configurationmanager.h>
#include <openspace/util/taskloader.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/task.h>
#include <openspace/scene/translation.h>
#include <openspace/scene/rotation.h>
#include <openspace/scene/scale.h>
#include <openspace/engine/moduleengine.h>

namespace {
    const std::string ConfigurationFile = "openspace.cfg";
    const std::string _loggerCat = "TaskRunner Main";
}

void initTextureReaders() {
    #ifdef GHOUL_USE_DEVIL
        ghoul::io::TextureReader::ref().addReader(std::make_shared<ghoul::io::TextureReaderDevIL>());
    #endif // GHOUL_USE_DEVIL
    #ifdef GHOUL_USE_FREEIMAGE
        ghoul::io::TextureReader::ref().addReader(std::make_shared<ghoul::io::TextureReaderFreeImage>());
    #endif // GHOUL_USE_FREEIMAGE
}

void performTasks(const std::string& path) {
    using namespace openspace;

    TaskLoader taskLoader;
    std::vector<std::unique_ptr<Task>> tasks = taskLoader.tasksFromFile(path);

    size_t nTasks = tasks.size();
    if (nTasks == 1) {
        LINFO("Task queue has 1 item");
    }
    else {
        LINFO("Task queue has " << tasks.size() << " items");
    }

    for (size_t i = 0; i < tasks.size(); i++) {
        Task& task = *tasks[i].get();
        LINFO("Performing task " << (i + 1) << " out of " << tasks.size() << ": " << task.description());
        ProgressBar progressBar(100);
        auto onProgress = [&progressBar](float progress) {
            progressBar.print(progress * 100);
        };
        task.perform(onProgress);
    }
    std::cout << "Done performing tasks." << std::endl;
}

int main(int argc, char** argv) {
    using namespace openspace;

    ghoul::initialize();

    ghoul::logging::LogManager::initialize(
        ghoul::logging::LogLevel::Debug,
        ghoul::logging::LogManager::ImmediateFlush::Yes
    );
    LogMgr.addLog(std::make_unique< ghoul::logging::ConsoleLog>());

    LDEBUG("Initialize FileSystem");

    ghoul::filesystem::Directory launchDirectory = FileSys.currentDirectory();

#ifdef __APPLE__
    ghoul::filesystem::File app(argv[0]);
    std::string dirName = app.directoryName();
    LINFO("Setting starting directory to '" << dirName << "'");
    FileSys.setCurrentDirectory(dirName);
#endif

    initTextureReaders();

    FactoryManager::initialize();
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<Renderable>>(),
        "Renderable"
        );
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<Task>>(),
        "Task"
        );
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<Translation>>(),
        "Translation"
        );

    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<Rotation>>(),
        "Rotation"
        );

    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<Scale>>(),
        "Scale"
        );

    openspace::ConfigurationManager configuration;
    std::string configurationFile = configuration.findConfiguration(ConfigurationFile);
    configuration.loadFromFile(configurationFile);

    ModuleEngine moduleEngine;
    moduleEngine.initialize();
    LINFO("Initialization done.");

    // Parse commandline arguments
    std::vector<std::string> args(argv, argv + argc);

    ghoul::cmdparser::CommandlineParser commandlineParser(
        "OpenSpace TaskRunner",
        ghoul::cmdparser::CommandlineParser::AllowUnknownCommands::Yes
    );

    std::string tasksPath = "";
    commandlineParser.addCommand(
        std::make_unique<ghoul::cmdparser::SingleCommand<std::string>>(
            &tasksPath,
            "--task",
            "-t",
            "Provides the path to a task file to execute"
            )
    );

    commandlineParser.setCommandLine(args);
    commandlineParser.execute();

    FileSys.setCurrentDirectory(launchDirectory);

    if (tasksPath != "") {
        performTasks(tasksPath);
        return 0;
    }

    // If no task file was specified in as argument, run in CLI mode.

    std::string tasksRoot;
    if (configuration.getValue(ConfigurationManager::KeyConfigTasksRoot, tasksRoot)) {
        LINFO("Task root: " << tasksRoot);
        FileSys.setCurrentDirectory(ghoul::filesystem::Directory(absPath(tasksRoot)));
    }

    std::cout << "TASK >";
    while (std::cin >> tasksPath) {
        performTasks(tasksPath);
        std::cout << "TASK >";
    }

    return 0;
};
