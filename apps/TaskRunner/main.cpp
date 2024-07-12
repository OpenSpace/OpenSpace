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

#include <iostream>
#include <string>
#include <ghoul/glm.h>

#include <ghoul/ghoul.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/logging/consolelog.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/cmdparser/commandlineparser.h>
#include <ghoul/cmdparser/singlecommand.h>

#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/settings.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/dashboarditem.h>
#include <openspace/util/progressbar.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/taskloader.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/resourcesynchronization.h>
#include <openspace/util/task.h>
#include <openspace/scene/translation.h>
#include <openspace/scene/rotation.h>
#include <openspace/scene/scale.h>
#include <openspace/engine/moduleengine.h>
#ifdef WIN32
#include <Windows.h>
#endif // WIN32

namespace {
    const std::string ConfigurationFile = "openspace.cfg";
    const std::string _loggerCat = "TaskRunner Main";
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
        LINFO(std::format("Task queue has {} items", tasks.size()));
    }

    for (size_t i = 0; i < tasks.size(); i++) {
        Task& task = *tasks[i].get();
        LINFO(std::format(
            "Performing task {} out of {}: {}", i + 1, tasks.size(), task.description()
        ));
        ProgressBar progressBar(100);
        auto onProgress = [&progressBar](float progress) {
            progressBar.print(static_cast<int>(progress * 100.f));
        };
        task.perform(onProgress);
    }
    std::cout << "Done performing tasks" << std::endl;
}

int main(int argc, char** argv) {
    using namespace openspace;

    ghoul::logging::LogManager::initialize(
        ghoul::logging::LogLevel::Debug,
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

    std::filesystem::path configFile = findConfiguration();

    // Register the base path as the directory where the configuration file lives
    std::filesystem::path base = configFile.parent_path();
    constexpr std::string_view BasePathToken = "${BASE}";
    FileSys.registerPathToken(BasePathToken.data(), base);

    // Using same configuration for size as in apps/OpenSpace/main.cpp
    glm::ivec2 size = glm::ivec2(1920, 1080);
#ifdef WIN32
    DEVMODEW dm = { 0 };
    dm.dmSize = sizeof(DEVMODEW);
    BOOL success = EnumDisplaySettingsW(nullptr, ENUM_CURRENT_SETTINGS, &dm);
    if (success) {
        size.x = dm.dmPelsWidth;
        size.y = dm.dmPelsHeight;
    }
#endif // WIN32

    std::filesystem::path settings = findSettings();
    *global::configuration = loadConfigurationFromFile(
        configFile.string(),
        settings,
        size
    );
    openspace::global::openSpaceEngine->registerPathTokens();
    global::openSpaceEngine->initialize();

    ghoul::cmdparser::CommandlineParser commandlineParser(
        "OpenSpace TaskRunner",
        ghoul::cmdparser::CommandlineParser::AllowUnknownCommands::Yes
    );

    std::optional<std::string> tasksPath;
    commandlineParser.addCommand(
        std::make_unique<ghoul::cmdparser::SingleCommand<std::string>>(
            tasksPath,
            "--task",
            "-t",
            "Provides the path to a task file to execute"
        )
    );

    commandlineParser.setCommandLine({ argv, argv + argc });
    commandlineParser.execute();

    //FileSys.setCurrentDirectory(launchDirectory);

    if (tasksPath.has_value()) {
        performTasks(*tasksPath);
        return 0;
    }

    // If no task file was specified in as argument, run in CLI mode.

    LINFO(std::format("Task root: {}", absPath("${TASKS}")));
    std::filesystem::current_path(absPath("${TASKS}"));

    std::cout << "TASK > ";
    std::string t;
    while (std::cin >> t) {
        performTasks(t);
        std::cout << "TASK > ";
    }

    global::destroy();
    ghoul::deinitialize();
    return 0;
};
