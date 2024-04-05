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

#include <openspace/engine/configuration.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/settings.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/engine/configuration.h>
#include <openspace/util/factorymanager.h>
#include <openspace/engine/globals.h>
#include <openspace/util/progressbar.h>
#include <openspace/util/resourcesynchronization.h>
#include <openspace/util/task.h>
#include <openspace/util/taskloader.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/ghoul.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/logging/consolelog.h>

int main(int, char**) {
    using namespace openspace;

    ghoul::initialize();

    std::filesystem::path configFile = findConfiguration();
    std::filesystem::path settings = findSettings();
    *global::configuration = loadConfigurationFromFile(
        configFile,
        settings,
        glm::ivec2(0, 0)
    );
    global::openSpaceEngine->initialize();


    TaskLoader taskLoader;
    std::filesystem::path t = absPath("${TASKS}/full_sync.task");
    std::vector<std::unique_ptr<Task>> tasks = taskLoader.tasksFromFile(t.string());

    for (size_t i = 0; i < tasks.size(); i++) {
        Task& task = *tasks[i].get();
        LINFOC(
            "Sync",
            std::format(
                "Synchronizing scene {} out of {}: {}",
                i + 1, tasks.size(), task.description()
            )
        );
        ProgressBar progressBar(100);
        task.perform([&progressBar](float progress) {
            progressBar.print(static_cast<int>(progress * 100.f));
        });
    }
    std::cout << "Done synchronizing" << std::endl;

    return 0;
};
