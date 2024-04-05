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

#include <openspace/util/taskloader.h>

#include <openspace/documentation/documentation.h>
#include <openspace/util/task.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/misc/dictionary.h>
#include <algorithm>
#include <filesystem>

namespace {
    constexpr std::string_view _loggerCat = "TaskRunner";
} // namespace

namespace openspace {

std::vector<std::unique_ptr<Task>> TaskLoader::tasksFromDictionary(
                                                      const ghoul::Dictionary& dictionary)
{
    std::vector<std::unique_ptr<Task>> tasks;

    for (const std::string_view key : dictionary.keys()) {
        if (dictionary.hasValue<std::string>(key)) {
            const std::string taskName = dictionary.value<std::string>(key);
            const std::string path = taskName + ".task";
            std::vector<std::unique_ptr<Task>> subTasks = tasksFromFile(path);
            std::move(subTasks.begin(), subTasks.end(), std::back_inserter(tasks));
        }
        else if (dictionary.hasValue<ghoul::Dictionary>(key)) {
            const ghoul::Dictionary subTask = dictionary.value<ghoul::Dictionary>(key);
            const std::string& taskType = subTask.value<std::string>("Type");
            std::unique_ptr<Task> task = Task::createFromDictionary(subTask);
            if (!task) {
                LERROR(std::format(
                    "Failed to create a Task object of type '{}'", taskType
                ));
            }
            tasks.push_back(std::move(task));
        }
    }
    return tasks;
}

std::vector<std::unique_ptr<Task>> TaskLoader::tasksFromFile(const std::string& path) {
    std::filesystem::path absTasksFile = absPath(path);
    if (!std::filesystem::is_regular_file(absTasksFile)) {
        LERROR(std::format(
            "Could not load tasks file '{}'. File not found", absTasksFile
        ));
        return std::vector<std::unique_ptr<Task>>();
    }

    ghoul::Dictionary tasksDictionary;
    try {
        ghoul::lua::loadDictionaryFromFile(absTasksFile.string(), tasksDictionary);
    }
    catch (const ghoul::RuntimeError& e) {
        LERROR(std::format(
            "Could not load tasks file '{}'. Lua error: ({}) {}",
            absTasksFile, e.message, e.component
        ));
        return std::vector<std::unique_ptr<Task>>();
    }

    try {
        return tasksFromDictionary(tasksDictionary);
    }
    catch (const documentation::SpecificationError& e) {
        LERROR(std::format("Could not load tasks file '{}': {}", absTasksFile, e.what()));
        logError(e);

        return std::vector<std::unique_ptr<Task>>();
    }
}

} // namespace openspace
