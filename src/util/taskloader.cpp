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

#include <openspace/util/taskloader.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/onscopeexit.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/scripting/scriptengine.h>
#include <algorithm>

namespace {
    const std::string _loggerCat = "TaskRunner";
}

namespace openspace {


std::vector<std::unique_ptr<Task>> TaskLoader::tasksFromDictionary(const ghoul::Dictionary& tasksDictionary) {
    std::vector<std::unique_ptr<Task>> tasks;
    std::vector<std::string> keys = tasksDictionary.keys();
    for (const std::string key : keys) {
        std::string taskName;
        ghoul::Dictionary subTask;
        if (tasksDictionary.getValue(key, taskName)) {
            std::string path = "${TASKS}/" + taskName + ".task";
            std::vector<std::unique_ptr<Task>> subTasks = tasksFromFile(path);
            std::move(subTasks.begin(), subTasks.end(), std::back_inserter(tasks));
        } else if (tasksDictionary.getValue(key, subTask)) {
            std::string taskType = subTask.value<std::string>("Type");
            std::unique_ptr<Task> task = Task::createFromDictionary(subTask);
            if (task == nullptr) {
                LERROR("Failed to create a Task object of type '" << taskType << "'");
            }
            tasks.push_back(std::move(task));
        }
    }
    return tasks;
}

std::vector<std::unique_ptr<Task>> TaskLoader::tasksFromFile(const std::string& path) {
    std::string absTasksFile = absPath(path);
    using RawPath = ghoul::filesystem::FileSystem::RawPath;
    if (!FileSys.fileExists(absTasksFile, RawPath::Yes)) {
        LERROR("Could not load tasks file '" << absTasksFile << "'. " <<
            "File not found");
        return std::vector<std::unique_ptr<Task>>();
    }

    ghoul::Dictionary tasksDictionary;
    try {
        ghoul::lua::loadDictionaryFromFile(
            absTasksFile,
            tasksDictionary
            );
    } catch (...) {
        LERROR("Could not load tasks file '" << absTasksFile << "'. " <<
            "Lua parse error");
        return std::vector<std::unique_ptr<Task>>();
    }
    return tasksFromDictionary(tasksDictionary);
}


}