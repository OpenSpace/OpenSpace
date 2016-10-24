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
#include <ghoul/ghoul.h>

#include <openspace/scripting/scriptengine.h>

#include <openspace/rendering/renderable.h>

#include <openspace/util/progressbar.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/configurationmanager.h>
#include <openspace/util/taskloader.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/task.h>
#include <openspace/scene/translation.h>
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

int main(int argc, char** argv) {
    using namespace openspace;

    ghoul::initialize();
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

    openspace::ConfigurationManager configuration;
    std::string configurationFile = configuration.findConfiguration(ConfigurationFile);
    configuration.loadFromFile(configurationFile);

    ModuleEngine moduleEngine;
    moduleEngine.initialize();

    std::string tasksPath;
    configuration.getValue(ConfigurationManager::KeyConfigTask, tasksPath);

    LINFO("Initialization done.");

    TaskLoader taskLoader;
    std::vector<std::unique_ptr<Task>> tasks = taskLoader.tasksFromFile(tasksPath);
    
    size_t nTasks = tasks.size();
    if (nTasks == 1) {
        LINFO("Task queue has 1 item");
    } else {
        LINFO("Task queue has " << tasks.size() << " items");
    }

    for (size_t i = 0; i < tasks.size(); i++) {
        Task& task = *tasks[i].get();
        LINFO("Performing task " << (i+1) << " out of " << tasks.size() << ": " << task.description());
        ProgressBar progressBar(100);
        auto onProgress = [&progressBar](float progress) {
            progressBar.print(progress * 100);
        };
        task.perform(onProgress);
    }

    std::cout << "Done performing tasks." << std::endl;

    std::cin.get();
    return 0;
};
