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

#include <openspace/scene/sceneinitializer.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/loadingscreen.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/logging/logmanager.h>

namespace openspace {

void SingleThreadedSceneInitializer::initializeNode(SceneGraphNode* node) {
    node->initialize();
    _initializedNodes.push_back(node);
}

std::vector<SceneGraphNode*> SingleThreadedSceneInitializer::takeInitializedNodes() {
    std::vector<SceneGraphNode*> nodes = std::move(_initializedNodes);
    return nodes;
}

bool SingleThreadedSceneInitializer::isInitializing() const {
    return false;
}

MultiThreadedSceneInitializer::MultiThreadedSceneInitializer(unsigned int nThreads)
    : _threadPool(nThreads)
{}

void MultiThreadedSceneInitializer::initializeNode(SceneGraphNode* node) {
    ZoneScoped;

    auto initFunction = [this, node]() {
        ZoneScopedN("MultiThreadedInit");

        LoadingScreen* loadingScreen = global::openSpaceEngine->loadingScreen();

        LoadingScreen::ProgressInfo progressInfo;
        progressInfo.progress = 1.f;
        if (loadingScreen) {
            loadingScreen->updateItem(
                node->identifier(),
                node->guiName(),
                LoadingScreen::ItemStatus::Initializing,
                progressInfo
            );
        }

        try {
            node->initialize();
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.message);
        }
        const std::lock_guard g(_mutex);
        _initializedNodes.push_back(node);
        _initializingNodes.erase(node);

        if (loadingScreen) {
            loadingScreen->updateItem(
                node->identifier(),
                node->guiName(),
                LoadingScreen::ItemStatus::Finished,
                progressInfo
            );
        }
    };

    LoadingScreen::ProgressInfo progressInfo;
    progressInfo.progress = 0.f;

    LoadingScreen* loadingScreen = global::openSpaceEngine->loadingScreen();
    if (loadingScreen) {
        loadingScreen->updateItem(
            node->identifier(),
            node->guiName(),
            LoadingScreen::ItemStatus::Started,
            progressInfo
        );
    }

    const std::lock_guard g(_mutex);
    _initializingNodes.insert(node);
    _threadPool.enqueue(initFunction);
}

std::vector<SceneGraphNode*> MultiThreadedSceneInitializer::takeInitializedNodes() {
    // Some of the scene graph nodes might still be in the initialization queue and we
    // should wait for those to finish or we end up in some half-initialized state since
    // other parts of the application already know about their existence
    while (_threadPool.hasOutstandingTasks()) {
        std::this_thread::sleep_for(std::chrono::milliseconds(1));
    }

    const std::lock_guard g(_mutex);
    std::vector<SceneGraphNode*> nodes = std::move(_initializedNodes);
    return nodes;
}

bool MultiThreadedSceneInitializer::isInitializing() const {
    const std::lock_guard g(_mutex);
    return !_initializingNodes.empty();
}

} // namespace openspace
