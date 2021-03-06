/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/sync/tasks/syncassettask.h>

#include <openspace/openspace.h>
#include <openspace/documentation/core_registration.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/globals.h>
#include <openspace/scene/assetloader.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/openspacemodule.h>
#include <openspace/util/synchronizationwatcher.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/luastate.h>

#include <chrono>
#include <filesystem>
#include <fstream>
#include <string>
#include <thread>

namespace {
    constexpr std::chrono::milliseconds ProgressPollInterval(200);

    struct [[codegen::Dictionary(SyncAssetTask)]] Parameters {
        // The asset file to sync
        std::filesystem::path asset;
    };
#include "syncassettask_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation SyncAssetTask::documentation() {
    return codegen::doc<Parameters>("sync_asset_task");
}

SyncAssetTask::SyncAssetTask(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);
    _asset = p.asset.string();
}

std::string SyncAssetTask::description() {
    return "Synchronize asset " + _asset;
}

void SyncAssetTask::perform(const Task::ProgressCallback& progressCallback) {
    SynchronizationWatcher watcher;

    scripting::ScriptEngine scriptEngine;

    registerCoreClasses(scriptEngine);

    for (OpenSpaceModule* m : global::moduleEngine->modules()) {
        scriptEngine.addLibrary(m->luaLibrary());

        for (scripting::LuaLibrary& l : m->luaLibraries()) {
            scriptEngine.addLibrary(l);
        }
    }

    scriptEngine.initialize();

    ghoul::lua::LuaState luaState;
    scriptEngine.initializeLuaState(luaState);

    AssetLoader loader(&luaState, &watcher, "${ASSETS}");

    loader.add(_asset);
    loader.rootAsset().startSynchronizations();

    std::vector<const Asset*> allAssets = loader.rootAsset().subTreeAssets();

    while (true) {
        bool inProgress = false;
        for (const Asset* asset : allAssets) {
            Asset::State state = asset->state();
            if (state == Asset::State::Unloaded ||
                state == Asset::State::Loaded ||
                state == Asset::State::Synchronizing)
            {
                inProgress = true;
            }
        }
        progressCallback(loader.rootAsset().requestedSynchronizationProgress());
        std::this_thread::sleep_for(ProgressPollInterval);
        watcher.notify();
        if (!inProgress) {
            return;
        }
    }
    progressCallback(1.f);
}

} // namespace openspace
