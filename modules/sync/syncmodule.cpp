/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/sync/syncmodule.h>

#include <modules/sync/syncs/httpsynchronization.h>
#include <modules/sync/syncs/torrentsynchronization.h>
#include <modules/sync/syncs/urlsynchronization.h>
#include <modules/sync/tasks/syncassettask.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/resourcesynchronization.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/templatefactory.h>

namespace {
    constexpr const char* KeyHttpSynchronizationRepositories =
        "HttpSynchronizationRepositories";
    constexpr const char* KeySynchronizationRoot = "SynchronizationRoot";
} // namespace

namespace openspace {

SyncModule::SyncModule() : OpenSpaceModule(Name) {}

void SyncModule::internalInitialize(const ghoul::Dictionary& configuration) {
    if (configuration.hasKey(KeyHttpSynchronizationRepositories)) {
        ghoul::Dictionary dictionary = configuration.value<ghoul::Dictionary>(
            KeyHttpSynchronizationRepositories
        );

        for (const std::string& key : dictionary.keys()) {
            _synchronizationRepositories.push_back(dictionary.value<std::string>(key));
        }
    }

    if (configuration.hasKey(KeySynchronizationRoot)) {
        _synchronizationRoot = configuration.value<std::string>(KeySynchronizationRoot);
    } else {
        LWARNINGC(
            "SyncModule",
            "No synchronization root specified. Disabling resource synchronization"
        );
        //_synchronizationEnabled = false;
        // TODO: Make it possible to disable synchronization manually.
        // Group root and enabled into a sync config object that can be passed to syncs.
    }

    auto fSynchronization = FactoryManager::ref().factory<ResourceSynchronization>();
    ghoul_assert(fSynchronization, "ResourceSynchronization factory was not created");

    fSynchronization->registerClass(
        "HttpSynchronization",
        [this](bool, const ghoul::Dictionary& dictionary) {
            return new HttpSynchronization(
                dictionary,
                _synchronizationRoot,
                _synchronizationRepositories
            );
        }
    );

    fSynchronization->registerClass(
        "TorrentSynchronization",
        [this](bool, const ghoul::Dictionary& dictionary) {
            return new TorrentSynchronization(
                dictionary,
                _synchronizationRoot,
                _torrentClient
            );
        }
    );

    fSynchronization->registerClass(
        "UrlSynchronization",
        [this](bool, const ghoul::Dictionary& dictionary) {
            return new UrlSynchronization(
                dictionary,
                _synchronizationRoot
            );
        }
    );

    auto fTask = FactoryManager::ref().factory<Task>();
    ghoul_assert(fTask, "No task factory existed");
    fTask->registerClass<SyncAssetTask>("SyncAssetTask");

    _torrentClient.initialize();

    global::callback::deinitialize.push_back([&]() { _torrentClient.deinitialize(); });
}

void SyncModule::internalDeinitialize() {
    _torrentClient.deinitialize();
}

std::string SyncModule::synchronizationRoot() const {
    return _synchronizationRoot;
}

void SyncModule::addHttpSynchronizationRepository(std::string repository) {
    _synchronizationRepositories.push_back(std::move(repository));
}

std::vector<std::string> SyncModule::httpSynchronizationRepositories() const {
    return _synchronizationRepositories;
}

std::vector<documentation::Documentation> SyncModule::documentations() const {
    return {
        HttpSynchronization::Documentation(),
        TorrentSynchronization::Documentation()
    };
}

} // namespace openspace
