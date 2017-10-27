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

#include <openspace/scene/assetmanager.h>

#include <openspace/scripting/script_helper.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/file.h>

#include "assetmanager_lua.inl"

namespace {
    const char* _loggerCat = "AssetManager";
}

namespace openspace {
AssetManager::AssetManager(std::unique_ptr<AssetLoader> loader, 
   std::unique_ptr<AssetSynchronizer> synchronizer)
{

}

void AssetManager::update() {
    // 1. Check clear flag. 

    for (const auto& c : _pendingStateChangeCommands) {
        const std::string& path = c.first;
        const AssetState targetState = c.second;

        AssetState currentState = AssetState::Unloaded;
               


        switch (currentState) {
        case AssetState::Unloaded:
            if (_assetLoader->rootAsset)
            _assetLoader->unloadAsset(path);
            break;
        case AssetState::Loaded:
            _assetLoader->loadAsset(path);
            break;
        case AssetState::Synchronized:
            //_assetSynchronizer->addAsset();
            break;
        case AssetState::Initialized:

            break;
        }
    }

//    startSynchronizations();
//    handleFinishedSynchronizations();
//    handleLoading();
//    handleInitialization();
}

void AssetManager::setTargetAssetState(const std::string& path, AssetState targetState) {
    ghoul::filesystem::File file(absPath(path));
    std::string normalizedPath = file.path();
    _pendingStateChangeCommands[normalizedPath] = targetState;
}

void AssetManager::clearAllTargetAssets() {
    _pendingStateChangeCommands.clear();
    _shouldClearAssets = true;
}

std::vector<std::shared_ptr<Asset>> AssetManager::allAssets()
{
    return std::vector<std::shared_ptr<Asset>>();
}

scripting::LuaLibrary AssetManager::luaLibrary() {
    return {
        "",
        {
            {
                "importAsset",
                &luascriptfunctions::importAsset,
                {this},
                "string",
                ""
            },
            {
                "unimportAsset",
                &luascriptfunctions::unimportAsset,
                {this},
                "string",
                ""
            }
        }
    };
}


}
