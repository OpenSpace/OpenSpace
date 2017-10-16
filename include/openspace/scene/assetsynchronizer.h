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

#ifndef __OPENSPACE_CORE___ASSETSYNCHRONIZER___H__
#define __OPENSPACE_CORE___ASSETSYNCHRONIZER___H__

#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/asset.h>

#include <openspace/scripting/lualibrary.h>
#include <openspace/util/resourcesynchronizer.h>

#include <ghoul/misc/dictionary.h>
#include <ghoul/lua/luastate.h>

#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/filesystem/directory.h>

#include <memory>
#include <string>

namespace openspace {

class AssetSynchronizer : public ResourceSyncClient {
public:
    AssetSynchronizer(ResourceSynchronizer* resourceSynchronizer);
    void addAsset(std::shared_ptr<Asset> asset);
    void removeAsset(Asset* asset);
    void syncAsset(Asset* asset);

    std::vector<Asset*> getRecentlySynchronizedAssets();
private:
    enum class SynchronizationState : int {
        Added,
        Synchronizing,
        RecentlySynchronized,
        Synchronized
    };

    struct AssetSynchronization {
        std::shared_ptr<Asset> asset;
        SynchronizationState state;
    };

    ResourceSynchronizer* _resourceSynchronizer;
    std::vector<AssetSynchronization> _synchronizations;
    
    std::map<ResourceSynchronization*, Asset*> _resourceToAssetMap;
};




} // namespace openspace

#endif // __OPENSPACE_CORE___ASSETSYNCHRONIZER___H__
