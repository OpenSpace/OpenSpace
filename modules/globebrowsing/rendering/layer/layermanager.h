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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___LAYERMANAGER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___LAYERMANAGER___H__

#include <openspace/properties/propertyowner.h>

#include <modules/globebrowsing/rendering/layer/layergroupid.h>
#include <modules/globebrowsing/rendering/layer/layer.h>
#include <modules/globebrowsing/tile/chunktile.h>
#include <modules/globebrowsing/tile/tiletextureinitdata.h>

#include <functional>

namespace openspace {
namespace globebrowsing {
    
struct LayerGroup;

/**
 * Manages multiple LayerGroups.
 */
class LayerManager : public properties::PropertyOwner  {
public:
    LayerManager(const ghoul::Dictionary& textureCategoriesDictionary);

    void addLayer(layergroupid::GroupID groupId, ghoul::Dictionary layerDict);
    void deleteLayer(layergroupid::GroupID groupId, std::string layerName);

    const LayerGroup& layerGroup(size_t groupId);
    const LayerGroup& layerGroup(layergroupid::GroupID);

    bool hasAnyBlendingLayersEnabled() const;

    const std::vector<std::shared_ptr<LayerGroup>>& layerGroups() const;

    void update();
    void reset(bool includingDisabled = false);

    static TileTextureInitData getTileTextureInitData(layergroupid::GroupID id,
        size_t preferredTileSize = 0);

    static bool shouldPerformPreProcessingOnLayergroup(layergroupid::GroupID id);
    void onChange(std::function<void(void)> callback);

private:
    std::vector<std::shared_ptr<LayerGroup>> _layerGroups;
};

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___LAYERMANAGER___H__
