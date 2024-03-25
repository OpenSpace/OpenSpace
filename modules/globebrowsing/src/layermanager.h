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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___LAYERMANAGER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___LAYERMANAGER___H__

#include <openspace/properties/propertyowner.h>

#include <modules/globebrowsing/src/layergroupid.h>
#include <ghoul/misc/boolean.h>
#include <array>
#include <functional>
#include <memory>

namespace ghoul { class Dictionary; }

namespace openspace::documentation { struct Documentation; }

namespace openspace::globebrowsing {

class Layer;
struct LayerGroup;
class TileTextureInitData;

/**
 * Manages multiple LayerGroups.
 */
class LayerManager : public properties::PropertyOwner {
public:
    constexpr static size_t NumLayerGroups = layers::Groups.size();

    LayerManager();

    void initialize(const ghoul::Dictionary& layerGroupsDict);
    void deinitialize();

    Layer* addLayer(layers::Group::ID id, const ghoul::Dictionary& layerDict);
    void deleteLayer(layers::Group::ID id, const std::string& layerName);

    LayerGroup& layerGroup(layers::Group::ID groupId);
    const LayerGroup& layerGroup(layers::Group::ID groupId) const;

    bool hasAnyBlendingLayersEnabled() const;

    std::array<LayerGroup*, NumLayerGroups> layerGroups() const;

    void update();
    void reset(bool includeDisabled = false);

    void onChange(const std::function<void(Layer* l)>& callback);

    static documentation::Documentation Documentation();

private:
    std::array<std::unique_ptr<LayerGroup>, NumLayerGroups> _layerGroups;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___LAYERMANAGER___H__
