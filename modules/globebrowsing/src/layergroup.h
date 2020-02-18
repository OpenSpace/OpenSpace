/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___LAYERGROUP___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___LAYERGROUP___H__

#include <openspace/properties/propertyowner.h>

#include <modules/globebrowsing/src/layergroupid.h>
#include <openspace/properties/scalar/boolproperty.h>

namespace openspace::globebrowsing {

class Layer;

namespace tileprovider { struct TileProvider; }

/**
 * Convenience class for dealing with multiple <code>Layer</code>s.
 */
struct LayerGroup : public properties::PropertyOwner {
    LayerGroup(layergroupid::GroupID id);

    void setLayersFromDict(const ghoul::Dictionary& dict);

    void initialize();
    void deinitialize();

    /// Updates all layers tile providers within this group
    /// Return:  Number of tiles that were updated
    int update();

    Layer* addLayer(const ghoul::Dictionary& layerDict);
    void deleteLayer(const std::string& layerName);
    void moveLayers(int oldPosition, int newPosition);

    /// @returns const vector of all layers
    std::vector<Layer*> layers() const;

    /// @returns const vector of all active layers
    const std::vector<Layer*>& activeLayers() const;

    /// @returns the size of the pile to be used in rendering of this layer
    int pileSize() const;

    bool layerBlendingEnabled() const;

    void onChange(std::function<void(Layer*)> callback);

private:
    const layergroupid::GroupID _groupId;
    std::vector<std::unique_ptr<Layer>> _layers;
    std::vector<Layer*> _activeLayers;

    properties::BoolProperty _levelBlendingEnabled;
    std::function<void(Layer*)> _onChangeCallback;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___LAYERGROUP___H__
