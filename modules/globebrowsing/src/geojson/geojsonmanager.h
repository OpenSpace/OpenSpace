/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___GEOJSONMANAGER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___GEOJSONMANAGER___H__

#include <openspace/properties/propertyowner.h>

#include <modules/globebrowsing/src/geojson/geojsoncomponent.h>

#include <functional>
#include <memory>
#include <vector>

namespace ghoul { class Dictionary; }
namespace openspace { struct RenderData; }
namespace openspace::documentation { struct Documentation; }

namespace openspace::globebrowsing {

class RenderableGlobe;

/**
 * Manages multiple GeoJSON objects of a globe.
 */
class GeoJsonManager : public properties::PropertyOwner {
public:
    GeoJsonManager();

    void initialize(RenderableGlobe* globe);
    void deinitializeGL();

    bool isReady() const;

    void addGeoJsonLayer(const ghoul::Dictionary& layerDict);
    void deleteLayer(const std::string& layerIdentifier);

    void update();
    void render(const RenderData& data);

private:
    std::vector<std::unique_ptr<GeoJsonComponent>> _geoJsonObjects;
    RenderableGlobe* _parentGlobe = nullptr;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___GEOJSONMANAGER___H__
