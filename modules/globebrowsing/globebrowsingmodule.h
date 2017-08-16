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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___GLOBEBROWSING_MODULE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___GLOBEBROWSING_MODULE___H__

#include <openspace/util/openspacemodule.h>
#include <ghoul/glm.h>
#include <memory>

namespace openspace::globebrowsing {
    class RenderableGlobe;
    struct TileIndex;
    struct Geodetic2;
    struct Geodetic3;

    namespace cache { class MemoryAwareTileCache; }
} // namespace openspace::globebrowsing

namespace openspace {

class Camera;

class GlobeBrowsingModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "GlobeBrowsing";

    GlobeBrowsingModule();

    void goToChunk(int x, int y, int level);
    void goToGeo(double latitude, double longitude);
    void goToGeo(double latitude, double longitude, double altitude);

    globebrowsing::cache::MemoryAwareTileCache* tileCache();
    scripting::LuaLibrary luaLibrary() const override;
    globebrowsing::RenderableGlobe* castFocusNodeRenderableToGlobe();

protected:
    void internalInitialize() override;

private:
    
    void goToChunk(Camera& camera, globebrowsing::TileIndex ti, glm::vec2 uv,
                   bool resetCameraDirection);
    void goToGeodetic2(Camera& camera, globebrowsing::Geodetic2 geo2,
                       bool resetCameraDirection);
    void goToGeodetic3(Camera& camera, globebrowsing::Geodetic3 geo3,
                       bool resetCameraDirection);
    void resetCameraDirection(Camera& camera,  globebrowsing::Geodetic2 geo2);

    /**
     \return a comma separated list of layer group names.
     */
    static std::string layerGroupNamesList();

    /**
     \return a comma separated list of layer type names.
     */
    static std::string layerTypeNamesList();

    std::unique_ptr<globebrowsing::cache::MemoryAwareTileCache> _tileCache;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___GLOBEBROWSING_MODULE___H__
