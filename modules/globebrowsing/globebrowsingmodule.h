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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___GLOBEBROWSING_MODULE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___GLOBEBROWSING_MODULE___H__

#include <openspace/util/openspacemodule.h>

#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/uintproperty.h>
#include <openspace/util/ellipsoid.h>
#include <ghoul/glm.h>
#include <future>
#include <memory>
#include <optional>

namespace openspace::globebrowsing {
    class RenderableGlobe;
    struct TileIndex;

    namespace cache { class MemoryAwareTileCache; }
} // namespace openspace::globebrowsing

namespace openspace {

class Camera;
struct Geodetic2;
struct Geodetic3;
class SceneGraphNode;

class GlobeBrowsingModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "GlobeBrowsing";

    GlobeBrowsingModule();

    void goToChunk(const SceneGraphNode& globe, int x, int y, int level);

    globebrowsing::cache::MemoryAwareTileCache* tileCache();
    scripting::LuaLibrary luaLibrary() const override;
    std::vector<documentation::Documentation> documentations() const override;
    static documentation::Documentation Documentation();

    const globebrowsing::RenderableGlobe* castFocusNodeRenderableToGlobe();

    struct Layer {
        std::string name;
        std::string url;
    };
    using Capabilities = std::vector<Layer>;

    // Stores the mapping between globe to names
    struct UrlInfo {
        std::string name;
        std::string url;
    };

    // Registers then user-usable name
    void loadWMSCapabilities(std::string name, std::string globe, std::string url);
    Capabilities capabilities(const std::string& name);

    std::vector<UrlInfo> urlInfo(const std::string& globe) const;
    bool hasUrlInfo(const std::string& globe) const;

    void removeWMSServer(const std::string& name);

    bool isMRFCachingEnabled() const;
    std::string mrfCacheLocation() const;

    bool hasDefaultGeoPointTexture() const;
    std::string_view defaultGeoPointTexture() const;

protected:
    void internalInitialize(const ghoul::Dictionary&) override;

private:
    properties::UIntProperty _tileCacheSizeMB;

    properties::StringProperty _defaultGeoPointTexturePath;
    properties::BoolProperty _mrfCacheEnabled;
    properties::StringProperty _mrfCacheLocation;

    std::unique_ptr<globebrowsing::cache::MemoryAwareTileCache> _tileCache;

    // name -> capabilities
    std::map<std::string, std::future<Capabilities>> _inFlightCapabilitiesMap;
    // name -> capabilities
    std::map<std::string, Capabilities> _capabilitiesMap;

    std::multimap<std::string, UrlInfo> _urlList;

    bool _hasDefaultGeoPointTexture = false;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___GLOBEBROWSING_MODULE___H__
