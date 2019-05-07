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

#ifndef __OPENSPACE_MODULE_ROVER_TERRAIN_RENDERER___CACHING_SURFACE_MODEL_PROVIDER__H_
#define __OPENSPACE_MODULE_ROVER_TERRAIN_RENDERER___CACHING_SURFACE_MODEL_PROVIDER__H_

#include <modules/roverterrainrenderer/renderable/asyncsurfacemodelprovider.h>
#include <modules/globebrowsing/src/lrucache.h>
#include <modules/globebrowsing/src/tileindex.h>
#include <modules/roverterrainrenderer/renderable/subsitemodels.h>
#include <modules/roverterrainrenderer/filehandler/subsite.h>

#include <ghoul/opengl/ghoul_gl.h>

#include <memory>

struct ProviderSubsiteKey {
    int level;
    std::string site;
    std::string drive;
    unsigned int providerID;

    bool operator==(const ProviderSubsiteKey& r) const {
        return (providerID == r.providerID) &&
               (level == r.level) && (site == r.site) &&
               (drive == r.drive);
    }
};

namespace openspace {

struct ProviderSubsiteHasher {
    /**
    Creates a hash which can be used as key in hash maps.
    First set the bits to be unique for all tiles.
    +-------+------------+-------+------------+
    | USAGE | BIT RANGE  | #BITS | MAX VALUE  |
    +-------+------------+-------+------------+
    | level |   0 -  5   |   5   |         31 |
    |     x |   5 - 35   |  30   | 1073741824 |
    |     y |  35 - 64   |  29   |  536870912 |
    +-------+------------+-------+------------+

    Bits are then shifted depending on the tile provider used.
    */
    unsigned long long operator()(const ProviderSubsiteKey& t) const {
        unsigned long long key = 0;
        const int site = std::stoi(t.site);
        const int drive = std::stoi(t.drive);
        key |= static_cast<unsigned long long>(t.level);
        key |= static_cast<unsigned long long>(site) << 5ULL;
        key |= static_cast<unsigned long long>(drive) << 35ULL;
        // Now the key is unique for all tiles, however not for all tile providers.
        // Add to the key depending on the tile provider to avoid some hash collisions.
        // (All hash collisions can not be avoided due to the limit in 64 bit for the
        // hash key)
        // Idea: make some offset in the place of the bits for the x value. Lesser chance
        // of having different x-value than having different tile provider ids.
        key += static_cast<unsigned long long>(t.providerID) << 25ULL;
        return key;
    }
};

using ModelCache = globebrowsing::cache::LRUCache<ProviderSubsiteKey, std::shared_ptr<SubsiteModels>, ProviderSubsiteHasher>;

class CachingSurfaceModelProvider {
public:
    CachingSurfaceModelProvider(Renderable* parent);

    std::vector<std::shared_ptr<SubsiteModels>> getModels(const std::vector<std::shared_ptr<Subsite>> Subsites, const int level);

    void update();

private:
    std::shared_ptr<AsyncSurfaceModelProvider> _asyncSurfaceModelProvider;
    std::shared_ptr<ModelCache> _modelCache;
    std::vector<int> getLevelsAbove(const std::vector<int> availableLevels, const int requestedLevel);

    void initModelsFromLoadedData();

    Renderable* _parent;

    Vertex* _vertexBufferData = nullptr;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_ROVER_TERRAIN_RENDERER___CACHING_SURFACEMODEL_PROVIDER__H_
