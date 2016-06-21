/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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


#include <ghoul/misc/assert.h>

#include <openspace/engine/openspaceengine.h>

#include <modules/globebrowsing/chunk/chunk.h>
#include <modules/globebrowsing/chunk/chunkedlodglobe.h>
#include <modules/globebrowsing/tile/layeredtextures.h>

#include <algorithm>

namespace {
    const std::string _loggerCat = "Chunk";
}

namespace openspace {

    Chunk::Chunk(ChunkedLodGlobe* owner, const ChunkIndex& chunkIndex, bool initVisible)
        : _owner(owner)
        , _surfacePatch(chunkIndex)
        , _index(chunkIndex)
        , _isVisible(initVisible) 
    {

    }

    const GeodeticPatch& Chunk::surfacePatch() const {
        return _surfacePatch;
    }

    ChunkedLodGlobe* const Chunk::owner() const {
        return _owner;
    }

    const ChunkIndex Chunk::index() const {
        return _index;
    }

    bool Chunk::isVisible() const {
        return _isVisible;
    }

    void Chunk::setIndex(const ChunkIndex& index) {
        _index = index;
        _surfacePatch = GeodeticPatch(index);
    }

    void Chunk::setOwner(ChunkedLodGlobe* newOwner) {
        _owner = newOwner;
    }

    Chunk::Status Chunk::update(const RenderData& data) {
        auto savedCamera = _owner->getSavedCamera();
        const Camera& camRef = savedCamera != nullptr ? *savedCamera : data.camera;
        RenderData myRenderData = { camRef, data.position, data.doPerformanceMeasurement };


        _isVisible = true;
        if (_owner->testIfCullable(*this, myRenderData)) {
            _isVisible = false;
            return Status::WANT_MERGE;
        }

        int desiredLevel = _owner->getDesiredLevel(*this, myRenderData);

        if (desiredLevel < _index.level) return Status::WANT_MERGE;
        else if (_index.level < desiredLevel) return Status::WANT_SPLIT;
        else return Status::DO_NOTHING;
    }

    Chunk::BoundingHeights Chunk::getBoundingHeights() const {
        BoundingHeights boundingHeights;
        boundingHeights.max = 0;
        boundingHeights.min = 0;
        boundingHeights.available = false;

        // In the future, this should be abstracted away and more easily queryable.
        // One must also handle how to sample pick one out of multiplte heightmaps
        auto tileProvidermanager = owner()->getTileProviderManager();
        auto heightMapProviders = tileProvidermanager->getActivatedLayerCategory(LayeredTextures::HeightMaps);
        if (heightMapProviders.size() > 0) {
            TileAndTransform tileAndTransform = TileSelector::getHighestResolutionTile(heightMapProviders[0].get(), _index);
            if (tileAndTransform.tile.status == Tile::Status::OK) {
                std::shared_ptr<TilePreprocessData> preprocessData = tileAndTransform.tile.preprocessData;
                if ((preprocessData != nullptr) && preprocessData->maxValues.size() > 0) {
                    boundingHeights.max = preprocessData->maxValues[0];
                    boundingHeights.min = preprocessData->minValues[0];
                    boundingHeights.available = true;
                }
            }
        }

        return boundingHeights;
    }

    std::vector<glm::dvec4> Chunk::getBoundingPolyhedronCorners() const {
        // OBS!
        // This implementation needs to be fixed! Its not completely bounding
        // See DebugRenderer::renderBoxFaces to see whats wrong

        const Ellipsoid& ellipsoid = owner()->ellipsoid();
        const GeodeticPatch& patch = surfacePatch();

        BoundingHeights boundingHeight = getBoundingHeights();

        // assume worst case
        double patchCenterRadius = ellipsoid.maximumRadius();

        double maxCenterRadius = patchCenterRadius + boundingHeight.max;
        Geodetic2 halfSize = patch.halfSize();

        // As the patch is curved, the maximum height offsets at the corners must be long 
        // enough to cover large enough to cover a boundingHeight.max at the center of the 
        // patch. Below this is done by an approximation.
        double scaleToCoverCenter = 1 / cos(halfSize.lat) + 1 / cos(halfSize.lon) - 1; 
        double maxCornerHeight = maxCenterRadius * scaleToCoverCenter - patchCenterRadius;

        bool chunkIsNorthOfEquator = patch.isNorthern();

        // The minimum height offset, however, we can simply 
        double minCornerHeight = boundingHeight.min;
        std::vector<glm::dvec4> corners(8);
        
        Scalar latCloseToEquator = patch.edgeLatitudeNearestEquator();
        Geodetic3 p1Geodetic = { { latCloseToEquator, patch.minLon() }, maxCornerHeight };
        Geodetic3 p2Geodetic = { { latCloseToEquator, patch.maxLon() }, maxCornerHeight };
        
        glm::vec3 p1 = ellipsoid.cartesianPosition(p1Geodetic);
        glm::vec3 p2 = ellipsoid.cartesianPosition(p2Geodetic);
        glm::vec3 p = 0.5f * (p1 + p2);
        Geodetic2 pGeodetic = ellipsoid.cartesianToGeodetic2(p);
        Scalar latDiff = latCloseToEquator - pGeodetic.lat;

        for (size_t i = 0; i < 8; i++) {
            Quad q = (Quad)(i % 4);
            double cornerHeight = i < 4 ? minCornerHeight : maxCornerHeight;
            Geodetic3 cornerGeodetic = { patch.getCorner(q), cornerHeight };
            
            bool cornerIsNorthern = i < 2;
            bool cornerCloseToEquator = chunkIsNorthOfEquator ^ cornerIsNorthern;
            if (cornerCloseToEquator) {
                cornerGeodetic.geodetic2.lat += latDiff;
            }

            corners[i] = dvec4(ellipsoid.cartesianPosition(cornerGeodetic), 1);
        }
        return corners;
    }


    void Chunk::render(const RenderData& data) const {
        _owner->getPatchRenderer().renderChunk(*this, data);
    }



} // namespace openspace
