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
        Camera* savedCamera = _owner->getSavedCamera();
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
        boundingHeights.max = _owner->chunkHeight;
        boundingHeights.min = 0;
        boundingHeights.available = false;

        // In the future, this should be abstracted away and more easily queryable.
        // One must also handle how to sample pick one out of multiplte heightmaps
        auto tileProvidermanager = owner()->getTileProviderManager();
        auto heightMapProviders = tileProvidermanager->getActivatedLayerCategory(LayeredTextures::HeightMaps);
        if (heightMapProviders.size() > 0) {
            TileAndTransform tileAndTransform = heightMapProviders[0]->getHighestResolutionTile(_index);
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


    void Chunk::render(const RenderData& data) const {
        _owner->getPatchRenderer().renderChunk(*this, data);
    }


    //////////////////////////////////////////////////////////////////////////////////////
    //                              Chunk evaluation                                    //
    //////////////////////////////////////////////////////////////////////////////////////


    int EvaluateChunkLevelByDistance::getDesiredLevel(const Chunk& chunk, const RenderData& data) const {
        ChunkedLodGlobe const * globe = chunk.owner();
        const Ellipsoid& ellipsoid = globe->ellipsoid();

        Vec3 cameraPosition = data.camera.positionVec3();
        Geodetic2 pointOnPatch = chunk.surfacePatch().closestPoint(
            ellipsoid.cartesianToGeodetic2(cameraPosition));
        
        Chunk::BoundingHeights heights = chunk.getBoundingHeights();

        Vec3 globePosition = data.position.dvec3();
        Vec3 patchPosition = globePosition + ellipsoid.cartesianSurfacePosition(pointOnPatch);
        Vec3 cameraToChunk = patchPosition - cameraPosition;


        // Calculate desired level based on distance
        Scalar distanceToPatch = glm::length(cameraToChunk);
        Scalar distance = distanceToPatch - heights.min; // distance to actual minimum heights

        Scalar scaleFactor = globe->lodScaleFactor * ellipsoid.minimumRadius();
        Scalar projectedScaleFactor = scaleFactor / distance;
        int desiredLevel = ceil(log2(projectedScaleFactor));
        return desiredLevel;
    }

    int EvaluateChunkLevelByProjectedArea::getDesiredLevel(const Chunk& chunk, const RenderData& data) const {
        ChunkedLodGlobe const * globe = chunk.owner();
        const Ellipsoid& ellipsoid = globe->ellipsoid();
        Vec3 cameraPosition = data.camera.positionVec3();
        Vec3 globePosition = data.position.dvec3();
        Vec3 cameraToEllipseCenter = globePosition - cameraPosition;

        Geodetic2 camPos = ellipsoid.cartesianToGeodetic2(cameraPosition);
        /*
        struct CornerDist {
            Geodetic2 corner;
            float dist;
        };

        struct {
            bool operator()(const CornerDist& a, const CornerDist& b) {
                return a.dist < b.dist;
            }
        } byDist;

        std::vector<CornerDist> cornerDists(4);
        for (size_t i = 0; i < 4; i++) {
            const Geodetic2& c = chunk.surfacePatch().getCorner((Quad)i);
            Geodetic2 diff = (camPos - c);
            float latDiff = fAngle::fromRadians(diff.lat).getNormalizedAround(fAngle::ZERO).asRadians();
            float lonDiff = fAngle::fromRadians(diff.lon).getNormalizedAround(fAngle::ZERO).asRadians();
            cornerDists[i].corner = c;
            cornerDists[i].dist = latDiff*latDiff + lonDiff*lonDiff;;
        }

        std::sort(cornerDists.begin(), cornerDists.end(), byDist);

        Chunk::BoundingHeights heights = chunk.getBoundingHeights();

        const Geodetic3 c0 = { cornerDists[0].corner, heights.min };
        const Geodetic3 c1 = { cornerDists[1].corner, heights.min };
        const Geodetic3 c2 = { cornerDists[2].corner, heights.max };
        const Geodetic3 c3 = { cornerDists[3].corner, heights.max };
        */

        Chunk::BoundingHeights heights = chunk.getBoundingHeights();

        const Geodetic3 c0 = { chunk.surfacePatch().getCorner((Quad)0), heights.min };
        const Geodetic3 c1 = { chunk.surfacePatch().getCorner((Quad)1), heights.min };
        const Geodetic3 c2 = { chunk.surfacePatch().getCorner((Quad)2), heights.min };
        const Geodetic3 c3 = { chunk.surfacePatch().getCorner((Quad)3), heights.min };

        Vec3 A = cameraToEllipseCenter + ellipsoid.cartesianPosition(c0);
        Vec3 B = cameraToEllipseCenter + ellipsoid.cartesianPosition(c1);
        Vec3 C = cameraToEllipseCenter + ellipsoid.cartesianPosition(c2);
        Vec3 D = cameraToEllipseCenter + ellipsoid.cartesianPosition(c3);

        // Project points onto unit sphere
        A = glm::normalize(A);
        B = glm::normalize(B);
        C = glm::normalize(C);
        D = glm::normalize(D);


        /*
        A-----____
        |         '''-----B
        |          __--'  |
        |   __--''        |
        C-----------------D
        */

        const Vec3 AB = B - A;
        const Vec3 AC = C - A;
        const Vec3 DC = C - D;
        const Vec3 DB = B - D;

        double areaTriangle1 = 0.5 * glm::length(glm::cross(AC, AB));
        double areaTriangle2 = 0.5 * glm::length(glm::cross(DC, DB));
        double projectedChunkAreaApprox = areaTriangle1 + areaTriangle2;

        double scaledArea = globe->lodScaleFactor * projectedChunkAreaApprox;
        return chunk.index().level + round(scaledArea - 1);
    }

    int EvaluateChunkLevelByAvailableTileData::getDesiredLevel(const Chunk& chunk, const RenderData& data) const {
        auto tileProvidermanager = chunk.owner()->getTileProviderManager();
        auto heightMapProviders = tileProvidermanager->getActivatedLayerCategory(LayeredTextures::HeightMaps);
        int currLevel = chunk.index().level;

        // simply check the first heigtmap
        if (heightMapProviders.size() > 0) {
            Tile::Status heightTileStatus = heightMapProviders[0]->getTileStatus(chunk.index());
            if (heightTileStatus == Tile::Status::IOError || heightTileStatus == Tile::Status::OutOfRange) {
                return currLevel-1;
            }

            return UNKNOWN_DESIRED_LEVEL;
        }

        return UNKNOWN_DESIRED_LEVEL;
    }

} // namespace openspace
