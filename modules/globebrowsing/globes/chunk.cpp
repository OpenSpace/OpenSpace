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

#include <modules/globebrowsing/globes/chunk.h>
#include <modules/globebrowsing/globes/chunkedlodglobe.h>



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

        const Ellipsoid& ellipsoid = _owner->ellipsoid();
        Vec3 cameraPosition = myRenderData.camera.positionVec3();
        Geodetic2 pointOnPatch = _surfacePatch.closestPoint(
                ellipsoid.cartesianToGeodetic2(cameraPosition));
        Vec3 globePosition = myRenderData.position.dvec3();
        Vec3 patchPosition = globePosition + ellipsoid.cartesianSurfacePosition(pointOnPatch);
        Vec3 cameraToChunk = patchPosition - cameraPosition;
        

        // Calculate desired level based on distance
        Scalar distance = glm::length(cameraToChunk);
        _owner->minDistToCamera = fmin(_owner->minDistToCamera, distance);

        Scalar scaleFactor = _owner->lodScaleFactor * ellipsoid.minimumRadius();;
        Scalar projectedScaleFactor = scaleFactor / distance;
        int desiredLevel = ceil(log2(projectedScaleFactor));

        // clamp level
        desiredLevel = glm::clamp(desiredLevel, _owner->minSplitDepth, _owner->maxSplitDepth);

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
        auto heightMapProviders = tileProvidermanager->getActivatedLayerCategory("HeightMaps");
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
    

} // namespace openspace
