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
#include <modules/globebrowsing/globes/chunklodglobe.h>



namespace {
    const std::string _loggerCat = "Chunk";
}

namespace openspace {

    Chunk::Chunk(ChunkLodGlobe* owner, const ChunkIndex& chunkIndex)
        : _owner(owner)
        , _surfacePatch(chunkIndex)
        , _index(chunkIndex)
        , _isVisible(true) 
    {

    }

    const GeodeticPatch& Chunk::surfacePatch() const {
        return _surfacePatch;
    }

    ChunkLodGlobe* const Chunk::owner() const {
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

    void Chunk::setOwner(ChunkLodGlobe* newOwner) {
        _owner = newOwner;
    }

    Chunk::Status Chunk::update(const RenderData& data) {
        //Uses horizon culling, frustum culling and distance to camera to determine a
        //desired level.
        //In the current implementation of the horizon culling and the distance to the
        //camera, the closer the ellipsoid is to a
        //sphere, the better this will make the splitting. Using the minimum radius to
        //be safe. This means that if the ellipsoid has high difference between radii,
        //splitting might accur even though it is not needed.

        _isVisible = true;
        Vec3 globePosition = data.position.dvec3();

        auto center = _surfacePatch.center();
        auto ellipsoid = _owner->ellipsoid();
        Vec3 patchPosition = globePosition + ellipsoid.geodetic2ToCartesian(center);

        Vec3 cameraPosition = data.camera.position().dvec3();
        Vec3 cameraToChunk = patchPosition - cameraPosition;
        Scalar minimumGlobeRadius = ellipsoid.minimumRadius();


        // Do horizon culling
        const int maxHeight = 8700; // should be read from gdal dataset or mod file
        if (!HorizonCuller::isVisible(data, _surfacePatch, ellipsoid, maxHeight)) {
            _isVisible = false;
            return WANT_MERGE;
        }

        // Do frustum culling
        if (!FrustumCuller::isVisible(data, _surfacePatch, ellipsoid)) {
            _isVisible = false;
            return WANT_MERGE;
        }


        // Calculate desired level based on distance
        Scalar distance = glm::length(cameraToChunk);
        _owner->minDistToCamera = fmin(_owner->minDistToCamera, distance);

        Scalar scaleFactor = 10 * minimumGlobeRadius;
        Scalar projectedScaleFactor = scaleFactor / distance;
        int desiredLevel = floor(log2(projectedScaleFactor));

        // clamp level
        desiredLevel = glm::clamp(desiredLevel, _owner->minSplitDepth, _owner->maxSplitDepth);

        if (desiredLevel < _index.level) return WANT_MERGE;
        else if (_index.level < desiredLevel) return WANT_SPLIT;
        else return DO_NOTHING;
    }


    

} // namespace openspace
