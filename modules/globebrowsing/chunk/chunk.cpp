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

#include <modules/globebrowsing/chunk/chunk.h>

#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/globes/chunkedlodglobe.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/globebrowsing/tile/tileselector.h>
#include <modules/globebrowsing/tile/tilemetadata.h>
#include <modules/globebrowsing/rendering/layer/layergroup.h>
#include <openspace/util/updatestructures.h>

namespace openspace::globebrowsing {

const float Chunk::DEFAULT_HEIGHT = 0.0f;

Chunk::Chunk(const RenderableGlobe& owner, const TileIndex& tileIndex, bool initVisible)
    : _owner(owner)
    , _tileIndex(tileIndex)
    , _isVisible(initVisible)
    , _surfacePatch(tileIndex) 
{}

const GeodeticPatch& Chunk::surfacePatch() const {
    return _surfacePatch;
}

const RenderableGlobe& Chunk::owner() const {
    return _owner;
}

const TileIndex Chunk::tileIndex() const {
    return _tileIndex;
}

bool Chunk::isVisible() const {
    return _isVisible;
}

Chunk::Status Chunk::update(const RenderData& data) {
    const auto& savedCamera = _owner.savedCamera();
    const Camera& camRef = savedCamera != nullptr ? *savedCamera : data.camera;
    RenderData myRenderData = { camRef, data.position, data.time, data.doPerformanceMeasurement, data.renderBinMask, data.modelTransform };

    _isVisible = true;
    if (_owner.chunkedLodGlobe()->testIfCullable(*this, myRenderData)) {
        _isVisible = false;
        return Status::WantMerge;
    }

    int desiredLevel = _owner.chunkedLodGlobe()->getDesiredLevel(*this, myRenderData);

    if (desiredLevel < _tileIndex.level) {
        return Status::WantMerge;
    }
    else if (_tileIndex.level < desiredLevel) {
        return Status::WantSplit;
    }
    else {
        return Status::DoNothing;
    }
}

Chunk::BoundingHeights Chunk::getBoundingHeights() const {
    using ChunkTileSettingsPair = std::pair<ChunkTile, const LayerRenderSettings*>;

    BoundingHeights boundingHeights {
        0.f, 0.f,
        false
    };

    // In the future, this should be abstracted away and more easily queryable.
    // One must also handle how to sample pick one out of multiplte heightmaps
    std::shared_ptr<LayerManager> layerManager =
        owner().chunkedLodGlobe()->layerManager();

    // The raster of a height map is the first one. We assume that the height map is
    // a single raster image. If it is not we will just use the first raster
    // (that is channel 0).
    const size_t HeightChannel = 0;
    const LayerGroup& heightmaps = layerManager->layerGroup(layergroupid::GroupID::HeightLayers);
    std::vector<ChunkTileSettingsPair> chunkTileSettingPairs =
        tileselector::getTilesAndSettingsUnsorted(
            heightmaps, _tileIndex);

    bool lastHadMissingData = true;
    for (const ChunkTileSettingsPair& chunkTileSettingsPair : chunkTileSettingPairs) {
        ChunkTile chunkTile = chunkTileSettingsPair.first;
        const LayerRenderSettings* settings = chunkTileSettingsPair.second;
        bool goodTile = (chunkTile.tile.status() == Tile::Status::OK);
        bool hasTileMetaData = (chunkTile.tile.metaData() != nullptr);

        if (goodTile && hasTileMetaData) {
            std::shared_ptr<TileMetaData> tileMetaData = chunkTile.tile.metaData();

            float minValue =
                settings->performLayerSettings(tileMetaData->minValues[HeightChannel]);
            float maxValue =
                settings->performLayerSettings(tileMetaData->maxValues[HeightChannel]);

            if (!boundingHeights.available) {
                if (tileMetaData->hasMissingData[HeightChannel]) {
                    boundingHeights.min = std::min(
                        DEFAULT_HEIGHT,
                        minValue
                    );
                    boundingHeights.max = std::max(
                        DEFAULT_HEIGHT,
                        maxValue
                    );
                }
                else {
                    boundingHeights.min = minValue;
                    boundingHeights.max = maxValue;
                }
                boundingHeights.available = true;
            }
            else {
                boundingHeights.min = std::min(
                    boundingHeights.min,
                    minValue
                );
                boundingHeights.max = std::max(
                    boundingHeights.max,
                    maxValue
                );
            }
            lastHadMissingData = tileMetaData->hasMissingData[HeightChannel];
        }

        // Allow for early termination
        if (!lastHadMissingData) {
            break;
        }
    }
        
    return boundingHeights;
}

std::vector<glm::dvec4> Chunk::getBoundingPolyhedronCorners() const {
    const Ellipsoid& ellipsoid = owner().ellipsoid();
    const GeodeticPatch& patch = surfacePatch();

    BoundingHeights boundingHeight = getBoundingHeights();

    // assume worst case
    double patchCenterRadius = ellipsoid.maximumRadius();

    double maxCenterRadius = patchCenterRadius + boundingHeight.max;
    Geodetic2 halfSize = patch.halfSize();

    // As the patch is curved, the maximum height offsets at the corners must be long 
    // enough to cover large enough to cover a boundingHeight.max at the center of the 
    // patch.
    // Approximating scaleToCoverCenter by assuming the latitude and longitude angles
    // of "halfSize" are equal to the angles they create from the center of the
    // globe to the patch corners. This is true for the longitude direction when
    // the ellipsoid can be approximated as a sphere and for the latitude for patches
    // close to the equator. Close to the pole this will lead to a bigger than needed
    // value for scaleToCoverCenter. However, this is a simple calculation and a good
    // Approximation.
    double y1 = tan(halfSize.lat);
    double y2 = tan(halfSize.lon);
    double scaleToCoverCenter = sqrt(1 + pow(y1, 2) + pow(y2, 2));
        
    double maxCornerHeight = maxCenterRadius * scaleToCoverCenter - patchCenterRadius;

    bool chunkIsNorthOfEquator = patch.isNorthern();

    // The minimum height offset, however, we can simply 
    double minCornerHeight = boundingHeight.min;
    std::vector<glm::dvec4> corners(8);
        
    double latCloseToEquator = patch.edgeLatitudeNearestEquator();
    Geodetic3 p1Geodetic = { { latCloseToEquator, patch.minLon() }, maxCornerHeight };
    Geodetic3 p2Geodetic = { { latCloseToEquator, patch.maxLon() }, maxCornerHeight };
        
    glm::vec3 p1 = ellipsoid.cartesianPosition(p1Geodetic);
    glm::vec3 p2 = ellipsoid.cartesianPosition(p2Geodetic);
    glm::vec3 p = 0.5f * (p1 + p2);
    Geodetic2 pGeodetic = ellipsoid.cartesianToGeodetic2(p);
    double latDiff = latCloseToEquator - pGeodetic.lat;

    for (size_t i = 0; i < 8; ++i) {
        Quad q = static_cast<Quad>(i % 4);
        double cornerHeight = i < 4 ? minCornerHeight : maxCornerHeight;
        Geodetic3 cornerGeodetic = { patch.getCorner(q), cornerHeight };
            
        bool cornerIsNorthern = !((i / 2) % 2);
        bool cornerCloseToEquator = chunkIsNorthOfEquator ^ cornerIsNorthern;
        if (cornerCloseToEquator) {
            cornerGeodetic.geodetic2.lat += latDiff;
        }

        corners[i] = glm::dvec4(ellipsoid.cartesianPosition(cornerGeodetic), 1);
    }

    return corners;
}

} // namespace openspace::globebrowsing
