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

#include <modules/globebrowsing/globes/chunklodglobe.h>

#include <modules/globebrowsing/meshes/basicgrid.h>

// open space includes
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/scene/scenegraphnode.h>

// ghoul includes
#include <ghoul/misc/assert.h>

#define _USE_MATH_DEFINES
#include <math.h>

namespace {
    const std::string _loggerCat = "ChunkLodGlobe";
}

namespace openspace {

    const GeodeticPatch ChunkLodGlobe::LEFT_HEMISPHERE = GeodeticPatch(0, -M_PI/2, M_PI/2, M_PI/2);
    const GeodeticPatch ChunkLodGlobe::RIGHT_HEMISPHERE = GeodeticPatch(0, M_PI/2, M_PI/2, M_PI/2);

    const ChunkIndex ChunkLodGlobe::LEFT_HEMISPHERE_INDEX = ChunkIndex(0, 0, 1);
    const ChunkIndex ChunkLodGlobe::RIGHT_HEMISPHERE_INDEX = ChunkIndex(1, 0, 1);


    ChunkLodGlobe::ChunkLodGlobe(
        const Ellipsoid& ellipsoid,
        std::shared_ptr<TileProviderManager> tileProviderManager)
        : _ellipsoid(ellipsoid)
        , _leftRoot(new ChunkNode(*this, LEFT_HEMISPHERE_INDEX))
        , _rightRoot(new ChunkNode(*this, RIGHT_HEMISPHERE_INDEX))
        , minSplitDepth(2)
        , maxSplitDepth(22)
    {
        auto geometry = std::shared_ptr<BasicGrid>(new BasicGrid(
            256,
            256,
            TriangleSoup::Positions::No,
            TriangleSoup::TextureCoordinates::Yes,
            TriangleSoup::Normals::No));

        _patchRenderer.reset(new LatLonPatchRenderer(geometry, tileProviderManager));
    }

    ChunkLodGlobe::~ChunkLodGlobe() {

    }

    bool ChunkLodGlobe::initialize() {
        return isReady();
    }

    bool ChunkLodGlobe::deinitialize() {
        return true;
    }

    bool ChunkLodGlobe::isReady() const {
        bool ready = true;
        return ready;
    }

    LatLonPatchRenderer& ChunkLodGlobe::getPatchRenderer() {
        return *_patchRenderer;
    }

    void ChunkLodGlobe::render(const RenderData& data){
        minDistToCamera = INFINITY;
        ChunkNode::renderedPatches = 0;        



        _leftRoot->render(data);
        _rightRoot->render(data);

        //LDEBUG("min distnace to camera: " << minDistToCamera);

        Vec3 cameraPos = data.camera.position().dvec3();
        //LDEBUG("cam pos  x: " << cameraPos.x << "  y: " << cameraPos.y << "  z: " << cameraPos.z);

        //LDEBUG("ChunkNode count: " << ChunkNode::instanceCount);
        //LDEBUG("RenderedPatches count: " << ChunkNode::renderedPatches);
    }

    void ChunkLodGlobe::update(const UpdateData& data) {
        _patchRenderer->update();
    }

    const Ellipsoid& ChunkLodGlobe::ellipsoid() const
    {
        return _ellipsoid;
    }

}  // namespace openspace
