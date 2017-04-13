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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___CACHING_TILE_PROVIDER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___CACHING_TILE_PROVIDER___H__

#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>
#include <ghoul/opengl/programobject.h>

#include <modules/space/rendering/planetgeometry.h>
#include <modules/newhorizons/util/projectioncomponent.h>
#include <modules/newhorizons/util/sequenceparser.h>

namespace openspace {
namespace globebrowsing {

class AsyncTileDataProvider;
class RawTile;
    
namespace tileprovider {

class ProjectionTileProvider : public TileProvider {
public:
    ProjectionTileProvider(const ghoul::Dictionary& dictionary);

    virtual ~ProjectionTileProvider() override;
        
    virtual Tile getTile(const TileIndex& tileIndex) override;

    virtual Tile getDefaultTile() override;
    virtual Tile::Status getTileStatus(const TileIndex& tileIndex) override;
    virtual TileDepthTransform depthTransform() override;
    virtual void update() override;
    virtual void reset() override;
    virtual int maxLevel() override;
    virtual float noDataValueAsFloat() override;

    bool initialize() override;
    bool deinitialize() override;

    //static documentation::Documentation Documentation();

protected:
    bool loadTextures();
    void attitudeParameters(double time);

private:
    void imageProjectGPU(std::shared_ptr<ghoul::opengl::Texture> projectionTexture);

    ProjectionComponent _projectionComponent;

    //properties::StringProperty _colorTexturePath;
    //properties::StringProperty _heightMapTexturePath;

    //properties::IntProperty _rotation;

    std::unique_ptr<ghoul::opengl::ProgramObject> _fboProgramObject;

    //std::unique_ptr<ghoul::opengl::Texture> _baseTexture;
    //std::unique_ptr<ghoul::opengl::Texture> _heightMapTexture;

    //properties::BoolProperty _shiftMeridianBy180;

    //properties::FloatProperty _heightExaggeration;
    //properties::FloatProperty _debugProjectionTextureRotation;

    std::unique_ptr<planetgeometry::PlanetGeometry> _geometry;
    
    glm::vec2 _camScaling;
    glm::vec3 _up;
    glm::mat4 _transform;
    glm::mat4 _projectorMatrix;

    glm::dmat3 _stateMatrix;
    glm::dmat3 _instrumentMatrix;
    glm::vec3 _boresight;

    double _time;

    std::vector<Image> _imageTimes;

    std::string _frame;

    bool _capture;

    GLuint _quad;
    GLuint _vertexPositionBuffer;




private:
    Tile _defaultTile;
};

} // namespace tileprovider
} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___CACHING_TILE_PROVIDER___H__
