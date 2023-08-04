/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__ASDFTILEPROVIDER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__ASDFTILEPROVIDER___H__

#include <modules/globebrowsing/src/tileprovider/tileprovider.h>
#include <modules/globebrowsing/src/geodeticpatch.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/optionproperty.h>

namespace openspace::globebrowsing {

class AsdfTileProvider : public TileProvider {
public:
    AsdfTileProvider(const ghoul::Dictionary& dictionary);
    ~AsdfTileProvider() override;

    void update() override final;
    void reset() override final;
    int minLevel() override final;
    int maxLevel() override final;
    float noDataValueAsFloat() override final;
    globebrowsing::Tile tile(const globebrowsing::TileIndex& tileIndex) override final;
    globebrowsing::Tile::Status tileStatus(
        const globebrowsing::TileIndex& tileIndex) override final;
    globebrowsing::TileDepthTransform depthTransform() override final;
    globebrowsing::ChunkTile chunkTile(globebrowsing::TileIndex tileIndex, int parents,
        int maxParents = 1337) override;
private:
    void internalInitialize() override final;
    void internalDeinitialize() override final;

    properties::StringProperty _JSONPath;
    properties::StringProperty _startTime;
    properties::IntProperty _resolution;
    properties::Vec3Property _color;
    properties::FloatProperty _lineWidth;
    properties::FloatProperty _cutoff;
    properties::BoolProperty _renderFullAsdf;
    properties::OptionProperty _renderingMode;
    properties::OptionProperty _kernelSize;

    struct Feature {
        double _lat, _lon;
        int _time;
    };

    double _start = 0.0;
    GLuint _fbo = 0;
    GLuint _fbo2 = 0;
    GLuint _vao = 0;
    GLuint _vbo = 0;
    GLuint _quadVao = 0;
    GLuint _quadVbo = 0;
    GeodeticPatch _bounds;
    glm::ivec2 _rendertargetDimensions;
    std::unique_ptr<ghoul::opengl::ProgramObject> _program;
    std::unique_ptr<ghoul::opengl::ProgramObject> _program2;
    std::unique_ptr<ghoul::opengl::Texture> _texture;
    std::unique_ptr<ghoul::opengl::Texture> _texture2;
    std::vector<Feature> _features;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__ASDFTILEPROVIDER___H__
