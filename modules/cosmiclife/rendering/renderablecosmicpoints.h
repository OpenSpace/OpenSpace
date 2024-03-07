/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#ifndef __OPENSPACE_MODULE_COSMICLIFE___RENDERABLECOSMICPOINTS___H__
#define __OPENSPACE_MODULE_COSMICLIFE___RENDERABLECOSMICPOINTS___H__

#include <openspace/rendering/renderable.h>

#include <openspace/data/speckloader.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/util/distanceconversion.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
#include <filesystem>
// from billboard cloud
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/vector/ivec2property.h>
#include <openspace/properties/vector/vec2property.h>
#include <functional>
#include <unordered_map>

namespace ghoul::filesystem { class File; }
namespace ghoul::fontrendering { class Font; }

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

namespace documentation { struct Documentation; }

class RenderableCosmicPoints : public Renderable {
public:
    explicit RenderableCosmicPoints(const ghoul::Dictionary& dictionary);
    ~RenderableCosmicPoints() = default;

    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    // For sonification
    dataloader::Labelset* labels();
    DistanceUnit unit() const;

    static documentation::Documentation Documentation();

private:
    std::vector<float> createDataSlice();
    void createPolygonTexture();
    void renderToTexture(GLuint textureToRenderTo, GLuint textureWidth,
        GLuint textureHeight);
    void loadPolygonGeometryForRendering();
    void renderPolygonGeometry(GLuint vao);
    void renderPoints(const RenderData& data, const glm::dmat4& modelMatrix,
        const glm::dvec3& orthoRight, const glm::dvec3& orthoUp, float fadeInVariable);
    void renderLabels(const RenderData& data, const glm::dmat4& modelViewProjectionMatrix,
        const glm::dvec3& orthoRight, const glm::dvec3& orthoUp, float fadeInVariable);
    float computeFadeFactor(float distanceNodeToCamera) const;


  //  void readColorMapFile();

    // bool variables
    bool _hasSpeckFile = false;
    bool _dataIsDirty = true;
    bool _textColorIsDirty = true;
    bool _hasSpriteTexture = false;
    bool _spriteTextureIsDirty = true;
    bool _hasColorMapFile = false;
    bool _isColorMapExact = false;
    bool _hasDatavarSize = false;
    bool _hasPolygon = false;
    bool _hasLabel = false;

    int _polygonSides = 0;

    GLuint _pTexture = 0;

    properties::FloatProperty _scaleFactor;
    properties::Vec3Property _pointColor;
    properties::StringProperty _spriteTexturePath;
    properties::Vec3Property _textColor;
    properties::FloatProperty _textOpacity;
    properties::FloatProperty _textSize;
    properties::IVec2Property _textMinMaxSize;
    properties::BoolProperty _drawElements;
    properties::BoolProperty _drawLabels;
    properties::BoolProperty _pixelSizeControl;
    properties::OptionProperty _colorOption;
    properties::Vec2Property _optionColorRangeData;
    properties::OptionProperty _datavarSizeOption;
    properties::Vec2Property _fadeInDistances;
    properties::BoolProperty _disableFadeInDistance;
    properties::Vec2Property _billboardMinMaxSize;
    properties::FloatProperty _correctionSizeEndDistance;
    properties::FloatProperty _correctionSizeFactor;
    properties::BoolProperty _useLinearFiltering;
    properties::TriggerProperty _setRangeFromData;
    properties::OptionProperty _renderOption;
    properties::BoolProperty _enableLabelFadingEffect;
    properties::Vec2Property _fadeLabelDistances;
    properties::Vec2Property _fadeLabelWidths;

    ghoul::opengl::Texture* _polygonTexture = nullptr;
    ghoul::opengl::Texture* _spriteTexture = nullptr;
    ghoul::opengl::ProgramObject* _program = nullptr;
    ghoul::opengl::ProgramObject* _renderToPolygonProgram = nullptr;

    // variables that are sent to the shaders
    UniformCache(
        cameraViewProjectionMatrix, modelMatrix, cameraPos, cameraLookup, renderOption,
        minBillboardSize, maxBillboardSize, correctionSizeEndDistance,
        correctionSizeFactor, color, alphaValue, scaleFactor, up, right, fadeInValue,
        screenSize, spriteTexture, hasColormap, enabledRectSizeControl, hasDvarScaling
    ) _uniformCache;

    // font variable from ghoul library
    std::shared_ptr<ghoul::fontrendering::Font> _font;

    // String variables
    std::string _speckFile;
    std::string _colorMapFile;
    std::string _labelFile;
    std::string _colorOptionString;
    std::string _datavarSizeOptionString;

    // distance default unit -- change?
    DistanceUnit _unit = DistanceUnit::Parsec;

    // speck files
    dataloader::Dataset _dataset;
    dataloader::Labelset _labelset;
    dataloader::ColorMap _colorMap;

    // range data, do we need conversion map?
    std::vector<glm::vec2> _colorRangeData;
    std::unordered_map<int, std::string> _optionConversionMap;
    std::unordered_map<int, std::string> _optionConversionSizeMap;

    glm::dmat4 _transformationMatrix = glm::dmat4(1.0);

    GLuint _vao = 0;
    GLuint _vbo = 0;
    // For polygons -- needed?
    GLuint _polygonVao = 0;
    GLuint _polygonVbo = 0;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_COSMICLIFE___RENDERABLECOSMICPOINTS___H__
