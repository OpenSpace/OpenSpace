/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_DIGITALUNIVERSE___RENDERABLEBILLBOARDSCLOUD___H__
#define __OPENSPACE_MODULE_DIGITALUNIVERSE___RENDERABLEBILLBOARDSCLOUD___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/vector/vec4property.h>

#include <ghoul/font/fontrenderer.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>

#include <functional>
#include <unordered_map>

namespace ghoul::filesystem { class File; }

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

    namespace documentation { struct Documentation; }

    class RenderableBillboardsCloud : public Renderable {
    public:
        explicit RenderableBillboardsCloud(const ghoul::Dictionary& dictionary);
        ~RenderableBillboardsCloud() = default;

        void initialize() override;
        void initializeGL() override;
        void deinitializeGL() override;

        bool isReady() const override;

        void render(const RenderData& data, RendererTasks& rendererTask) override;
        void update(const UpdateData& data) override;

        static documentation::Documentation Documentation();

    private:

        enum Unit {
            Meter = 0,
            Kilometer = 1,
            Parsec = 2,
            Kiloparsec = 3,
            Megaparsec = 4,
            Gigaparsec = 5,

            GigalightYears = 6
        };

        void createDataSlice();
        void createPolygonTexture();
        void renderToTexture(std::function<void(void)> geometryLoadingFunction,
            std::function<void(GLuint)> renderFunction,
            GLuint textureToRenderTo, GLuint textureWidth, GLuint textureHeight);
        void loadPolygonGeometryForRendering();
        void renderPolygonGeometry(GLuint vao);
        void renderBillboards(const RenderData& data, const glm::dmat4& modelMatrix,
            const glm::dvec3& orthoRight, const glm::dvec3& orthoUp, float fadeInVariable);
        void renderLabels(const RenderData& data, const glm::dmat4& modelViewProjectionMatrix,
            const glm::dvec3& orthoRight, const glm::dvec3& orthoUp, float fadeInVariable);

        bool loadData();
        bool loadSpeckData();
        bool loadLabelData();
        bool readSpeckFile();
        bool readColorMapFile();
        bool readLabelFile();
        bool loadCachedFile(const std::string& file);
        bool saveCachedFile(const std::string& file) const;

        bool _hasSpeckFile;
        bool _dataIsDirty;
        bool _textColorIsDirty;
        bool _hasSpriteTexture;
        bool _spriteTextureIsDirty;
        bool _hasColorMapFile;
        bool _hasPolygon;
        bool _hasLabel;
        bool _labelDataIsDirty;

        int _polygonSides;

        GLuint _pTexture;

        properties::FloatProperty _alphaValue;
        properties::FloatProperty _scaleFactor;
        properties::Vec3Property _pointColor;
        properties::StringProperty _spriteTexturePath;
        properties::Vec4Property _textColor;
        properties::FloatProperty _textSize;
        properties::FloatProperty _textMinSize;
        properties::FloatProperty _textMaxSize;
        properties::BoolProperty _drawElements;
        properties::BoolProperty _drawLabels;
        properties::BoolProperty _pixelSizeControl;
        properties::OptionProperty _colorOption;
        properties::Vec2Property _fadeInDistance;
        properties::BoolProperty _disableFadeInDistance;
        properties::FloatProperty _billboardMaxSize;
        properties::FloatProperty _billboardMinSize;
        properties::FloatProperty _correctionSizeEndDistance;
        properties::FloatProperty _correctionSizeFactor;

        // DEBUG:
        properties::OptionProperty _renderOption;


        std::unique_ptr<ghoul::opengl::Texture> _polygonTexture;
        std::unique_ptr<ghoul::opengl::Texture> _spriteTexture;
        std::unique_ptr<ghoul::filesystem::File> _spriteTextureFile;
        //std::unique_ptr<ghoul::opengl::ProgramObject> _program;
        ghoul::opengl::ProgramObject* _program;
        ghoul::opengl::ProgramObject* _renderToPolygonProgram;
        UniformCache(
            cameraViewProjectionMatrix, modelMatrix, cameraPos, cameraLookup, 
            renderOption, minBillboardSize, maxBillboardSize, correctionSizeEndDistance,
            correctionSizeFactor, color, sides, alphaValue, scaleFactor, up, right, 
            fadeInValue, screenSize, spriteTexture, polygonTexture, hasPolygon, 
            hasColormap, enabledRectSizeControl
        ) _uniformCache;
        std::shared_ptr<ghoul::fontrendering::Font> _font;

        std::string _speckFile;
        std::string _colorMapFile;
        std::string _labelFile;
        std::string _colorOptionString;

        Unit _unit;

        std::vector<float> _slicedData;
        std::vector<float> _fullData;
        std::vector<glm::vec4> _colorMapData;
        std::vector<std::pair<glm::vec3, std::string>> _labelData;
        std::unordered_map<std::string, int> _variableDataPositionMap;
        std::unordered_map<int, std::string> _optionConversionMap;
        std::vector<glm::vec2> _colorRangeData;

        int _nValuesPerAstronomicalObject;

        glm::dmat4 _transformationMatrix;

        GLuint _vao;
        GLuint _vbo;

        // For polygons
        GLuint _polygonVao;
        GLuint _polygonVbo;
    };


} // namespace openspace

#endif // __OPENSPACE_MODULE_DIGITALUNIVERSE___RENDERABLEBILLBOARDSCLOUD___H__
