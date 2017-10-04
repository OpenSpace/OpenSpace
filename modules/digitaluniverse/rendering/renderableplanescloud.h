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

#ifndef __OPENSPACE_MODULE_DIGITALUNIVERSE___RENDERABLEPLANESCLOUD___H__
#define __OPENSPACE_MODULE_DIGITALUNIVERSE___RENDERABLEPLANESCLOUD___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/vector/vec4property.h>

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/font/fontrenderer.h>

#include <functional>

namespace ghoul::filesystem { 
    class File; 
}

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

    namespace documentation { struct Documentation; }

    class RenderablePlanesCloud : public Renderable {
    public:
        explicit RenderablePlanesCloud(const ghoul::Dictionary& dictionary);
        ~RenderablePlanesCloud() = default;

        void initialize() override;
        void deinitialize() override;

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
        void createLabelData();
        void renderToTexture(std::function<GLuint(void)> geometryLoadingFunction,
            std::function<void(GLuint)> renderFunction,
            GLuint textureToRenderTo, GLuint textureWidth, GLuint textureHeight);
        GLuint loadPolygonGeometryForRendering();
        void renderPolygonGeometry(GLuint vao);
        GLuint loadTextGeometryForRendering();
        void renderTextGeometry(GLuint vao);
        void renderBillboards(const RenderData& data, const glm::dmat4& modelViewMatrix,
            const glm::dmat4& projectionMatrix, const glm::vec3& orthoRight, const glm::vec3& orthoUp);
        void renderLabels(const RenderData& data, const glm::dmat4& modelViewMatrix,
            const glm::dmat4& projectionMatrix, const glm::vec3& orthoRight, const glm::vec3& orthoUp);

        bool loadData();
        bool readSpeckFile();
        bool readColorMapFile();
        bool readLabelFile();
        bool loadCachedFile(const std::string& file);
        bool saveCachedFile(const std::string& file) const;
        
        bool _hasSpeckFile;
        bool _dataIsDirty;
        bool _textColorIsDirty;
        bool _hasLabel;
        bool _labelDataIsDirty;

        int _textMinSize;

        int _planeStartingIndexPos;
        int _textureVariableIndex;

        GLuint _pTexture;
        GLuint _tTexture;

        properties::FloatProperty _alphaValue;
        properties::FloatProperty _scaleFactor;
        properties::Vec4Property _textColor;
        properties::FloatProperty _textSize;
        properties::BoolProperty _drawElements;


        std::unique_ptr<ghoul::opengl::Texture> _polygonTexture;
        std::unique_ptr<ghoul::opengl::Texture> _spriteTexture;
        std::unique_ptr<ghoul::filesystem::File> _spriteTextureFile;
        std::unique_ptr<ghoul::opengl::ProgramObject> _program;
        std::unique_ptr<ghoul::fontrendering::FontRenderer> _fontRenderer;        
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
    };


} // namespace openspace

#endif // __OPENSPACE_MODULE_DIGITALUNIVERSE___RENDERABLEPLANESCLOUD___H__
