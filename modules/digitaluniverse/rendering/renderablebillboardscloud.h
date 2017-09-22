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

#ifndef __OPENSPACE_MODULE_DIGITALUNIVERSE___RENDERABLEBILLBOARDSCLOUD___H__
#define __OPENSPACE_MODULE_DIGITALUNIVERSE___RENDERABLEBILLBOARDSCLOUD___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>

#include <ghoul/opengl/ghoul_gl.h>

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

    class RenderableBillboardsCloud : public Renderable {
    public:
        explicit RenderableBillboardsCloud(const ghoul::Dictionary& dictionary);
        ~RenderableBillboardsCloud() = default;

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
        void createPolygonTexture();
        void createTextTexture();
        void renderToTexture(std::function<GLuint(void)> geometryLoadingFunction,
            std::function<void(GLuint)> renderFunction,
            GLuint textureToRenderTo, GLuint textureWidth, GLuint textureHeight);
        GLuint loadPolygonGeometryForRendering();
        void renderPolygonGeometry(GLuint vao);
        GLuint loadTextGeometryForRendering();
        void renderTextgonGeometry(GLuint vao);

        bool loadData();
        bool readSpeckFile();
        bool readColorMapFile();
        bool loadCachedFile(const std::string& file);
        bool saveCachedFile(const std::string& file) const;
        void saveTextureToPPMFile(const GLenum color_buffer_attachment,
            const std::string & fileName, const int width, const int height) const;

        bool _dataIsDirty;
        bool _hasSpriteTexture;
        bool _spriteTextureIsDirty;
        bool _hasColorMapFile;
        bool _hasPolygon;

        int _polygonSides;
        GLuint _pTexture;
        GLuint _tTexture;

        properties::FloatProperty _alphaValue;
        properties::FloatProperty _scaleFactor;
        properties::Vec3Property _pointColor;
        properties::StringProperty _spriteTexturePath;
        
        std::unique_ptr<ghoul::opengl::Texture> _polygonTexture;
        std::unique_ptr<ghoul::opengl::Texture> _spriteTexture;
        std::unique_ptr<ghoul::filesystem::File> _spriteTextureFile;
        std::unique_ptr<ghoul::opengl::ProgramObject> _program;
        //std::unique_ptr<ghoul::fontrendering::FontRenderer> _fontRenderer;

        std::string _speckFile;
        std::string _colorMapFile;

        Unit _unit;

        std::vector<float> _slicedData;
        std::vector<float> _fullData;
        std::vector<glm::vec4> _colorMapData;

        int _nValuesPerAstronomicalObject;

        GLuint _vao;
        GLuint _vbo;
    };


} // namespace openspace

#endif // __OPENSPACE_MODULE_DIGITALUNIVERSE___RENDERABLEBILLBOARDSCLOUD___H__
