/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_MODULE_DIGITALUNIVERSE___RENDERABLEDUMESHES___H__
#define __OPENSPACE_MODULE_DIGITALUNIVERSE___RENDERABLEDUMESHES___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/vector/vec4property.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
#include <unordered_map>

namespace ghoul::filesystem { class File; }
namespace ghoul::fontrendering { class Font; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

namespace documentation { struct Documentation; }

class RenderableDUMeshes : public Renderable {
public:
    explicit RenderableDUMeshes(const ghoul::Dictionary& dictionary);
    ~RenderableDUMeshes() = default;

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

    enum MeshType {
        Solid = 0,
        Wire = 1,
        Point = 2,
        INVALID = 9
    };

    struct RenderingMesh {
        int meshIndex;
        int colorIndex;
        int textureIndex;
        // From: Partiview User’s Guide
        // Brian Abbott
        // Hayden Planetarium American Museum of Natural History New York, USA
        // "Specifies the dimensions of the mesh."
        // "If you wish to draw a line between points, then numU will be 1 while
        // numV will equal the number of points to connect.
        // If you want a square, 4000×4000 grid with lines every 200 units,
        // then numU numU will both equal 21
        int numU;
        int numV;
        MeshType style;
        std::vector<GLuint> vaoArray;
        std::vector<GLuint> vboArray;
        std::vector<GLfloat> vertices;
    };

    void createMeshes();
    void renderMeshes(const RenderData& data, const glm::dmat4& modelViewMatrix,
        const glm::dmat4& projectionMatrix);
    void renderLabels(const RenderData& data, const glm::dmat4& modelViewProjectionMatrix,
        const glm::vec3& orthoRight, const glm::vec3& orthoUp);

    bool loadData();
    bool readSpeckFile();
    bool readLabelFile();
    bool loadCachedFile(const std::string& file);
    bool saveCachedFile(const std::string& file) const;

    bool _hasSpeckFile = false;
    bool _dataIsDirty = true;
    bool _textColorIsDirty = true;
    bool _hasLabel = false;

    properties::FloatProperty _alphaValue;
    //properties::FloatProperty _scaleFactor;
    //properties::Vec3Property _pointColor;
    properties::Vec4Property _textColor;
    properties::FloatProperty _textSize;
    properties::BoolProperty _drawElements;
    properties::BoolProperty _drawLabels;
    //properties::OptionProperty _blendMode;
    properties::FloatProperty _textMinSize;
    properties::FloatProperty _textMaxSize;
    properties::FloatProperty _lineWidth;

    // DEBUG:
    properties::OptionProperty _renderOption;

    ghoul::opengl::ProgramObject* _program = nullptr;
    UniformCache(modelViewTransform, projectionTransform, alphaValue,
        color) _uniformCache;
    std::shared_ptr<ghoul::fontrendering::Font> _font = nullptr;

    std::string _speckFile;
    std::string _labelFile;

    Unit _unit = Parsec;

    std::vector<float> _fullData;
    std::vector<std::pair<glm::vec3, std::string>> _labelData;
    int _nValuesPerAstronomicalObject = 0;

    glm::dmat4 _transformationMatrix = glm::dmat4(1.0);

    std::unordered_map<int, glm::vec3> _meshColorMap;
    std::unordered_map<int, RenderingMesh> _renderingMeshesMap;
};
} // namespace openspace

#endif // __OPENSPACE_MODULE_DIGITALUNIVERSE___RENDERABLEDUMESHES___H__
