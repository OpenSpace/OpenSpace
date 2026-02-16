/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___RENDERABLEEXOPLANETGLYPHCLOUD___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___RENDERABLEEXOPLANETGLYPHCLOUD___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/list/intlistproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/rendering/labelscomponent.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/uniformcache.h>

namespace openspace::documentation { struct Documentation; }

namespace ghoul::filesystem { class File; }
namespace ghoul::opengl { class ProgramObject; }

namespace openspace::exoplanets {

constexpr const int MaxNumberColors = 8;

class RenderableExoplanetGlyphCloud : public Renderable {
public:
    RenderableExoplanetGlyphCloud(const ghoul::Dictionary& dictionary);

    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    void updateDataFromFile();

    static documentation::Documentation Documentation();

private:
    void createGlyphIdTexture(const glm::uvec3 dimensions);
    void mapVertexAttributes(GLuint vao);

    bool _renderDataIsDirty = true;
    bool _selectionChanged = true;

    std::unique_ptr<ghoul::opengl::ProgramObject> _program = nullptr;
    UniformCache(modelMatrix, cameraViewProjectionMatrix, onTop, useFixedRingWidth,
        opacity, size, screenSize, minBillboardSize, maxBillboardSize, maxIndex,
        currentIndex, isRenderIndexStep
    ) _uniformCache;

    properties::Vec3Property _highlightColor;
    properties::FloatProperty _size;
    properties::IntListProperty _selectedIndices;
    properties::Vec2Property _billboardMinMaxSize;
    properties::BoolProperty _useFixedRingWidth;

    std::unique_ptr<ghoul::filesystem::File> _dataFile;
    bool _dataFileIsDirty = false;

    struct GlyphData {
        glm::vec3 position;
        float component = -1.f;
        size_t index = 0;
        int nColors = -1;
        glm::vec4 colors[MaxNumberColors];
    };

    std::vector<GlyphData> _fullGlyphData;
    std::vector<size_t> _glyphIndices; // indices of the points in the dataviewer
    int _maxIndex = -1;
    properties::IntProperty _currentlyHoveredIndex;

    bool _isInSelectionMode = false;

    // Labels
    bool _hasLabels = false;
    properties::BoolProperty _drawLabels;
    std::unique_ptr<LabelsComponent> _labels;

    GLuint _pointsVao = 0;
    GLuint _pointsVbo = 0;

    GLuint _selectedVao = 0;
    GLuint _selectedVbo = 0;

    // Point id from screenspace position
    std::unique_ptr<ghoul::opengl::Texture> _glyphIdTexture;
    std::unique_ptr<ghoul::opengl::Texture> _depthTexture;
    GLuint _glyphIdFbo = 0;

    glm::ivec2 _lastViewPortSize;
};

}// namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___RENDERABLEEXOPLANETGLYPHCLOUD___H__
