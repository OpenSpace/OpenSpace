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
#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec4property.h>
#include <openspace/util/syncdata.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/uniformcache.h>
#include <array>

namespace openspace { struct Documentation; }

namespace ghoul::filesystem { class File; }
namespace ghoul::opengl { class ProgramObject; }

namespace openspace::exoplanets {

class RenderableExoplanetGlyphCloud : public Renderable {
public:
    RenderableExoplanetGlyphCloud(const ghoul::Dictionary& dictionary);

    static const size_t MaxNumberColors = 4;

    /**
     * Return the index of the currently hovered glyph, or -1 if none is hovered
     */
    int hoveredIndex() const;

    void initialize() override;
    void initializeGL() override;
    void deinitialize() override;
    void deinitializeGL() override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static openspace::Documentation Documentation();

private:
    void initializeSelectionCallbacks();
    void initializeShaders();

    void createGlyphIdTexture(const glm::uvec3 dimensions);
    void mapVertexAttributes(GLuint vao);

    void updateDataIfChanged();

    // Rendering helper methods
    void setupCommonUniforms(
        ghoul::opengl::ProgramObject& program,
        const RenderData& data
    );
    void setupRingsSpecificUniforms(
        ghoul::opengl::ProgramObject& program,
        const RenderData& data
    );
    void renderMainPass();
    void renderIndexTexture(ghoul::opengl::ProgramObject& program);
    void renderSelectedPoints(ghoul::opengl::ProgramObject& program);

    void renderStars(const RenderData& data);

    bool _renderDataIsDirty = true;
    bool _selectionChanged = true;
    bool _glyphModeChanged = false;

    std::unique_ptr<ghoul::opengl::ProgramObject> _programRings = nullptr;

    UniformCache(modelMatrix, cameraViewProjectionMatrix, onTop, opacity, scale, maxIndex,
        currentIndex, cameraPosition, isHighlightMode, darkenFactor,
        // Rings specific uniforms
        renderOption, up, right, cameraLookUp,  useFixedRingWidth
    ) _uniformCacheRings;

    std::unique_ptr<ghoul::opengl::ProgramObject> _programInclination = nullptr;

    UniformCache(modelMatrix, cameraViewProjectionMatrix, onTop, opacity, scale, maxIndex,
        currentIndex, cameraPosition, isHighlightMode, darkenFactor
    ) _uniformCacheInclination;

    FloatProperty _scale;
    IntListProperty _selectedIndices;
    BoolProperty _useFixedRingWidth;

    OptionProperty _orientationRenderOption;
    OptionProperty _glyphMode;

    FloatProperty _darkenFactor;

    BoolProperty _showMissingInclination;

    struct {
        PropertyOwner owner;
        BoolProperty enabled;
        Vec4Property lineColor;
        FloatProperty lineWidth;
        FloatProperty lineLength;
    } _starGlyph;

    // Unified glyph data structure
    struct GlyphData {
        // Base data (always present)
        glm::vec3 position;
        float component = -1.f;
        size_t index = 0;

        // Rings mode data
        int nColors = -1;
        std::array<glm::vec4, MaxNumberColors> colors = {};

        // Inclination mode data
        glm::vec3 inclinationVector = glm::vec3(0.f);
        int hasInclination = 1; // 1 if inclination is present, 0 if not (default value is used)
    };

    std::vector<GlyphData> _glyphData;
    std::vector<size_t> _glyphIndices; // indices of the points in the dataviewer
    int _maxIndex = -1;

    // Hovered index, set from interaction on master node and synced to other nodes
    SyncData<int> _currentlyHoveredIndex;
    SyncData<bool> _shouldHighlightHovered;

    bool _isInSelectionMode = false;
    bool _isLeftShiftHeld = false;

    GLuint _pointsVao = 0;
    GLuint _pointsVbo = 0;

    GLuint _selectedVao = 0;
    GLuint _selectedVbo = 0;

    // Point id from screenspace position
    std::unique_ptr<ghoul::opengl::Texture> _glyphIdTexture;
    std::unique_ptr<ghoul::opengl::Texture> _depthTexture;
    GLuint _glyphIdFbo = 0;

    glm::ivec2 _lastViewPortSize;
    double _lastDataTimeStamp = 0.0;

    // Star glyphs
    struct StarGlyphData {
        glm::vec3 position;
        glm::vec3 up; // Up vector of the star, in world space
    };
    std::vector<StarGlyphData> _starData;

    GLuint _starsVao = 0;
    GLuint _starsVbo = 0;

    std::unique_ptr<ghoul::opengl::ProgramObject> _programStars = nullptr;

    UniformCache(
        modelMatrix, cameraViewProjectionMatrix, opacity, scale, cameraPosition,
        originLineColor, lineLengthFactor
    ) _uniformCacheStars;
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___RENDERABLEEXOPLANETGLYPHCLOUD___H__
