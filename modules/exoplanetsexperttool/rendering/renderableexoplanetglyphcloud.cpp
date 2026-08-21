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

#include <modules/exoplanetsexperttool/rendering/renderableexoplanetglyphcloud.h>

#include <modules/exoplanetsexperttool/exoplanetsexperttoolmodule.h>
#include <modules/exoplanets/exoplanetshelper.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/syncengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/keys.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/framebufferobject.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <optional>
#include <string_view>

namespace {
    using namespace openspace;

    constexpr std::string_view _loggerCat = "ExoplanetGlyphCloud";

    enum OrientationRenderOption {
        ViewDirection = 0,
        PositionNormal
    };

    enum GlyphMode {
        Rings = 0,
        Inclination
    };

    constexpr Property::PropertyInfo ScaleInfo = {
        "Scale",
        "Scale",
        "A scale value controlling the size of the glyphs."
    };

    constexpr Property::PropertyInfo SelectionInfo = {
        "Selection",
        "Selection",
        "A list of indices of selected points to be highlighted."
    };

    constexpr Property::PropertyInfo UseFixedWidthInfo = {
        "UseFixedWidth",
        "Use fixed width",
        "If true, all the rings representing the planets will have the very same width. "
        "Otherwise, the width of each ring decreases a bit as the radius gets larger."
    };

    constexpr Property::PropertyInfo OrientationRenderOptionInfo =
    {
        "OrientationRenderOption",
        "Orientation render option",
        "Controls how the planes for the points will be oriented. \"Camera View "
        "Direction\" rotates the points so that the plane is orthogonal to the viewing "
        "direction of the camera (useful for planar displays), and \"Camera Position "
        "Normal\" rotates the points towards the position of the camera (useful for "
        "spherical displays, like dome theaters). In both these cases the points will "
        "be billboarded towards the camera.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo GlyphModeInfo = {
        "GlyphMode",
        "Glyph mode",
        "Controls which visual representation to use for the exoplanet glyps.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo DarkenFactorInfo = {
        "DarkenFactor",
        "Darker factor (on highlight)",
        "The factor t use when darkening the glyph when in highlight mode (triggered by "
        "holding CTRL + SHIFT)."
    };

    constexpr Property::PropertyInfo ShowMissingInclinationInfo = {
        "ShowMissingInclination",
        "Show Missing Inclination",
        "If true, glyphs without inclination data will be displayed with a dashed "
        "pattern in the inlincation glyph mode. Otherwise, they will be hidden."
    };

    const PropertyOwner::PropertyOwnerInfo StarGlyphInfo = {
        "StarGlyph",
        "Star Glyph",
        "Controls properties for the part for the glyph that are related to the star "
        "itself, rather than individual planets."
    };

    constexpr Property::PropertyInfo ShowStarLineInfo = {
        "Enabled",
        "Enabled",
        "If true, the inclination glyphs include a line in the direction of Earth."
    };

    constexpr Property::PropertyInfo StarLineColorInfo = {
        "LineColor",
        "Line color",
        "The color of the lines from the stars towards the center in inclination mode."
    };

    constexpr Property::PropertyInfo StarLineWidthInfo = {
        "LineWidth",
        "Line width",
        "The width of the lines from the stars towards the center in inclination mode."
    };

    constexpr Property::PropertyInfo StarLineLengthInfo = {
        "LineLength",
        "Line length",
        "A factor controlling the length of the lines from the stars towards the center "
        "in inclination mode."
    };

    struct [[codegen::Dictionary(RenderableExoplanetGlyphCloud)]] Parameters {
        // [[codegen::verbatim(ScaleInfo.description)]]
        std::optional<float> scale;

        // [[codegen::verbatim(SelectionInfo.description)]]
        std::optional<std::vector<int>> selection;

        // [[codegen::verbatim(UseFixedWidthInfo.description)]]
        std::optional<bool> useFixedWidth;

        // [[codegen::verbatim(DarkenFactorInfo.description)]]
        std::optional<float> darkenFactor [[codegen::inrange(0.f, 1.f)]];

        enum class [[codegen::map(OrientationRenderOption)]] OrientationRenderOption {
            ViewDirection [[codegen::key("Camera View Direction")]],
            PositionNormal [[codegen::key("Camera Position Normal")]]
        };

        // The billboard orientation to use for rendering the points.
        // This can be either "Camera View Direction" or "Camera Position Normal".
        std::optional<OrientationRenderOption> billboard;

        enum class [[codegen::map(GlyphMode)]] GlyphMode {
            Rings,
            Inclination
        };

        // [[codegen::verbatim(GlyphModeInfo.description)]]
        std::optional<GlyphMode> glyphMode;

        // [[codegen::verbatim(ShowMissingInclinationInfo.description)]]
        std::optional<bool> showMissingInclination;

        struct StarGlyph {
            // [[codegen::verbatim(ShowStarLineInfo.description)]]
            std::optional<bool> enabled;

            // [[codegen::verbatim(StarLineColorInfo.description)]]
            std::optional<glm::vec4> lineColor [[codegen::color()]];

            // [[codegen::verbatim(StarLineWidthInfo.description)]]
            std::optional<float> lineWidth [[codegen::greater(0.f)]];

            // [[codegen::verbatim(StarLineLengthInfo.description)]]
            std::optional<float> lineLength [[codegen::greater(0.f)]];
        };

        // [[codegen::verbatim(StarGlyphInfo.description)]]
        std::optional<StarGlyph> _starGlyph;
    };
#include "renderableexoplanetglyphcloud_codegen.cpp"
} // namespace

namespace openspace::exoplanets {

Documentation RenderableExoplanetGlyphCloud::Documentation() {
    return codegen::doc<Parameters>(
        "exoplanetsexperttool_renderable_exoplanetglyphcloud"
    );
}

RenderableExoplanetGlyphCloud::RenderableExoplanetGlyphCloud(
                                                     const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _scale(ScaleInfo, 1.f, 0.f, 10.f)
    , _selectedIndices(SelectionInfo)
    , _useFixedRingWidth(UseFixedWidthInfo, true)
    , _orientationRenderOption(OrientationRenderOptionInfo)
    , _glyphMode(GlyphModeInfo)
    , _darkenFactor(DarkenFactorInfo, 0.3f, 0.f, 1.f)
    , _showMissingInclination(ShowMissingInclinationInfo, false)
    , _starGlyph{
        .owner = PropertyOwner(StarGlyphInfo),
        .enabled = BoolProperty(ShowStarLineInfo, true),
        .lineColor = Vec4Property(
            StarLineColorInfo,
            glm::vec4(1.f, 0.f, 0.f, 0.2f),
            glm::vec4(0.f),
            glm::vec4(1.f)
        ),
        .lineWidth = FloatProperty(StarLineWidthInfo, 2.f, 0.01f, 3.f),
        .lineLength = FloatProperty(StarLineLengthInfo, 1.f, 0.01f, 3.f)
    }
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(Fadeable::_opacity);

    _scale = p.scale.value_or(_scale);
    addProperty(_scale);

    _selectedIndices = p.selection.value_or(_selectedIndices);
    _selectedIndices.onChange([this]() { _selectionChanged = true; });
    _selectedIndices.setReadOnly(true);
    addProperty(_selectedIndices);

    _useFixedRingWidth = p.useFixedWidth.value_or(_useFixedRingWidth);
    addProperty(_useFixedRingWidth);

    _orientationRenderOption.addOption(
        OrientationRenderOption::ViewDirection,
        "Camera View Direction"
    );
    _orientationRenderOption.addOption(
        OrientationRenderOption::PositionNormal,
        "Camera Position Normal"
    );

    _orientationRenderOption = p.billboard.has_value() ?
        codegen::map<OrientationRenderOption>(*p.billboard) :
        OrientationRenderOption::ViewDirection;

    addProperty(_orientationRenderOption);

    _glyphMode.addOption(GlyphMode::Rings, "Rings");
    _glyphMode.addOption(GlyphMode::Inclination, "Inclination");
    _glyphMode = p.glyphMode.has_value() ?
        codegen::map<GlyphMode>(*p.glyphMode) :
        GlyphMode::Rings;
    _glyphMode.onChange([this]() { _glyphModeChanged = true; });
    addProperty(_glyphMode);

    _darkenFactor = p.darkenFactor.value_or(_darkenFactor);
    addProperty(_darkenFactor);

    _showMissingInclination = p.showMissingInclination.value_or(_showMissingInclination);
    _showMissingInclination.onChange([this]() {
        // Force a recomputation of the render data
        _glyphModeChanged = true;
    });
    addProperty(_showMissingInclination);

    if (p._starGlyph.has_value()) {
        const Parameters::StarGlyph& params = *p._starGlyph;
        _starGlyph.enabled = params.enabled.value_or(_starGlyph.enabled);
        _starGlyph.lineColor = params.lineColor.value_or(_starGlyph.lineColor);
        _starGlyph.lineWidth = params.lineWidth.value_or(_starGlyph.lineWidth);
        _starGlyph.lineLength = params.lineLength.value_or(_starGlyph.lineLength);
    }

    _starGlyph.owner.addProperty(_starGlyph.enabled);
    _starGlyph.lineColor.setViewOption(Property::ViewOptions::Color);
    _starGlyph.owner.addProperty(_starGlyph.lineColor);
    _starGlyph.owner.addProperty(_starGlyph.lineWidth);
    _starGlyph.owner.addProperty(_starGlyph.lineLength);
    addPropertySubOwner(_starGlyph.owner);

    updateDataIfChanged();

    initializeSelectionCallbacks();

    _currentlyHoveredIndex = -1;
}

void RenderableExoplanetGlyphCloud::initializeSelectionCallbacks() {
    // Only initialize callbacks for the master instance
    if (!global::windowDelegate->isMaster()) {
        return;
    }

    global::callback::keyboard->emplace_back(
        [&](Key key, KeyModifier, KeyAction action, bool) -> bool {
            if (!_enabled) {
                return false;
            }

            bool isCtrl = key == Key::LeftControl;
            if (isCtrl && action == KeyAction::Press) {
                _isInSelectionMode = true;
            }
            else if (isCtrl && action == KeyAction::Release) {
                _isInSelectionMode = false;
                _currentlyHoveredIndex = -1;
            }

            // Extra check for left shift, used for extra highlighting
            bool isLeftShift = key == Key::LeftShift;
            if (isLeftShift && action == KeyAction::Press) {
                _isLeftShiftHeld = true;
            }
            else if (isLeftShift && action == KeyAction::Release) {
                _isLeftShiftHeld = false;
            }

            // Do not capture
            return false;
        }
    );

    global::callback::mousePosition->emplace_back(
        [&](double x, double y, bool) {
            if (!_enabled || !_isInSelectionMode) {
                return; // do nothing
            }

            // Convert mouse position to pixel position
            glm::dvec2 lastViewportSize = glm::dvec2(_lastViewPortSize);
            double normalizedX = x / lastViewportSize.x;
            double normalizedY = (lastViewportSize.y - y) / lastViewportSize.y;

            if (_glyphIdTexture) {
                glm::uvec3 textureDim = _glyphIdTexture->dimensions();
                glm::uvec2 pos = glm::uvec2(
                    normalizedX * static_cast<double>(textureDim.x),
                    normalizedY * static_cast<double>(textureDim.y)
                );

                _glyphIdTexture->downloadTexture();
                // TODO: make sure pos is within texture
                if (pos.x < textureDim.x && pos.y < textureDim.y) {
                    glm::vec4 pixelValue = _glyphIdTexture->texelAsFloat({ pos, 0 });
                    _currentlyHoveredIndex =
                        static_cast<int>(std::round(pixelValue.r * _maxIndex)) - 1;
                }
                _glyphIdTexture->clearDownloadedTexture();
            }
        }
    );

    global::callback::preSync->emplace_back([this]() {
        if (!_enabled) {
            return;
        }
        // Update flag that needs syncing
        _shouldHighlightHovered = _isInSelectionMode && _isLeftShiftHeld;
    });
}

int RenderableExoplanetGlyphCloud::hoveredIndex() const {
    return _currentlyHoveredIndex;
}

void RenderableExoplanetGlyphCloud::initialize() {
    global::syncEngine->addSyncables({ &_currentlyHoveredIndex, &_shouldHighlightHovered });
}

void RenderableExoplanetGlyphCloud::initializeGL() {
    initializeShaders();

    // Generate texture and frame buffer for rendering glyph id
    glCreateFramebuffers(1, &_glyphIdFbo);
    createGlyphIdTexture(glm::uvec3(1080, 720, 1));

    // Give the framebuffer a reasonable name (for RonderDoc debugging)
    if (glbinding::Binding::ObjectLabel.isResolved()) {
        glObjectLabel(GL_FRAMEBUFFER, _glyphIdFbo, -1, "Glyph ID Framebuffer");
    }

    // Check status
    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Framebuffer is not complete!");
    }

    glCreateVertexArrays(1, &_pointsVao);
    glCreateBuffers(1, &_pointsVbo);

    glCreateVertexArrays(1, &_selectedVao);
    glCreateBuffers(1, &_selectedVbo);

    glCreateVertexArrays(1, &_starsVao);
    glCreateBuffers(1, &_starsVbo);
}

void RenderableExoplanetGlyphCloud::deinitialize() {
    global::syncEngine->removeSyncables({ &_currentlyHoveredIndex, &_shouldHighlightHovered });
}

void RenderableExoplanetGlyphCloud::deinitializeGL() {
    glDeleteVertexArrays(1, &_pointsVao);
    glDeleteBuffers(1, &_pointsVbo);

    glDeleteVertexArrays(1, &_selectedVao);
    glDeleteBuffers(1, &_selectedVbo);

    glDeleteVertexArrays(1, &_starsVao);
    glDeleteBuffers(1, &_starsVbo);

    if (_programRings) {
        global::renderEngine->removeRenderProgram(_programRings.get());
        _programRings = nullptr;
    }

    if (_programInclination) {
        global::renderEngine->removeRenderProgram(_programInclination.get());
        _programInclination = nullptr;
    }

    if (_programStars) {
        global::renderEngine->removeRenderProgram(_programStars.get());
        _programStars = nullptr;
    }
}

void RenderableExoplanetGlyphCloud::initializeShaders() {
    _programRings = global::renderEngine->buildRenderProgram(
        "ExoGlyphCloud_Rings",
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/glyphs_vs.glsl"),
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/glyphs_fs.glsl"),
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/glyphs_gs.glsl")
    );
    ghoul::opengl::updateUniformLocations(*_programRings, _uniformCacheRings);

    _programInclination = global::renderEngine->buildRenderProgram(
        "ExoGlyphCloud_Inclination",
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/glyphs_inclination_vs.glsl"),
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/glyphs_inclination_fs.glsl"),
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/glyphs_inclination_gs.glsl")
    );
    ghoul::opengl::updateUniformLocations(*_programInclination, _uniformCacheInclination);

    _programStars = global::renderEngine->buildRenderProgram(
        "ExoGlyphCloud_StarGlyph",
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/glyphs_star_vs.glsl"),
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/glyphs_star_fs.glsl"),
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/glyphs_star_gs.glsl")
    );
    ghoul::opengl::updateUniformLocations(*_programStars, _uniformCacheStars);
}

void RenderableExoplanetGlyphCloud::render(const RenderData& data, RendererTasks&) {
    if (_glyphData.empty()) {
        return;
    }

    // Select the appropriate program and uniform cache based on mode
    ghoul::opengl::ProgramObject* program = nullptr;
    if (_glyphMode == GlyphMode::Rings) {
        program = _programRings.get();
    }
    else if (_glyphMode == GlyphMode::Inclination) {
        program = _programInclination.get();
    }
    else {
        throw ghoul::MissingCaseException();
    }

    program->activate();

    // Setup all uniforms
    setupCommonUniforms(*program, data);

    if (_glyphMode == GlyphMode::Rings) {
        setupRingsSpecificUniforms(*program, data);
    }

    // 1st rendering pass: render the glyphs normally, with correct color
    renderMainPass();

    // 2nd rendering pass: Render IDs to a separate texture every frame as well
    // To use for picking. We only need to do this on the master node
    if (global::windowDelegate->isMaster()) {
        renderIndexTexture(*program);
    }

    // 3rd rendering pass: Render selected points on top
    renderSelectedPoints(*program);

    program->deactivate();
    glBindVertexArray(0);

    // 4th rendering pass: Render the star glyphs (lines from stars to center)
    renderStars(data);

    // Restores GL State
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
}

void RenderableExoplanetGlyphCloud::setupCommonUniforms(
                                                    ghoul::opengl::ProgramObject& program,
                                                                   const RenderData& data)
{
    const glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    const glm::dmat4 viewProjectionMatrix =
        glm::dmat4(data.camera.projectionMatrix()) * data.camera.combinedViewMatrix();

    // Get the correct uniform cache based on mode
    if (_glyphMode == GlyphMode::Rings) {
        program.setUniform(_uniformCacheRings.modelMatrix, modelTransform);
        program.setUniform(_uniformCacheRings.cameraViewProjectionMatrix, viewProjectionMatrix);
        program.setUniform(_uniformCacheRings.opacity, opacity());
        program.setUniform(_uniformCacheRings.scale, _scale);
        program.setUniform(_uniformCacheRings.onTop, false);
        program.setUniform(_uniformCacheRings.maxIndex, _maxIndex);
        program.setUniform(_uniformCacheRings.currentIndex, _currentlyHoveredIndex + 1);
        program.setUniform(_uniformCacheRings.isHighlightMode, _shouldHighlightHovered);
        program.setUniform(_uniformCacheRings.darkenFactor, _darkenFactor);
        program.setUniform(_uniformCacheRings.cameraPosition, data.camera.position());
    }
    else if (_glyphMode == GlyphMode::Inclination) {
        program.setUniform(_uniformCacheInclination.modelMatrix, modelTransform);
        program.setUniform(_uniformCacheInclination.cameraViewProjectionMatrix, viewProjectionMatrix);
        program.setUniform(_uniformCacheInclination.opacity, opacity());
        program.setUniform(_uniformCacheInclination.scale, _scale);
        program.setUniform(_uniformCacheInclination.onTop, false);
        program.setUniform(_uniformCacheInclination.maxIndex, _maxIndex);
        program.setUniform(_uniformCacheInclination.currentIndex, _currentlyHoveredIndex + 1);
        program.setUniform(_uniformCacheInclination.isHighlightMode, _shouldHighlightHovered);
        program.setUniform(_uniformCacheInclination.darkenFactor, _darkenFactor);
        program.setUniform(_uniformCacheInclination.cameraPosition, data.camera.position());
    }

    program.setUniform("isRenderIndexStep", false);
}

void RenderableExoplanetGlyphCloud::setupRingsSpecificUniforms(
                                                    ghoul::opengl::ProgramObject& program,
                                                                   const RenderData& data)
{
    program.setUniform(_uniformCacheRings.useFixedRingWidth, _useFixedRingWidth);

    glm::dvec3 cameraViewDirectionWorld = -data.camera.viewDirectionWorldSpace();
    glm::dvec3 cameraUpDirectionWorld = data.camera.lookUpVectorWorldSpace();
    glm::dvec3 orthoRight = glm::normalize(
        glm::cross(cameraUpDirectionWorld, cameraViewDirectionWorld)
    );
    if (orthoRight == glm::dvec3(0.0)) {
        glm::dvec3 otherVector = glm::vec3(
            cameraUpDirectionWorld.y,
            cameraUpDirectionWorld.x,
            cameraUpDirectionWorld.z
        );
        orthoRight = glm::normalize(glm::cross(otherVector, cameraViewDirectionWorld));
    }
    glm::dvec3 orthoUp = glm::normalize(glm::cross(cameraViewDirectionWorld, orthoRight));

    program.setUniform(_uniformCacheRings.renderOption, _orientationRenderOption.value());
    program.setUniform(_uniformCacheRings.up, glm::vec3(orthoUp));
    program.setUniform(_uniformCacheRings.right, glm::vec3(orthoRight));
    program.setUniform(
        _uniformCacheRings.cameraLookUp,
        glm::vec3(data.camera.lookUpVectorWorldSpace())
    );
}

void RenderableExoplanetGlyphCloud::renderMainPass() {
    glEnablei(GL_BLEND, 0);
    glDepthMask(true);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glBindVertexArray(_pointsVao);
    glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(_glyphData.size()));
}

void RenderableExoplanetGlyphCloud::renderIndexTexture(
                                                    ghoul::opengl::ProgramObject& program)
{
    // Start by getting the viewport size
    GLint viewport[4];
    glGetIntegerv(GL_VIEWPORT, viewport);

    program.setUniform("isRenderIndexStep", true);
    GLint defaultFBO = ghoul::opengl::FramebufferObject::getActiveObject();

    glBindFramebuffer(GL_FRAMEBUFFER, _glyphIdFbo);
    GLenum drawBuffers[1] = { GL_COLOR_ATTACHMENT0 };
    glDrawBuffers(1, drawBuffers);

    glm::uvec3 textureDim = _glyphIdTexture->dimensions();

    // Potentially update texture size
    if (static_cast<unsigned int>(viewport[2]) != textureDim.x ||
        static_cast<unsigned int>(viewport[3]) != textureDim.y)
    {
        createGlyphIdTexture(glm::uvec3(viewport[2], viewport[3], 1));
    }

    // Clear the previous values from the texture
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    // No blending please
    glDisablei(GL_BLEND, 0);

    // Draw again! And specify viewport size
    glViewport(viewport[0], viewport[1], textureDim.x, textureDim.y);
    glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(_glyphData.size()));

    // Reset index rendering, viewport size and frame buffer
    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
    glViewport(viewport[0], viewport[1], viewport[2], viewport[3]);
    program.setUniform("isRenderIndexStep", false);
    glEnablei(GL_BLEND, 0);

    // Save viewport size
    _lastViewPortSize = glm::ivec2(viewport[2], viewport[3]);
}

void RenderableExoplanetGlyphCloud::renderSelectedPoints(
                                                    ghoul::opengl::ProgramObject& program)
{
    const size_t nSelected = _selectedIndices.value().size();
    if (nSelected == 0) {
        return;
    }

    if (_glyphMode == GlyphMode::Rings) {
        program.setUniform(_uniformCacheRings.opacity, 1.f);
        program.setUniform(_uniformCacheRings.onTop, true);
    }
    else if (_glyphMode == GlyphMode::Inclination) {
        program.setUniform(_uniformCacheInclination.opacity, 1.f);
        program.setUniform(_uniformCacheInclination.onTop, true);
    }

    glBindVertexArray(_selectedVao);
    glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(nSelected));
}

void RenderableExoplanetGlyphCloud::renderStars(const RenderData& data) {
    if (_starData.empty() || !_programStars || !_starGlyph.enabled) {
        return;
    }
    _programStars->activate();

    const glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    const glm::dmat4 viewProjectionMatrix =
        glm::dmat4(data.camera.projectionMatrix()) * data.camera.combinedViewMatrix();

    _programStars->setUniform(_uniformCacheStars.modelMatrix, modelTransform);
    _programStars->setUniform(_uniformCacheStars.cameraViewProjectionMatrix, viewProjectionMatrix);
    _programStars->setUniform(_uniformCacheStars.opacity, opacity());
    _programStars->setUniform(_uniformCacheStars.scale, _scale);
    _programStars->setUniform(_uniformCacheStars.cameraPosition, data.camera.position());
    _programStars->setUniform(_uniformCacheStars.originLineColor, _starGlyph.lineColor);
    _programStars->setUniform(_uniformCacheStars.lineLengthFactor, _starGlyph.lineLength);

    glLineWidth(_starGlyph.lineWidth);

    glEnablei(GL_BLEND, 0);
    glDepthMask(true);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glBindVertexArray(_starsVao);
    glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(_starData.size()));
};

void RenderableExoplanetGlyphCloud::update(const UpdateData&) {
    if (_programInclination->isDirty()) {
        _programInclination->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_programInclination, _uniformCacheInclination);
    }

    if (_programRings->isDirty()) {
        _programRings->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_programRings, _uniformCacheRings);
    }

    if (_programStars->isDirty()) {
        _programStars->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_programStars, _uniformCacheStars);
    }

    updateDataIfChanged();

    if (_renderDataIsDirty) {
        glNamedBufferData(
            _pointsVbo,
            _glyphData.size() * sizeof(GlyphData),
            _glyphData.data(),
            GL_STATIC_DRAW
        );
        mapVertexAttributes(_pointsVao);
        glVertexArrayVertexBuffer(_pointsVao, 0, _pointsVbo, 0, sizeof(GlyphData));

        glNamedBufferData(
            _starsVbo,
            _starData.size() * sizeof(StarGlyphData),
            _starData.data(),
            GL_STATIC_DRAW
        );

        // Location 0: in_position
        glEnableVertexArrayAttrib(_starsVao, 0);
        glVertexArrayAttribBinding(_starsVao, 0, 0);
        glVertexArrayAttribFormat(
            _starsVao, 0, 3, GL_FLOAT, GL_FALSE,
            offsetof(StarGlyphData, position)
        );

        // Location 1: in_up
        glEnableVertexArrayAttrib(_starsVao, 1);
        glVertexArrayAttribBinding(_starsVao, 1, 0);
        glVertexArrayAttribFormat(
            _starsVao, 1, 3, GL_FLOAT, GL_FALSE,
            offsetof(StarGlyphData, up)
        );

        glVertexArrayVertexBuffer(_starsVao, 0, _starsVbo, 0, sizeof(StarGlyphData));

        _renderDataIsDirty = false;
    }

    if (_selectionChanged) {
        const int nSelected = static_cast<int>(_selectedIndices.value().size());
        std::vector<GlyphData> selectedPoints;
        std::vector<int> newIndices;
        selectedPoints.reserve(nSelected);
        newIndices.reserve(nSelected);

        // For each of the selected indices, find the corresponding point
        for (int i : _selectedIndices.value()) {
            std::vector<size_t>::iterator pos =
                std::find(_glyphIndices.begin(), _glyphIndices.end(), i);

            if (pos != _glyphIndices.end()) {
                const int index = static_cast<int>(pos - _glyphIndices.begin());
                const GlyphData& p = _glyphData.at(index);
                selectedPoints.push_back(p);
                newIndices.push_back(i);
            }
            else {
                LINFO(std::format("No 3D point matching selected index '{}'", i));
            }
        }
        selectedPoints.shrink_to_fit();

        _selectedIndices = newIndices;

        if (selectedPoints.size() > 0) {
            glNamedBufferData(
                _selectedVbo,
                selectedPoints.size() * sizeof(GlyphData),
                selectedPoints.data(),
                GL_STATIC_DRAW
            );
            mapVertexAttributes(_selectedVao);
            glVertexArrayVertexBuffer(_selectedVao, 0, _selectedVbo, 0, sizeof(GlyphData));
        }

        _selectionChanged = false;
    }

    _glyphModeChanged = false;
}

void RenderableExoplanetGlyphCloud::createGlyphIdTexture(const glm::uvec3 dimensions) {
    // TODO (emmbr, 2022-11-15): at some point try using a integer value for the texture instead.
    // But for now, just make it work!
    _glyphIdTexture = std::make_unique<ghoul::opengl::Texture>(
        ghoul::opengl::Texture::FormatInit{
            .dimensions = dimensions,
            .type = GL_TEXTURE_2D,
            .format = ghoul::opengl::Texture::Format::RGBA,
            .dataType = GL_FLOAT
        },
        ghoul::opengl::Texture::SamplerInit{
            .filter = ghoul::opengl::Texture::FilterMode::Nearest
        }
    );

    // And a depth buffer of the same dimension
    _depthTexture = std::make_unique<ghoul::opengl::Texture>(
        ghoul::opengl::Texture::FormatInit{
            .dimensions = dimensions,
            .type = GL_TEXTURE_2D,
            .format = ghoul::opengl::Texture::Format::DepthComponent,
            .dataType = GL_FLOAT
        },
        ghoul::opengl::Texture::SamplerInit{
            .filter = ghoul::opengl::Texture::FilterMode::Linear
        }
    );

    glNamedFramebufferTexture(_glyphIdFbo, GL_COLOR_ATTACHMENT0, *_glyphIdTexture, 0);
    glNamedFramebufferTexture(_glyphIdFbo, GL_DEPTH_ATTACHMENT, *_depthTexture, 0);
}

void RenderableExoplanetGlyphCloud::mapVertexAttributes(GLuint vao) {
    // First the attributes common to both modes

    // Location 0: in_position
    glEnableVertexArrayAttrib(vao, 0);
    glVertexArrayAttribBinding(vao, 0, 0);
    glVertexArrayAttribFormat(
        vao, 0, 3, GL_FLOAT, GL_FALSE,
        offsetof(GlyphData, position)
    );

    // Location 1: in_component
    glEnableVertexArrayAttrib(vao, 1);
    glVertexArrayAttribBinding(vao, 1, 0);
    glVertexArrayAttribFormat(
        vao, 1, 1, GL_FLOAT, GL_FALSE,
        offsetof(GlyphData, component)
    );

    // Location 2: in_glyphIndex
    glEnableVertexArrayAttrib(vao, 2);
    glVertexArrayAttribBinding(vao, 2, 0);
    glVertexArrayAttribIFormat(
        vao, 2, 1, GL_INT,
        offsetof(GlyphData, index)
    );

    if (_glyphMode == GlyphMode::Rings) {
        // Location 3: in_nColors
        glEnableVertexArrayAttrib(vao, 3);
        glVertexArrayAttribBinding(vao, 3, 0);
        glVertexArrayAttribIFormat(
            vao, 3, 1, GL_INT,
            offsetof(GlyphData, nColors)
        );

        // Locations 4-7: in_colors[4] array
        for (int i = 0; i < 4; i++) {
            int location = 4 + i;
            glEnableVertexArrayAttrib(vao, location);
            glVertexArrayAttribBinding(vao, location, 0);
            glVertexArrayAttribFormat(
                vao, location, 4, GL_FLOAT, GL_FALSE,
                offsetof(GlyphData, colors) + i * 4 * sizeof(float)
            );
        }
    }
    else if (_glyphMode == GlyphMode::Inclination) {
        // Location 3: in_inclinationVector
        glEnableVertexArrayAttrib(vao, 3);
        glVertexArrayAttribBinding(vao, 3, 0);
        glVertexArrayAttribFormat(
            vao, 3, 3, GL_FLOAT, GL_FALSE,
            offsetof(GlyphData, inclinationVector)
        );

        // Location 4: in_hasInclinationFlag
        glEnableVertexArrayAttrib(vao, 4);
        glVertexArrayAttribBinding(vao, 4, 0);
        glVertexArrayAttribIFormat(
            vao, 4, 1, GL_INT,
            offsetof(GlyphData, hasInclination)
        );

        // Location 5: in_colors[0] (only one color used for inclination mode)
        glVertexArrayAttribFormat(
            vao, 5, 4, GL_FLOAT, GL_FALSE,
            offsetof(GlyphData, colors)
        );
    }
    else throw ghoul::MissingCaseException();
}

void RenderableExoplanetGlyphCloud::updateDataIfChanged() {
    auto mod = global::moduleEngine->module<ExoplanetsExpertToolModule>();

    using GlyphRenderData = ExoplanetsExpertToolModule::GlyphRenderData;
    GlyphRenderData syncedData = mod->glyphRenderData();

    // Check if timestamp was updated, to avoid unnecessary updates. But always update
    // if the glyph mode changed, since the data needs to be reprocessed for the new mode
    if (!_glyphModeChanged && (syncedData.timeStamp <= _lastDataTimeStamp)) {
        return; // No update
    }

    LDEBUG("Got new data. Updating data for rendering");

    _lastDataTimeStamp = syncedData.timeStamp;

    _glyphData.clear();
    _glyphIndices.clear();

    // Read number of data points
    size_t nPoints = syncedData.items.size();

    _glyphData.reserve(nPoints);
    _glyphIndices.reserve(nPoints);
    int maxIndex = -1;

    _starData.clear();
    _starData.reserve(nPoints);
    std::vector<glm::vec3> uniquePositions;

    for (const GlyphRenderData::Item& item : syncedData.items) {
        GlyphData d;

        // Position is given in parsec
        d.position = glm::vec3(item.position * distanceconstants::Parsec);
        d.component = static_cast<float>(item.component);

        // Increase by one to avoid having 0 as a valid index, since we use 0 in the
        // shader to indicate "no point"
        d.index = item.index + 1;

        // Clear all color slots first (defensive programming)
        for (size_t i = 0; i < MaxNumberColors; i++) {
            d.colors[i] = glm::vec4(0.0f);
        }

        // Mode-specific data population
        if (_glyphMode == GlyphMode::Rings) {
            size_t nColors = item.colors.size();
            d.nColors = static_cast<int>(nColors);

            // Limit the number of colors to the maximum supported by the shader
            size_t temp = std::min(nColors, MaxNumberColors);
            for (size_t j = 0; j < temp; j++) {
                d.colors[j] = item.colors[j];
            }
        }
        else if (_glyphMode == GlyphMode::Inclination) {
            // Rotation corresponding to a plane whose normal is facing Earth
            glm::dmat3 systemRotation = computeSystemRotation(d.position);

            glm::dmat3 planeRotation;

            bool hasInclination = !std::isnan(item.inclination);
            if (!hasInclination && !_showMissingInclination) {
                continue;
            }
            else if (!hasInclination) {
                // Use a default inclination of 90 degrees, which is edge-on, to show the
                // orbit plane the same way as it is render in close up view
                planeRotation = computeOrbitPlaneRotationMatrix(90.f);
                d.hasInclination = 0;
            }
            else {
                // Use the real inclination data!
                planeRotation = computeOrbitPlaneRotationMatrix(item.inclination);
                d.hasInclination = 1;
            }

            // This is the up vector of the orbit plane, in world space
            d.inclinationVector = glm::normalize(
                systemRotation * planeRotation * glm::vec3(0.f, 0.f, 1.f)
            );

            // Use just one of the colors (more becomes too visually complex)
            d.colors[0] = item.colors[0];

            // Add data for star glyph
            if (std::find(uniquePositions.begin(), uniquePositions.end(), d.position) ==
                uniquePositions.end())
            {
                uniquePositions.push_back(d.position);
                _starData.push_back({
                    .position = d.position,
                    .up = systemRotation * glm::vec3(0.f, 1.f, 0.f)
                });
            }
        }

        if (static_cast<int>(d.index) > maxIndex) {
            maxIndex = static_cast<int>(d.index);
        }

        _glyphData.push_back(std::move(d));
        _glyphIndices.push_back(item.index);
    }

    _starData.shrink_to_fit();

    _maxIndex = maxIndex;

    _renderDataIsDirty = true;
}

} // namespace openspace::exoplanets
