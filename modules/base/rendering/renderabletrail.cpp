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

#include <modules/base/rendering/renderabletrail.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/translation.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>

namespace {
    constexpr const char* ProgramName = "EphemerisProgram";
    constexpr const char* KeyTranslation = "Translation";

    constexpr const std::array<const char*, 14> UniformNames = {
        "opacity", "modelViewTransform", "projectionTransform", "color", "useLineFade",
        "lineFade", "vertexSortingMethod", "idOffset", "nVertices", "stride", "pointSize",
        "renderPhase", "resolution", "lineWidth"
    };

    // The possible values for the _renderingModes property
    enum RenderingMode {
        RenderingModeLines = 0,
        RenderingModePoints,
        RenderingModeLinesPoints
    };

    // Fragile! Keep in sync with documentation
    const std::map<std::string, RenderingMode> RenderingModeConversion = {
        { "Lines", RenderingModeLines },
        { "Points", RenderingModePoints },
        { "Lines+Points", RenderingModeLinesPoints },
        { "Points+Lines", RenderingModeLinesPoints }
    };

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        AppearanceInfo = {
            "Appearance",
            "Appearance",
            "The appearance of the trail."
    };

    constexpr openspace::properties::Property::PropertyInfo LineColorInfo = {
        "Color",
        "Color",
        "This value determines the RGB main color for the lines and points of the trail."
    };

    constexpr openspace::properties::Property::PropertyInfo EnableFadeInfo = {
        "EnableFade",
        "Enable line fading of old points",
        "Toggles whether the trail should fade older points out. If this value is "
        "'true', the 'Fade' parameter determines the speed of fading. If this value is "
        "'false', the entire trail is rendered at full opacity and color."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeInfo = {
        "Fade",
        "Line fade",
        "The fading factor that is applied to the trail if the 'EnableFade' value is "
        "'true'. If it is 'false', this setting has no effect. The higher the number, "
        "the less fading is applied."
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "This value specifies the line width of the trail if the selected rendering "
        "method includes lines. If the rendering mode is set to Points, this value is "
        "ignored."
    };

    constexpr openspace::properties::Property::PropertyInfo PointSizeInfo = {
        "PointSize",
        "Point Size",
        "This value specifies the base size of the points along the line if the selected "
        "rendering method includes points. If the rendering mode is set the Lines, this "
        "value is ignored. If a subsampling of the values is performed, the subsampled "
        "values are half this size."
    };

    constexpr openspace::properties::Property::PropertyInfo RenderingModeInfo = {
        "Rendering",
        "Rendering Mode",
        "Determines how the trail should be rendered to the screen.If 'Lines' is "
        "selected, only the line part is visible, if 'Points' is selected, only the "
        "corresponding points (and subpoints) are shown. 'Lines+Points' shows both parts."
    };

} // namespace

namespace openspace {

documentation::Documentation RenderableTrail::Documentation() {
    using namespace documentation;
    return {
        "RenderableTrail",
        "base_renderable_renderabletrail",
        {
            {
                KeyTranslation,
                new ReferencingVerifier("core_transform_translation"),
                Optional::No,
                "This object is used to compute locations along the path. Any "
                "Translation object can be used here."
            },
            {
                LineColorInfo.identifier,
                new DoubleVector3Verifier,
                Optional::No,
                LineColorInfo.description
            },
            {
                EnableFadeInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                EnableFadeInfo.description
            },
            {
                FadeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                FadeInfo.description
            },
            {
                LineWidthInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                LineWidthInfo.description
            },
            {
                PointSizeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                PointSizeInfo.description
            },
            {
                RenderingModeInfo.identifier,
                new StringInListVerifier(
                    // Taken from the RenderingModeConversion map above
                    { "Lines", "Points", "Lines+Points", "Points+Lines" }
                ),
                Optional::Yes,
                RenderingModeInfo.description
            }
        }
    };
}

RenderableTrail::Appearance::Appearance()
    : properties::PropertyOwner(AppearanceInfo)
    , lineColor(LineColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , useLineFade(EnableFadeInfo, true)
    , lineFade(FadeInfo, 1.f, 0.f, 30.f)
    , lineWidth(LineWidthInfo, 10.f, 1.f, 20.f)
    , pointSize(PointSizeInfo, 1, 1, 64)
    , renderingModes(
          RenderingModeInfo,
          properties::OptionProperty::DisplayType::Dropdown
    )
{
    renderingModes.addOptions({
        { RenderingModeLines, "Lines" },
        { RenderingModePoints, "Points" },
        { RenderingModeLinesPoints, "Lines+Points" }
    });

    addProperty(lineColor);
    addProperty(useLineFade);
    addProperty(lineFade);
    addProperty(lineWidth);
    addProperty(pointSize);
    addProperty(renderingModes);
}

RenderableTrail::RenderableTrail(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
{
    setRenderBin(RenderBin::Overlay);
    addProperty(_opacity);

    _translation = Translation::createFromDictionary(
        dictionary.value<ghoul::Dictionary>(KeyTranslation)
    );
    addPropertySubOwner(_translation.get());

    _appearance.lineColor = dictionary.value<glm::vec3>(LineColorInfo.identifier);

    if (dictionary.hasKeyAndValue<bool>(EnableFadeInfo.identifier)) {
        _appearance.useLineFade = dictionary.value<bool>(EnableFadeInfo.identifier);
    }

    if (dictionary.hasKeyAndValue<double>(FadeInfo.identifier)) {
        _appearance.lineFade = static_cast<float>(
            dictionary.value<double>(FadeInfo.identifier)
        );
    }

    if (dictionary.hasKeyAndValue<double>(LineWidthInfo.identifier)) {
        _appearance.lineWidth = static_cast<float>(dictionary.value<double>(
            LineWidthInfo.identifier
        ));
    }

    if (dictionary.hasKeyAndValue<double>(PointSizeInfo.identifier)) {
        _appearance.pointSize = static_cast<int>(
            dictionary.value<double>(PointSizeInfo.identifier)
        );
    }

    // This map is not accessed out of order as long as the Documentation is adapted
    // whenever the map changes. The documentation will check for valid values
    if (dictionary.hasKeyAndValue<std::string>(RenderingModeInfo.identifier)) {
        _appearance.renderingModes = RenderingModeConversion.at(
            dictionary.value<std::string>(RenderingModeInfo.identifier)
        );
    }
    else {
        _appearance.renderingModes = RenderingModeLines;
    }

    addPropertySubOwner(_appearance);
}

void RenderableTrail::initializeGL() {
#ifdef __APPLE__
    _programObject = BaseModule::ProgramObjectManager.request(
        ProgramName,
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine.buildRenderProgram(
                ProgramName,
                absPath("${MODULE_BASE}/shaders/renderabletrail_apple_vs.glsl"),
                absPath("${MODULE_BASE}/shaders/renderabletrail_apple_fs.glsl")
            );
        }
    );
#else
    _programObject = BaseModule::ProgramObjectManager.request(
        ProgramName,
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine.buildRenderProgram(
                ProgramName,
                absPath("${MODULE_BASE}/shaders/renderabletrail_vs.glsl"),
                absPath("${MODULE_BASE}/shaders/renderabletrail_fs.glsl")
            );
        }
    );
#endif

    ghoul::opengl::updateUniformLocations(*_programObject, _uniformCache, UniformNames);
}

void RenderableTrail::deinitializeGL() {
    BaseModule::ProgramObjectManager.release(
        ProgramName,
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine.removeRenderProgram(p);
        }
    );
    _programObject = nullptr;
}

bool RenderableTrail::isReady() const {
    return _programObject != nullptr;
}

void RenderableTrail::render(const RenderData& data, RendererTasks&) {
    _programObject->activate();
    _programObject->setUniform(_uniformCache.opacity, _opacity);

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    _programObject->setUniform(_uniformCache.projection, data.camera.projectionMatrix());

    _programObject->setUniform(_uniformCache.color, _appearance.lineColor);
    _programObject->setUniform(_uniformCache.useLineFade, _appearance.useLineFade);
    if (_appearance.useLineFade) {
        _programObject->setUniform(_uniformCache.lineFade, _appearance.lineFade);
    }

    /*glm::ivec2 resolution = global::renderEngine.renderingResolution();
    _programObject->setUniform(_uniformCache.resolution, resolution);*/

    static std::map<RenderInformation::VertexSorting, int> SortingMapping = {
        // Fragile! Keep in sync with shader
        { RenderInformation::VertexSorting::NewestFirst, 0 },
        { RenderInformation::VertexSorting::OldestFirst, 1 },
        { RenderInformation::VertexSorting::NoSorting, 2}
    };

    const bool usingFramebufferRenderer =
        global::renderEngine.rendererImplementation() ==
        RenderEngine::RendererImplementation::Framebuffer;

    if (usingFramebufferRenderer) {
        glDepthMask(false);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    }

    const bool renderLines = (_appearance.renderingModes == RenderingModeLines) |
                             (_appearance.renderingModes == RenderingModeLinesPoints);

    const bool renderPoints = (_appearance.renderingModes == RenderingModePoints) |
                              (_appearance.renderingModes == RenderingModeLinesPoints);

    if (renderLines) {
#ifdef __APPLE__
        glLineWidth(1.f);
#else
        glLineWidth(ceil((2.f * 1.f + _appearance.lineWidth) * std::sqrt(2.f)));
#endif
    }
    if (renderPoints) {
        glEnable(GL_PROGRAM_POINT_SIZE);
    }

    auto render = [renderLines, renderPoints, p = _programObject, &data,
                   &modelTransform, pointSize = _appearance.pointSize.value(),
                   c = _uniformCache, lw = _appearance.lineWidth]
                  (RenderInformation& info, int nVertices, int offset)
    {
        // We pass in the model view transformation matrix as double in order to maintain
        // high precision for vertices; especially for the trails, a high vertex precision
        // is necessary as they are usually far away from their reference
        p->setUniform(
            c.modelView,
            data.camera.combinedViewMatrix() * modelTransform * info._localTransform
        );

        // The vertex sorting method is used to tweak the fading along the trajectory
        p->setUniform(c.vertexSorting, SortingMapping[info.sorting]);

        // This value is subtracted from the vertex id in the case of a potential ring
        // buffer (as used in RenderableTrailOrbit) to keep the first vertex at its
        // brightest
        p->setUniform(c.idOffset, offset);

        p->setUniform(c.nVertices, nVertices);

        #ifndef __APPLE__
                glm::ivec2 resolution = global::renderEngine.renderingResolution();
                p->setUniform(c.resolution, resolution);

                p->setUniform(c.lineWidth, ceil((2.f * 1.f + lw) * std::sqrt(2.f)));
        #endif

        if (renderPoints) {
            // The stride parameter determines the distance between larger points and
            // smaller ones
            p->setUniform(c.stride, info.stride);
            p->setUniform(c.pointSize, pointSize);
        }

        // Fragile! Keep in sync with fragment shader
        enum RenderPhase {
            RenderPhaseLines = 0,
            RenderPhasePoints
        };

        glBindVertexArray(info._vaoID);
        if (renderLines) {
            p->setUniform(c.renderPhase, RenderPhaseLines);
            // Subclasses of this renderer might be using the index array or might now be
            // so we check if there is data available and if there isn't, we use the
            // glDrawArrays draw call; otherwise the glDrawElements
            if (info._iBufferID == 0) {
                glDrawArrays(
                    GL_LINE_STRIP,
                    info.first,
                    info.count
                );
            }
            else {
                glDrawElements(
                    GL_LINE_STRIP,
                    info.count,
                    GL_UNSIGNED_INT,
                    reinterpret_cast<void*>(info.first * sizeof(unsigned int))
                );
            }
        }
        if (renderPoints) {
            // Subclasses of this renderer might be using the index array or might now be
            // so we check if there is data available and if there isn't, we use the
            // glDrawArrays draw call; otherwise the glDrawElements
            p->setUniform(c.renderPhase, RenderPhasePoints);
            if (info._iBufferID == 0) {
                glDrawArrays(GL_POINTS, info.first, info.count);
            }
            else {
                glDrawElements(
                    GL_POINTS,
                    info.count,
                    GL_UNSIGNED_INT,
                    reinterpret_cast<void*>(info.first * sizeof(unsigned int))
                );
            }
        }
    };

    // The combined size of vertices; -1 because we duplicate the penultimate point
    const int totalNumber = _primaryRenderInformation.count +
                            _floatingRenderInformation.count - 1;

    // The primary information might use an index buffer, so we might need to start at an
    // offset
    const int primaryOffset = (_primaryRenderInformation._iBufferID == 0) ?
        0 :
        _primaryRenderInformation.first;

    // Culling
    const double scaledRadius = glm::length(
        glm::dmat3(modelTransform) * glm::dvec3(_boundingSphere, 0.0, 0.0)
    );

    glm::dvec3 trailPosWorld = glm::dvec3(
        modelTransform * _primaryRenderInformation._localTransform *
        glm::dvec4(0.0, 0.0, 0.0, 1.0)
    );
    const double distance = glm::distance(
        trailPosWorld,
        data.camera.eyePositionVec3()
    );

    if (distance > scaledRadius * DISTANCE_CULLING_RADII) {
        return;
    }

    // Render the primary batch of vertices
    render(_primaryRenderInformation, totalNumber, primaryOffset);

    // The secondary batch is optional, so we need to check whether we have any data here
    if (_floatingRenderInformation._vaoID != 0 && _floatingRenderInformation.count != 0) {
        render(
            _floatingRenderInformation,
            totalNumber,
            // -1 because we duplicate the penultimate point between the vertices
            -(primaryOffset + _primaryRenderInformation.count - 1)
        );
    }

    if (renderPoints) {
        glDisable(GL_PROGRAM_POINT_SIZE);
    }

    glBindVertexArray(0);

    if (usingFramebufferRenderer) {
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glDepthMask(true);
    }

    _programObject->deactivate();
}

} // namespace openspace
