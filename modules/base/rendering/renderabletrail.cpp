/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <openspace/documentation/verifier.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/translation.h>

namespace {
    static const char* KeyTranslation = "Translation";
    static const char* KeyColor = "Color";
    static const char* KeyEnableFade = "EnableFade";
    static const char* KeyFade = "Fade";
    static const char* KeyLineWidth = "LineWidth";
    static const char* KeyPointSize = "PointSize";
    static const char* KeyRendering = "Rendering";

    // The possible values for the _renderingModes property
    enum RenderingMode {
        RenderingModeLines = 0,
        RenderingModePoints,
        RenderingModeLinesPoints
    };

    // Fragile! Keep in sync with documentation
    static const std::map<std::string, RenderingMode> RenderingModeConversion = {
        { "Lines", RenderingModeLines },
        { "Points", RenderingModePoints },
        { "Lines+Points", RenderingModeLinesPoints },
        { "Points+Lines", RenderingModeLinesPoints }
    };
}

namespace openspace {

Documentation RenderableTrail::Documentation() {
using namespace documentation;
    return {
        "RenderableTrail",
        "base_renderable_renderabletrail",
        {
            {
                KeyTranslation,
                new ReferencingVerifier("core_transform_translation"),
                "This object is used to compute locations along the path. Any "
                "Translation object can be used here.",
                Optional::No
            },
            {
                KeyColor,
                new DoubleVector3Verifier,
                "The main color the for lines and points on this trail. The value is "
                "interpreted as an RGB value.",
                Optional::No
            },
            {
                KeyEnableFade,
                new BoolVerifier,
                "Toggles whether the trail should fade older points out. If this value "
                "is 'true', the 'Fade' parameter determines the speed of fading. If this "
                "value is 'false', the entire trail is rendered at full opacity and "
                "color. The default value is 'true'.",
                Optional::Yes
            },
            {
                KeyFade,
                new DoubleVerifier,
                "The fading factor that is applied to the trail if the 'EnableFade' "
                "value is 'true'. If it is 'false', this setting has no effect. The "
                "higher the number, the less fading is applied. This value defaults to "
                "1.0.",
                Optional::Yes
            },
            {
                KeyLineWidth,
                new DoubleVerifier,
                "This value specifies the line width of the trail if this rendering "
                "method is selected. It defaults to 2.0.",
                Optional::Yes
            },
            {
                KeyPointSize,
                new DoubleVerifier,
                "This value specifies the base size of the points along the line if this "
                "rendering method is selected. If a subsampling of the values is "
                "performed, the subsampled values are half this size. The default value "
                "is 1.0.",
                Optional::Yes
            },
            {
                KeyRendering,
                new StringInListVerifier(
                    // Taken from the RenderingModeConversion map above
                    { "Lines", "Points", "Lines+Points", "Points + Lines" }
                ),
                "Determines how the trail should be rendered to the screen. If 'Lines' "
                "is selected, only the line part is visible, if 'Points' is selected, "
                "only the corresponding points (and subpoints) are shown. "
                "Lines+Points' shows both parts. On default, only the lines are rendered",
                Optional::Yes
            }
        },
        Exhaustive::No
    };
}

RenderableTrail::RenderableTrail(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _lineColor("lineColor", "Color", glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _useLineFade("useLineFade", "Use Line Fade", true)
    , _lineFade("lineFade", "Line Fade", 1.f, 0.f, 20.f)
    , _lineWidth("lineWidth", "Line Width", 2.f, 1.f, 20.f)
    , _pointSize("pointSize", "Point Size", 1, 1, 64)
    , _renderingModes(
        "renderingMode",
        "Rendering Mode",
        properties::OptionProperty::DisplayType::Dropdown
    )
{
    _translation = std::unique_ptr<Translation>(Translation::createFromDictionary(
        dictionary.value<ghoul::Dictionary>(KeyTranslation)
    ));

    _lineColor = dictionary.value<glm::vec3>(KeyColor);
    addProperty(_lineColor);

    if (dictionary.hasKeyAndValue<bool>(KeyEnableFade)) {
        _useLineFade = dictionary.value<bool>(KeyEnableFade);
    }
    addProperty(_useLineFade);

    if (dictionary.hasKeyAndValue<double>(KeyFade)) {
        _lineFade = dictionary.value<double>(KeyFade);
    }
    addProperty(_lineFade);

    if (dictionary.hasKeyAndValue<double>(KeyLineWidth)) {
        _lineWidth = dictionary.value<double>(KeyLineWidth);
    }
    addProperty(_lineWidth);

    if (dictionary.hasKeyAndValue<double>(KeyPointSize)) {
        _pointSize = dictionary.value<double>(KeyPointSize);
    }
    addProperty(_pointSize);

    _renderingModes.addOptions({
        { RenderingModeLines, "Lines" },
        { RenderingModePoints, "Points" },
        { RenderingModeLinesPoints, "Lines+Points" }
    });

    // This map is not accessed out of order as long as the Documentation is adapted
    // whenever the map changes. The documentation will check for valid values
    if (dictionary.hasKeyAndValue<std::string>(KeyRendering)) {
        _renderingModes = RenderingModeConversion.at(
            dictionary.value<std::string>(KeyRendering)
        );
    }
    else {
        _renderingModes = RenderingModeLines;
    }
    addProperty(_renderingModes);
}

bool RenderableTrail::initialize() {
    RenderEngine& renderEngine = OsEng.renderEngine();
    _programObject = renderEngine.buildRenderProgram(
        "EphemerisProgram",
        "${MODULE_BASE}/shaders/renderabletrail_vs.glsl",
        "${MODULE_BASE}/shaders/renderabletrail_fs.glsl"
    );

    setRenderBin(Renderable::RenderBin::Overlay);

    return true;
}

bool RenderableTrail::deinitialize() {
    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_programObject) {
        renderEngine.removeRenderProgram(_programObject);
        _programObject = nullptr;
    }
    return true;
}

bool RenderableTrail::isReady() const {
    return _programObject != nullptr;
}

void RenderableTrail::render(const RenderData & data) {
    _programObject->activate();

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)));

    _programObject->setUniform("projectionTransform", data.camera.projectionMatrix());

    _programObject->setUniform("color", _lineColor);
    _programObject->setUniform("useLineFade", _useLineFade);
    if (_useLineFade) {
        _programObject->setUniform("lineFade", _lineFade);
    }

    static std::map<RenderInformation::VertexSorting, int> SortingMapping = {
        // Fragile! Keep in sync with shader
        { RenderInformation::VertexSorting::NewestFirst, 0 },
        { RenderInformation::VertexSorting::OldestFirst, 1 },
        { RenderInformation::VertexSorting::NoSorting, 2}
    };

    bool usingFramebufferRenderer =
        OsEng.renderEngine().rendererImplementation() ==
        RenderEngine::RendererImplementation::Framebuffer;
    
    if (usingFramebufferRenderer) {
        glDepthMask(false);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    }

    bool renderLines =
        _renderingModes == RenderingModeLines |
        _renderingModes == RenderingModeLinesPoints;

    bool renderPoints =
        _renderingModes == RenderingModePoints |
        _renderingModes == RenderingModeLinesPoints;

    if (renderLines) {
        glLineWidth(_lineWidth);
    }
    if (renderPoints) {
        glEnable(GL_PROGRAM_POINT_SIZE);
    }

    auto render = [renderLines, renderPoints, p = _programObject.get(), &data,
                   &modelTransform, pointSize = _pointSize.value()]
                  (RenderInformation& info, int nVertices, int offset)
    {
        // We pass in the model view transformation matrix as double in order to maintain
        // high precision for vertices; especially for the trails, a high vertex precision
        // is necessary as they are usually far away from their reference
        p->setUniform(
            "modelViewTransform",
            data.camera.combinedViewMatrix() * modelTransform * info._localTransform
        );

        // The vertex sorting method is used to tweak the fading along the trajectory
        p->setUniform(
            "vertexSortingMethod",
            SortingMapping[info.sorting]
        );

        // This value is subtracted from the vertex id in the case of a potential ring
        // buffer (as used in RenderableTrailOrbit) to keep the first vertex at its
        // brightest
        p->setUniform(
            "idOffset",
            offset
        );

        p->setUniform("nVertices", nVertices);

        if (renderPoints) {
            // The stride parameter determines the distance between larger points and
            // smaller ones
            p->setUniform("stride", info.stride);
            p->setUniform("pointSize", pointSize);
        }

        // Fragile! Keep in sync with fragment shader
        enum RenderPhase {
            RenderPhaseLines = 0,
            RenderPhasePoints
        };

        glBindVertexArray(info._vaoID);
        if (renderLines) {
            p->setUniform("renderPhase", RenderPhaseLines);
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
            p->setUniform("renderPhase", RenderPhasePoints);
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
    int totalNumber =
        _primaryRenderInformation.count + _floatingRenderInformation.count - 1;

    // The primary information might use an index buffer, so we might need to start at an
    // offset
    int primaryOffset =
        _primaryRenderInformation._iBufferID == 0 ? 0 : _primaryRenderInformation.first;

    // Render the primary batch of vertices
    render(_primaryRenderInformation, totalNumber, primaryOffset);

    // The secondary batch is optional,. so we need to check whether we have any data here
    if (_floatingRenderInformation._vaoID == 0 || _floatingRenderInformation.count == 0) {
        render(
            _floatingRenderInformation,
            totalNumber,
            // -1 because we duplicate the penultimate point between the vertices
            primaryOffset + _primaryRenderInformation.count - 1
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
