/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/space/rendering/renderableorbitalkepler.h>

#include <modules/space/translation/keplertranslation.h>
#include <modules/space/spacemodule.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/engine/globals.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/time.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/misc/csvreader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/logging/logmanager.h>
#include <chrono>
#include <cmath>
#include <fstream>
#include <random>
#include <vector>

namespace {

    // The possible values for the _renderingModes property
    enum RenderingMode {
        RenderingModeTrail = 0,
        RenderingModePoint,
        RenderingModePointTrail
    };

    constexpr openspace::properties::Property::PropertyInfo PathInfo = {
        "Path",
        "Path",
        "The file path to the data file to read.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SegmentQualityInfo = {
        "SegmentQuality",
        "Segment Quality",
        "A segment quality value for the orbital trail. A value from 1 (lowest) to "
        "10 (highest) that controls the number of line segments in the rendering of the "
        "orbital trail. This does not control the direct number of segments because "
        "these automatically increase according to the eccentricity of the orbit.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo TrailWidthInfo = {
        "TrailWidth",
        "Trail Width",
        "The line width used for the trail, if the selected rendering method includes "
        "lines. If the rendering mode is set to Points, this value is ignored.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo PointSizeExponentInfo = {
        "PointSizeExponent",
        "Point Size Exponent",
        "An exponential scale value to set the absolute size of the point.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo EnableMaxSizeInfo = {
        "EnableMaxSize",
        "Enable Max Size",
        "If true, the Max Size property will be used as an upper limit for the size of "
        "the point. This reduces the size of the points when approaching them, so that "
        "they stick to a maximum visual size depending on the Max Size value.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MaxSizeInfo = {
        "MaxSize",
        "Max Size",
        "Controls the maximum allowed size for the points, when the max size control "
        "feature is enabled. This limits the visual size of the points based on the "
        "distance to the camera. The larger the value, the larger the points are allowed "
        "to be. In the background, the computations are made to limit the size of the "
        "angle between the CameraToPointMid and CameraToPointEdge vectors.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RenderingModeInfo = {
        "Rendering",
        "Rendering Mode",
        "Determines how the trail should be rendered. If 'Trail' is selected, "
        "only the line part is visible, if 'Point' is selected, only the "
        "current satellite/debris point is visible.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "The RGB main color for the trails and points.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo TrailFadeInfo = {
        "TrailFade",
        "Trail Fade Factor",
        "Determines how fast the trail fades out.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo EnableOutlineInfo = {
        "EnableOutline",
        "Enable Point Outline",
        "Determines if the points should be rendered with an outline or not.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo OutlineColorInfo = {
        "OutlineColor",
        "Outline Color",
        "The color of the outline.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo OutlineWidthInfo = {
        "OutlineWidth",
        "Outline Width",
        "Determines the thickness of the outline. A value of 0 will not show any "
        "outline, while a value of 1 will cover the whole point.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo StartRenderIdxInfo = {
        "StartRenderIdx",
        "Contiguous Starting Index of Render",
        "Index of the first object in the block to render (all prior objects will be "
        "ignored). The block of objects to render will be determined by StartRenderIdx "
        "and RenderSize.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RenderSizeInfo = {
        "RenderSize",
        "Contiguous Size of Render Block",
        "Number of objects to render sequentially from StartRenderIdx.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ContiguousModeInfo = {
        "ContiguousMode",
        "Contiguous Mode",
        "If enabled, the contiguous set of objects starting from StartRenderIdx "
        "of size RenderSize will be rendered. If disabled, the number of objects "
        "defined by UpperLimit will rendered from an evenly dispersed sample of the "
        "full length of the data file.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(RenderableOrbitalKepler)]] Parameters {
        // [[codegen::verbatim(PathInfo.description)]]
        std::filesystem::path path;

        enum class [[codegen::map(openspace::kepler::Format)]] Format {
            // A NORAD-style Two-Line element.
            TLE,
            // Orbit Mean-Elements Message in the KVN notation.
            OMM,
            // JPL's Small Bodies Database.
            SBDB
        };
        // The file format that is contained in the file.
        Format format;

        // [[codegen::verbatim(SegmentQualityInfo.description)]]
        int segmentQuality;

        // [[codegen::verbatim(TrailWidthInfo.description)]]
        std::optional<float> trailWidth;

        // [[codegen::verbatim(ColorInfo.description)]]
        glm::dvec3 color [[codegen::color()]];

        // [[codegen::verbatim(TrailFadeInfo.description)]]
        std::optional<float> trailFade;

        enum class RenderingMode {
            Trail,
            Point,
            PointsTrails
        };
        // [[codegen::verbatim(RenderingModeInfo.description)]]
        std::optional<RenderingMode> renderingMode [[codegen::key("Rendering")]];

        // [[codegen::verbatim(StartRenderIdxInfo.description)]]
        std::optional<int> startRenderIdx;

        // [[codegen::verbatim(RenderSizeInfo.description)]]
        std::optional<int> renderSize;

        // [[codegen::verbatim(ContiguousModeInfo.description)]]
        std::optional<bool> contiguousMode;

        // [[codegen::verbatim(PointSizeExponentInfo.description)]]
        std::optional<float> pointSizeExponent;

        // [[codegen::verbatim(EnableMaxSizeInfo.description)]]
        std::optional<bool> enableMaxSize;

        // [[codegen::verbatim(MaxSizeInfo.description)]]
        std::optional<float> maxSize;

        // [[codegen::verbatim(EnableOutlineInfo.description)]]
        std::optional<bool> enableOutline;

        // [[codegen::verbatim(OutlineColorInfo.description)]]
        std::optional<glm::vec3> outlineColor;

        // [[codegen::verbatim(OutlineWidthInfo.description)]]
        std::optional<float> outlineWidth;
    };
#include "renderableorbitalkepler_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableOrbitalKepler::Documentation() {
    return codegen::doc<Parameters>("space_renderableorbitalkepler");
}

RenderableOrbitalKepler::Appearance::Appearance()
    : properties::PropertyOwner({
        "Appearance",
        "Appearance",
        "Appearance of RenderableOrbitalKepler"
    })
    , color(ColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , trailWidth(TrailWidthInfo, 2.f, 1.f, 20.f)
    , pointSizeExponent(PointSizeExponentInfo, 1.0f, 0.f, 25.f)
    , renderingModes(
        RenderingModeInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , trailFade(TrailFadeInfo, 20.f, 0.f, 30.f)
    , enableMaxSize(EnableMaxSizeInfo, false)
    , maxSize(MaxSizeInfo, 1.f, 0.f, 45.f)
    , enableOutline(EnableOutlineInfo, true)
    , outlineColor(OutlineColorInfo, glm::vec3(0.f), glm::vec3(0.f), glm::vec3(1.f))
    , outlineWidth(OutlineWidthInfo, 0.2f, 0.f, 1.f)
{
    renderingModes.addOptions({
        { RenderingModeTrail, "Trails" },
        { RenderingModePoint, "Points" },
        { RenderingModePointTrail , "Points+Trails" }
    });
    addProperty(renderingModes);

    color.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(color);
    addProperty(trailWidth);
    addProperty(trailFade);
    addProperty(pointSizeExponent);
    addProperty(enableMaxSize);
    addProperty(maxSize);
    addProperty(enableOutline);
    outlineColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(outlineColor);
    addProperty(outlineWidth);
}

RenderableOrbitalKepler::RenderableOrbitalKepler(const ghoul::Dictionary& dict)
    : Renderable(dict)
    , _segmentQuality(SegmentQualityInfo, 2, 1, 10)
    , _startRenderIdx(StartRenderIdxInfo, 0, 0, 1)
    , _sizeRender(RenderSizeInfo, 1, 1, 2)
    , _path(PathInfo)
    , _contiguousMode(ContiguousModeInfo, false)
{
    const Parameters p = codegen::bake<Parameters>(dict);

    addProperty(Fadeable::_opacity);

    _segmentQuality = static_cast<unsigned int>(p.segmentQuality);
    _segmentQuality.onChange([this]() { updateBuffers(); });
    addProperty(_segmentQuality);

    _appearance.color = p.color;
    _appearance.trailFade = p.trailFade.value_or(_appearance.trailFade);
    _appearance.trailWidth = p.trailWidth.value_or(_appearance.trailWidth);
    _appearance.enableMaxSize = p.enableMaxSize.value_or(_appearance.enableMaxSize);
    _appearance.maxSize = p.maxSize.value_or(_appearance.maxSize);
    _appearance.enableOutline = p.enableOutline.value_or(_appearance.enableOutline);
    _appearance.outlineColor = p.outlineColor.value_or(_appearance.outlineColor);
    _appearance.outlineWidth = p.outlineWidth.value_or(_appearance.outlineWidth);
    _appearance.pointSizeExponent =
        p.pointSizeExponent.value_or(_appearance.pointSizeExponent);

    if (p.renderingMode.has_value()) {
        switch (*p.renderingMode) {
            case Parameters::RenderingMode::Trail:
                _appearance.renderingModes = RenderingModeTrail;
                break;
            case Parameters::RenderingMode::Point:
                _appearance.renderingModes = RenderingModePoint;
                break;
            case Parameters::RenderingMode::PointsTrails:
                _appearance.renderingModes = RenderingModePointTrail;
                break;
        }
    }
    else {
        _appearance.renderingModes = RenderingModeTrail;
    }
    addPropertySubOwner(_appearance);

    _path = p.path.string();
    _path.onChange([this]() { updateBuffers(); });
    addProperty(_path);

    _format = codegen::map<kepler::Format>(p.format);

    _startRenderIdx = p.startRenderIdx.value_or(0);
    _startRenderIdx.onChange([this]() {
        if (_contiguousMode) {
            if ((_numObjects - _startRenderIdx) < _sizeRender) {
                _sizeRender = static_cast<unsigned int>(_numObjects - _startRenderIdx);
            }
            _updateDataBuffersAtNextRender = true;
        }
    });
    addProperty(_startRenderIdx);

    _sizeRender = p.renderSize.value_or(0u);
    _sizeRender.onChange([this]() {
        if (_contiguousMode) {
            if (_sizeRender > (_numObjects - _startRenderIdx)) {
                _startRenderIdx = static_cast<unsigned int>(_numObjects - _sizeRender);
            }
        }
        _updateDataBuffersAtNextRender = true;
    });
    addProperty(_sizeRender);

    _contiguousMode = p.contiguousMode.value_or(false);
    _contiguousMode.onChange([this]() { _updateDataBuffersAtNextRender = true; });
    addProperty(_contiguousMode);
}

void RenderableOrbitalKepler::initializeGL() {
    ghoul_assert(_vertexArray == 0, "Vertex array object already existed");
    ghoul_assert(_vertexBuffer == 0, "Vertex buffer object already existed");
    glGenVertexArrays(1, &_vertexArray);
    glGenBuffers(1, &_vertexBuffer);

    // Program for line rendering
    _trailProgram = SpaceModule::ProgramObjectManager.request(
        "OrbitalKeplerTrails",
       []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
           return global::renderEngine->buildRenderProgram(
               "OrbitalKeplerTrails",
               absPath("${MODULE_SPACE}/shaders/debrisVizTrails_vs.glsl"),
               absPath("${MODULE_SPACE}/shaders/debrisVizTrails_fs.glsl")
           );
       }
   );

    // Program for point rendering
    _pointProgram = SpaceModule::ProgramObjectManager.request(
        "OrbitalKeplerPoints",
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                "OrbitalKeplerPoints",
                absPath("${MODULE_SPACE}/shaders/debrisVizPoints_vs.glsl"),
                absPath("${MODULE_SPACE}/shaders/debrisVizPoints_fs.glsl"),
                absPath("${MODULE_SPACE}/shaders/debrisVizPoints_gs.glsl")
            );
        }
    );

    // Init cache for line rendering
    _uniformTrailCache.modelView =
        _trailProgram->uniformLocation("modelViewTransform");
    _uniformTrailCache.projection =
        _trailProgram->uniformLocation("projectionTransform");
    _uniformTrailCache.trailFade = _trailProgram->uniformLocation("trailFade");
    _uniformTrailCache.inGameTime = _trailProgram->uniformLocation("inGameTime");
    _uniformTrailCache.color = _trailProgram->uniformLocation("color");
    _uniformTrailCache.opacity = _trailProgram->uniformLocation("opacity");

    // Init cache for point rendering
    _uniformPointCache.modelTransform = _pointProgram->uniformLocation("modelTransform");
    _uniformPointCache.viewTransform = _pointProgram->uniformLocation("viewTransform");
    _uniformPointCache.cameraUpWorld = _pointProgram->uniformLocation("cameraUpWorld");
    _uniformPointCache.inGameTime = _pointProgram->uniformLocation("inGameTime");
    _uniformPointCache.color = _pointProgram->uniformLocation("color");
    _uniformPointCache.enableMaxSize = _pointProgram->uniformLocation("enableMaxSize");
    _uniformPointCache.maxSize = _pointProgram->uniformLocation("maxSize");
    _uniformPointCache.enableOutline = _pointProgram->uniformLocation("enableOutline");
    _uniformPointCache.outlineColor = _pointProgram->uniformLocation("outlineColor");
    _uniformPointCache.outlineWeight = _pointProgram->uniformLocation("outlineWeight");
    _uniformPointCache.opacity = _pointProgram->uniformLocation("opacity");
    _uniformPointCache.projectionTransform =
        _pointProgram->uniformLocation("projectionTransform");
    _uniformPointCache.cameraPositionWorld =
        _pointProgram->uniformLocation("cameraPositionWorld");
    _uniformPointCache.pointSizeExponent =
        _pointProgram->uniformLocation("pointSizeExponent");

    updateBuffers();
}

void RenderableOrbitalKepler::deinitializeGL() {
    glDeleteBuffers(1, &_vertexBuffer);
    glDeleteVertexArrays(1, &_vertexArray);

    SpaceModule::ProgramObjectManager.release(
        "OrbitalKeplerTrails",
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );

    SpaceModule::ProgramObjectManager.release(
        "OrbitalKeplerPoints",
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );

    _pointProgram = nullptr;
    _trailProgram = nullptr;
}

bool RenderableOrbitalKepler::isReady() const {
    return _pointProgram != nullptr && _trailProgram != nullptr;
}

void RenderableOrbitalKepler::update(const UpdateData&) {
    if (_updateDataBuffersAtNextRender) {
        _updateDataBuffersAtNextRender = false;
        updateBuffers();
    }
}

void RenderableOrbitalKepler::render(const RenderData& data, RendererTasks&) {
    if (_vertexBufferData.empty()) {
        return;
    }

    GLint* _si = _startIndex.data();
    GLint* _ss = _segmentSize.data();

    const int selection = _appearance.renderingModes;
    const bool renderPoints = (
        selection == RenderingModePoint ||
        selection == RenderingModePointTrail
    );
    const bool renderTrails = (
        selection == RenderingModeTrail ||
        selection == RenderingModePointTrail
    );

    if (renderPoints) {
        _pointProgram->activate();
        _pointProgram->setUniform(
            _uniformPointCache.modelTransform,
            calcModelTransform(data)
        );
        _pointProgram->setUniform(
            _uniformPointCache.viewTransform,
            data.camera.combinedViewMatrix()
        );
        _pointProgram->setUniform(
            _uniformPointCache.projectionTransform,
            data.camera.projectionMatrix()
        );
        _pointProgram->setUniform(
            _uniformPointCache.cameraPositionWorld,
            data.camera.positionVec3()
        );
        _pointProgram->setUniform(
            _uniformPointCache.cameraUpWorld,
            data.camera.lookUpVectorWorldSpace()
        );
        _pointProgram->setUniform(
            _uniformPointCache.inGameTime,
            data.time.j2000Seconds()
        );
        _pointProgram->setUniform(
            _uniformPointCache.pointSizeExponent,
            _appearance.pointSizeExponent
        );
        _pointProgram->setUniform(
            _uniformPointCache.enableMaxSize,
            _appearance.enableMaxSize
        );
        _pointProgram->setUniform(
            _uniformPointCache.enableOutline,
            _appearance.enableOutline
        );
        _pointProgram->setUniform(
            _uniformPointCache.outlineColor,
            _appearance.outlineColor
        );
        _pointProgram->setUniform(
            _uniformPointCache.outlineWeight,
            _appearance.outlineWidth
        );
        _pointProgram->setUniform(_uniformPointCache.color, _appearance.color);
        _pointProgram->setUniform(_uniformPointCache.maxSize, _appearance.maxSize);
        _pointProgram->setUniform(_uniformPointCache.opacity, opacity());

        glBindVertexArray(_vertexArray);
        glMultiDrawArrays(
            GL_LINE_STRIP,
            _si,
            _ss,
            static_cast<GLsizei>(_startIndex.size())
        );
        glBindVertexArray(0);

        _pointProgram->deactivate();
    }

    if (renderTrails) {
        _trailProgram->activate();
        _trailProgram->setUniform(_uniformTrailCache.opacity, opacity());
        _trailProgram->setUniform(_uniformTrailCache.color, _appearance.color);
        _trailProgram->setUniform(
            _uniformTrailCache.inGameTime,
            data.time.j2000Seconds()
        );
        _trailProgram->setUniform(
            _uniformTrailCache.modelView,
            calcModelViewTransform(data)
        );
        _trailProgram->setUniform(
            _uniformTrailCache.projection,
            data.camera.projectionMatrix()
        );

        // Because we want the property to work similar to the planet trails
        const float fade = pow(
            _appearance.trailFade.maxValue() - _appearance.trailFade, 2.f
        );
        _trailProgram->setUniform(_uniformTrailCache.trailFade, fade);

        glLineWidth(_appearance.trailWidth);

        glBindVertexArray(_vertexArray);
        glMultiDrawArrays(
            GL_LINE_STRIP,
            _si,
            _ss,
            static_cast<GLsizei>(_startIndex.size())
        );
        glBindVertexArray(0);

        _trailProgram->deactivate();
    }
}

void RenderableOrbitalKepler::updateBuffers() {
    std::vector<kepler::Parameters> parameters = kepler::readFile(
       _path.value(),
        _format
    );

    _numObjects = parameters.size();

    if (_startRenderIdx >= _numObjects) {
        throw ghoul::RuntimeError(std::format(
            "Start index {} out of range [0, {}]", _startRenderIdx.value(), _numObjects
        ));
    }

    long long endElement = _startRenderIdx + _sizeRender - 1;
    endElement = (endElement >= _numObjects) ? _numObjects - 1 : endElement;
    if (endElement < 0 || endElement >= _numObjects) {
        throw ghoul::RuntimeError(std::format(
            "End index {} out of range [0, {}]", endElement, _numObjects
        ));
    }

    _startRenderIdx.setMaxValue(static_cast<unsigned int>(_numObjects - 1));
    _sizeRender.setMaxValue(static_cast<unsigned int>(_numObjects));
    if (_sizeRender == 0u) {
        _sizeRender = static_cast<unsigned int>(_numObjects);
    }

    if (_contiguousMode) {
        if (_startRenderIdx >= parameters.size() ||
            (_startRenderIdx + _sizeRender) >= parameters.size())
        {
            throw ghoul::RuntimeError(std::format(
                "Tried to load {} objects but only {} are available",
                _startRenderIdx + _sizeRender, parameters.size()
            ));
        }

        // Extract subset that starts at _startRenderIdx and contains _sizeRender obejcts
        parameters = std::vector<kepler::Parameters>(
            parameters.begin() + _startRenderIdx,
            parameters.begin() + _startRenderIdx + _sizeRender
        );
    }
    else {
        // First shuffle the whole array
        std::default_random_engine rng;
        std::shuffle(parameters.begin(), parameters.end(), rng);

        // Then take the first _sizeRender values
        parameters = std::vector<kepler::Parameters>(
            parameters.begin(),
            parameters.begin() + _sizeRender
        );
    }

    _segmentSize.clear();
    _startIndex.clear();
    _startIndex.push_back(0);
    for (size_t i = 0; i < parameters.size(); i++) {
        const double scale = static_cast<double>(_segmentQuality) * 10.0;
        const kepler::Parameters& p = parameters[i];
        _segmentSize.push_back(
            static_cast<int>(scale + (scale / pow(1.0 - p.eccentricity, 1.2)))
        );
        _startIndex.push_back(_startIndex[i] + static_cast<GLint>(_segmentSize[i]));
    }
    _startIndex.pop_back();

    size_t nVerticesTotal = 0;

    const int numOrbits = static_cast<int>(parameters.size());
    for (int i = 0; i < numOrbits; i++) {
        nVerticesTotal += _segmentSize[i];
    }
    _vertexBufferData.resize(nVerticesTotal);

    size_t vertexBufIdx = 0;
    KeplerTranslation keplerTranslator;
    for (int orbitIdx = 0; orbitIdx < numOrbits; ++orbitIdx) {
        const kepler::Parameters& orbit = parameters[orbitIdx];

        keplerTranslator.setKeplerElements(
            orbit.eccentricity,
            orbit.semiMajorAxis,
            orbit.inclination,
            orbit.ascendingNode,
            orbit.argumentOfPeriapsis,
            orbit.meanAnomaly,
            orbit.period,
            orbit.epoch
        );

        for (GLint j = 0 ; j < (_segmentSize[orbitIdx]); j++) {
            const double timeOffset = orbit.period *
                static_cast<double>(j) / static_cast<double>(_segmentSize[orbitIdx] - 1);

            const glm::dvec3 position = keplerTranslator.position({
                {},
                Time(timeOffset + orbit.epoch),
                Time(0.0)
            });

            _vertexBufferData[vertexBufIdx].x = static_cast<float>(position.x);
            _vertexBufferData[vertexBufIdx].y = static_cast<float>(position.y);
            _vertexBufferData[vertexBufIdx].z = static_cast<float>(position.z);
            _vertexBufferData[vertexBufIdx].time = static_cast<float>(timeOffset);
            _vertexBufferData[vertexBufIdx].epoch = orbit.epoch;
            _vertexBufferData[vertexBufIdx].period = orbit.period;

            vertexBufIdx++;
        }
    }

    glBindVertexArray(_vertexArray);

    glBindBuffer(GL_ARRAY_BUFFER, _vertexBuffer);
    glBufferData(
        GL_ARRAY_BUFFER,
        _vertexBufferData.size() * sizeof(TrailVBOLayout),
        _vertexBufferData.data(),
        GL_STATIC_DRAW
    );

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(TrailVBOLayout), nullptr);

    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        2,
        GL_DOUBLE,
        GL_FALSE,
        sizeof(TrailVBOLayout),
        reinterpret_cast<GLvoid*>(4 * sizeof(GL_FLOAT))
    );

    glBindVertexArray(0);

    double maxSemiMajorAxis = 0.0;
    for (const kepler::Parameters& kp : parameters) {
        if (kp.semiMajorAxis > maxSemiMajorAxis) {
            maxSemiMajorAxis = kp.semiMajorAxis;
        }
    }
    setBoundingSphere(maxSemiMajorAxis * 1000);
}

} // namespace openspace
