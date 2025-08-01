/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
        "Trail Fade",
        "Determines how fast the trail fades out. A smaller number shows less of the "
        "trail and a larger number shows more.",
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
        openspace::properties::Property::Visibility::AdvancedUser
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
            SBDB,
            // Minor Planet Center.
            MPC
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
    , pointSizeExponent(PointSizeExponentInfo, 1.0f, 0.f, 11.f)
    , enableMaxSize(EnableMaxSizeInfo, true)
    , maxSize(MaxSizeInfo, 5.f, 0.f, 45.f)
    , renderingModes(RenderingModeInfo)
    , trailFade(TrailFadeInfo, 20.f, 0.f, 30.f)
    , enableOutline(EnableOutlineInfo, true)
    , outlineColor(OutlineColorInfo, glm::vec3(0.f), glm::vec3(0.f), glm::vec3(1.f))
    , outlineWidth(OutlineWidthInfo, 0.2f, 0.f, 1.f)
{
    renderingModes.addOptions({
        { RenderingModeTrail, "Trails" },
        { RenderingModePoint, "Points" },
        { RenderingModePointTrail , "Points and Trails" }
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

    _path = p.path.string();
    _path.onChange([this]() { updateBuffers(); });
    addProperty(_path);
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
               absPath("${MODULE_SPACE}/shaders/keplertrails_vs.glsl"),
               absPath("${MODULE_SPACE}/shaders/keplertrails_fs.glsl")
           );
       }
   );

    // Program for point rendering
    _pointProgram = SpaceModule::ProgramObjectManager.request(
        "OrbitalKeplerPoints",
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                "OrbitalKeplerPoints",
                absPath("${MODULE_SPACE}/shaders/keplerpoints_vs.glsl"),
                absPath("${MODULE_SPACE}/shaders/keplerpoints_fs.glsl"),
                absPath("${MODULE_SPACE}/shaders/keplerpoints_gs.glsl")
            );
        }
    );

    ghoul::opengl::updateUniformLocations(*_trailProgram, _uniformTrailCache);
    ghoul::opengl::updateUniformLocations(*_pointProgram, _uniformPointCache);

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
        calculateSegmentsForPoints(data);

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
            static_cast<glm::vec3>(data.camera.lookUpVectorWorldSpace())
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
            _startIndexPoints.data(),
            _segmentSizePoints.data(),
            static_cast<GLsizei>(_startIndexPoints.size())
        );
        glBindVertexArray(0);

        _pointProgram->deactivate();
    }

    if (renderTrails) {
        calculateSegmentsForTrails(data);
       
        _trailProgram->activate();
        _trailProgram->setUniform(_uniformTrailCache.opacity, opacity());
        _trailProgram->setUniform(_uniformTrailCache.color, _appearance.color);
        _trailProgram->setUniform(
            _uniformTrailCache.inGameTime,
            data.time.j2000Seconds()
        );
        _trailProgram->setUniform(
            _uniformTrailCache.modelViewTransform,
            calcModelViewTransform(data)
        );
        _trailProgram->setUniform(
            _uniformTrailCache.projectionTransform,
            data.camera.projectionMatrix()
        );

        // Because we want the property to work similar to the planet trails
        const float fade = pow(
            _appearance.trailFade.maxValue() - _appearance.trailFade, 2.f
        );
        _trailProgram->setUniform(_uniformTrailCache.trailFadeExponent, fade);

        // 0.05 is the "alpha value" for which the trail should no longer be rendered.
        // The value that's compared to 0.05 is calculated in the shader and depends
        // on the distance from the head of the trail to the part that's being rendered.
        // Value is passed as uniform due to it being used in both geometry and fragment
        // shader.
        _trailProgram->setUniform(_uniformTrailCache.colorFadeCutoffValue, 0.05f);

        glLineWidth(_appearance.trailWidth);

        glBindVertexArray(_vertexArray);
        glMultiDrawArrays(
            GL_LINE_STRIP,
            _startIndexTrails.data(),
            _segmentSizeTrails.data(),
            _lineDrawCount
        );
        glBindVertexArray(0);

        _trailProgram->deactivate();
    }
}

void RenderableOrbitalKepler::updateBuffers() {
    _parameters = kepler::readFile(_path.value(), _format);

    _numObjects = _parameters.size();

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
        if (_startRenderIdx >= _parameters.size() ||
            (_startRenderIdx + _sizeRender) >= _parameters.size())
        {
            throw ghoul::RuntimeError(std::format(
                "Tried to load {} objects but only {} are available",
                _startRenderIdx + _sizeRender, _parameters.size()
            ));
        }

        // Extract subset that starts at _startRenderIdx and contains _sizeRender obejcts
        _parameters = std::vector<kepler::Parameters>(
            _parameters.begin() + _startRenderIdx,
            _parameters.begin() + _startRenderIdx + _sizeRender
        );
    }
    else {
        // First shuffle the whole array
        std::default_random_engine rng;
        std::shuffle(_parameters.begin(), _parameters.end(), rng);

        // Then take the first _sizeRender values
        _parameters = std::vector<kepler::Parameters>(
            _parameters.begin(),
            _parameters.begin() + _sizeRender
        );
    }

    _startIndexPoints.clear();
    _segmentSizePoints.clear();
    _startIndexTrails.clear();
    _segmentSizeTrails.clear();
    _segmentSizeRaw.clear();
    size_t nVerticesTotal = 0;
    const int numOrbits = static_cast<int>(_parameters.size());
    for (int i = 0; i < numOrbits; i++) {
        // For points rendering as they are always two vertices long
        _segmentSizePoints.push_back(2);

        const double scale = static_cast<double>(_segmentQuality) * 10.0;
        const kepler::Parameters& p = _parameters[i];
        _segmentSizeRaw.push_back(
            static_cast<int>(scale + (scale / std::pow(1.0 - p.eccentricity, 1.2)))
        );
        nVerticesTotal += _segmentSizeRaw[i];
    }
    _startIndexPoints.resize(numOrbits);
    _startIndexTrails.resize(numOrbits*2);
    _segmentSizeTrails.resize(numOrbits*2);
    _vertexBufferData.resize(nVerticesTotal);

    size_t vertexBufIdx = 0;
    for (int orbitIdx = 0; orbitIdx < numOrbits; ++orbitIdx) {
        const kepler::Parameters& orbit = _parameters[orbitIdx];

        ghoul::Dictionary d;
        d.setValue("Type", std::string("KeplerTranslation"));
        d.setValue("Eccentricity", orbit.eccentricity);
        d.setValue("SemiMajorAxis", orbit.semiMajorAxis);
        d.setValue("Inclination", orbit.inclination);
        d.setValue("AscendingNode", orbit.ascendingNode);
        d.setValue("ArgumentOfPeriapsis", orbit.argumentOfPeriapsis);
        d.setValue("MeanAnomaly", orbit.meanAnomaly);
        d.setValue("Period", orbit.period);
        d.setValue("Epoch", orbit.epoch);
        KeplerTranslation keplerTranslator = KeplerTranslation(d);

        const int nSegments = _segmentSizeRaw[orbitIdx];
        for (GLint j = 0 ; j < nSegments; j++) {
            const double timeOffset = orbit.period *
                static_cast<double>(j) / static_cast<double>(nSegments - 1);

            const glm::dvec3 position = keplerTranslator.position({
                {},
                Time(timeOffset + orbit.epoch),
                Time(0.0)
            });

            _vertexBufferData[vertexBufIdx + j].x = static_cast<float>(position.x);
            _vertexBufferData[vertexBufIdx + j].y = static_cast<float>(position.y);
            _vertexBufferData[vertexBufIdx + j].z = static_cast<float>(position.z);
            _vertexBufferData[vertexBufIdx + j].time = static_cast<float>(timeOffset);
            _vertexBufferData[vertexBufIdx + j].epoch = orbit.epoch;
            _vertexBufferData[vertexBufIdx + j].period = orbit.period;
        }
        vertexBufIdx += nSegments;
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
    for (const kepler::Parameters& kp : _parameters) {
        if (kp.semiMajorAxis > maxSemiMajorAxis) {
            maxSemiMajorAxis = kp.semiMajorAxis;
        }
    }
    setBoundingSphere(maxSemiMajorAxis * 1000);
}

void RenderableOrbitalKepler::calculateSegmentsForPoints(const RenderData& data) {
    int startVertexIndex = 0;
    for (int i = 0; i < _segmentSizeRaw.size(); i++) {
        // Check how far along the trail we are
        const kepler::Parameters& orbit = _parameters[i];
        const double nRevs = (data.time.j2000Seconds() - orbit.epoch) / orbit.period;
        double frac = nRevs - std::trunc(nRevs);
        frac += (frac < 0.0) ? 1.0: 0.0;

        // Get the closest vertex before that point
        const int nSegments = _segmentSizeRaw[i] - 1;
        const int offset = static_cast<int>(std::floor(frac * nSegments));

        // Set start vertex ID in buffer
        _startIndexPoints[i] = startVertexIndex + offset;
        startVertexIndex += _segmentSizeRaw[i];
    }
}

void RenderableOrbitalKepler::calculateSegmentsForTrails(const RenderData& data) {
    const float fade = pow(_appearance.trailFade.maxValue() - _appearance.trailFade, 2.f);
    const float threshold = 1.f - pow(0.05f, 1.f / fade);

    int nTotalTrailParts = 0;
    int startVertexIndex = 0;
    for (int i = 0; i < _segmentSizeRaw.size(); i++) {
        // Check how far along the trail we are
        const kepler::Parameters& orbit = _parameters[i];
        const double nRevs = (data.time.j2000Seconds() - orbit.epoch) / orbit.period;
        double frac = nRevs - std::trunc(nRevs);
        frac += (frac < 0.0) ? 1.0 : 0.0;

        int p0Start = 0;
        int p0Length = 0;
        int p1Start = 0;
        int p1Length = 0;

        const int nVerts = _segmentSizeRaw[i];
        const int nSegments = nVerts - 1;
        const int trailLength = static_cast<int>(std::ceil(threshold * nSegments));
        if (trailLength == nSegments) {
            // Whole trail should be visible
            p0Start = startVertexIndex;
            p0Length = nVerts;
        }
        else {
            const int headOffset = static_cast<int>(std::ceil(frac * nSegments));
            const int headVertexIndex = startVertexIndex + headOffset;
            const int correctTrailLength = trailLength + 2;

            int correctVertexIndex = headVertexIndex - correctTrailLength + 1;

            // If the start of the trail should be at the end of the orbit
            if (correctVertexIndex < startVertexIndex) {
                correctVertexIndex += nVerts;
            }

            // If the trail is length passes over the last point of the orbit
            const int lastVertexIndex = startVertexIndex + nVerts;
            if (correctVertexIndex + correctTrailLength > lastVertexIndex) {
                p1Start = correctVertexIndex - 1;
                p1Length = lastVertexIndex - correctVertexIndex + 1;
                p0Start = startVertexIndex;
                p0Length = correctTrailLength - p1Length + 1;
            }
            else {
                // If the entire trail is within the bounds of the orbit
                p0Start = correctVertexIndex;
                p0Length = correctTrailLength;
            }
        }

        int newTrailParts = 0;
        if (p0Length > 1) {
            _startIndexTrails[nTotalTrailParts] = p0Start;
            _segmentSizeTrails[nTotalTrailParts] = p0Length;
            newTrailParts += 1;
        }

        if (p1Length > 1) {
            _startIndexTrails[nTotalTrailParts + newTrailParts] = p1Start;
            _segmentSizeTrails[nTotalTrailParts + newTrailParts] = p1Length;
            newTrailParts += 1;
        }

        startVertexIndex += nVerts;
        nTotalTrailParts += newTrailParts;
    }
    _lineDrawCount = static_cast<GLsizei>(nTotalTrailParts);
}

} // namespace openspace
