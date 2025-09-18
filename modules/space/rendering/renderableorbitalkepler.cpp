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
#include <execution>
#include <fstream>
#include <random>
#include <vector>

namespace {
    // The possible values for the _renderingModes property
    enum RenderMode {
        RenderingModeTrail = 0,
        RenderingModePoint,
        RenderingModePointTrail
    };

    enum PointRenderingMode {
        ViewDirection = 0,
        PositionNormal
    };

    constexpr openspace::properties::Property::PropertyInfo PathInfo = {
        "Path",
        "Path",
        "The file path to the data file to read.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo PointRenderingModeInfo =
    {
        "PointRenderingMode",
        "Point Rendering Mode",
        "Controls how the points will be oriented. \"Camera View "
        "Direction\" rotates the points so that they are orthogonal to the viewing "
        "direction of the camera (useful for planar displays), and \"Camera Position "
        "Normal\" rotates the points towards the position of the camera (useful for "
        "spherical displays, like dome theaters).",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SegmentQualityInfo = {
        "SegmentQuality",
        "Segment quality",
        "A segment quality value for the orbital trail. A value from 1 (lowest) to "
        "10 (highest) that controls the number of line segments in the rendering of the "
        "orbital trail. This does not control the direct number of segments because "
        "these automatically increase according to the eccentricity of the orbit.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo TrailWidthInfo = {
        "TrailWidth",
        "Trail width",
        "The line width used for the trail, if the selected rendering method includes "
        "lines. If the rendering mode is set to Points, this value is ignored.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo PointSizeExponentInfo = {
        "PointSizeExponent",
        "Point size exponent",
        "An exponential scale value to set the absolute size of the point.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo EnableMaxSizeInfo = {
        "EnableMaxSize",
        "Enable max size",
        "If true, the Max Size property will be used as an upper limit for the size of "
        "the point. This reduces the size of the points when approaching them, so that "
        "they stick to a maximum visual size depending on the Max Size value.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MaxSizeInfo = {
        "MaxSize",
        "Max size",
        "Controls the maximum allowed size for the points, when the max size control "
        "feature is enabled. This limits the visual size of the points based on the "
        "distance to the camera. The larger the value, the larger the points are allowed "
        "to be. In the background, the computations are made to limit the size of the "
        "angle between the CameraToPointMid and CameraToPointEdge vectors.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RenderingModeInfo = {
        "Rendering",
        "Rendering mode",
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
        "Trail fade",
        "Determines how fast the trail fades out. A smaller number shows less of the "
        "trail and a larger number shows more.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo EnableOutlineInfo = {
        "EnableOutline",
        "Enable point outline",
        "Determines if the points should be rendered with an outline or not.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo OutlineColorInfo = {
        "OutlineColor",
        "Outline color",
        "The color of the outline.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo OutlineWidthInfo = {
        "OutlineWidth",
        "Outline width",
        "Determines the thickness of the outline. A value of 0 will not show any "
        "outline, while a value of 1 will cover the whole point.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo StartRenderIdxInfo = {
        "StartRenderIdx",
        "Contiguous starting index of render",
        "Index of the first object in the block to render (all prior objects will be "
        "ignored). The block of objects to render will be determined by StartRenderIdx "
        "and RenderSize.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RenderSizeInfo = {
        "RenderSize",
        "Contiguous size of render block",
        "Number of objects to render sequentially from StartRenderIdx.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ContiguousModeInfo = {
        "ContiguousMode",
        "Contiguous mode",
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

        enum class [[codegen::map(PointRenderingMode)]] PointRenderingMode {
            ViewDirection [[codegen::key("Camera View Direction")]],
            PositionNormal [[codegen::key("Camera Position Normal")]]
        };
        // [[codegen::verbatim(PointRenderingModeInfo.description)]]
        std::optional<PointRenderingMode> pointRenderingMode;

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
    , pointRenderOption(PointRenderingModeInfo)
    , trailFade(TrailFadeInfo, 20.f, 0.f, 30.f)
    , enableOutline(EnableOutlineInfo, true)
    , outlineColor(OutlineColorInfo, glm::vec3(0.f), glm::vec3(0.f), glm::vec3(1.f))
    , outlineWidth(OutlineWidthInfo, 0.2f, 0.f, 1.f)
{
    renderingModes.addOptions({
        { RenderMode::RenderingModeTrail, "Trails" },
        { RenderMode::RenderingModePoint, "Points" },
        { RenderMode::RenderingModePointTrail , "Points and Trails" }
    });
    renderingModes.onChange([this]() { changedRenderType = true; });
    addProperty(renderingModes);
    color.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(color);
    addProperty(trailWidth);
    addProperty(trailFade);
    addProperty(pointRenderOption);
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
    , _nThreads(static_cast<int>(std::ceil(std::thread::hardware_concurrency() / 2.0)))
    , _segmentQuality(SegmentQualityInfo, 2, 1, 10)
    , _startRenderIdx(StartRenderIdxInfo, 0, 0, 1)
    , _sizeRender(RenderSizeInfo, 1, 1, 2)
    , _path(PathInfo)
    , _contiguousMode(ContiguousModeInfo, false)
{
    const Parameters p = codegen::bake<Parameters>(dict);

    addProperty(Fadeable::_opacity);

    _segmentQuality = static_cast<unsigned int>(p.segmentQuality);
    _segmentQuality.onChange([this]() { _updateDataBuffersAtNextRender = true; });
    addProperty(_segmentQuality);

    _appearance.color = p.color;
    _appearance.trailFade = p.trailFade.value_or(_appearance.trailFade);
    _appearance.trailFade.onChange([this]() { _forceUpdate = true; });
    _appearance.trailWidth = p.trailWidth.value_or(_appearance.trailWidth);
    _appearance.enableMaxSize = p.enableMaxSize.value_or(_appearance.enableMaxSize);
    _appearance.maxSize = p.maxSize.value_or(_appearance.maxSize);
    _appearance.enableOutline = p.enableOutline.value_or(_appearance.enableOutline);
    _appearance.outlineColor = p.outlineColor.value_or(_appearance.outlineColor);
    _appearance.outlineWidth = p.outlineWidth.value_or(_appearance.outlineWidth);
    _appearance.pointSizeExponent =
        p.pointSizeExponent.value_or(_appearance.pointSizeExponent);

    _appearance.pointRenderOption.addOption(
        PointRenderingMode::ViewDirection,
        "Camera View Direction"
    );
    _appearance.pointRenderOption.addOption(
        PointRenderingMode::PositionNormal,
        "Camera Position Normal"
    );
    if (p.pointRenderingMode.has_value()) {
        switch (*p.pointRenderingMode) {
        case Parameters::PointRenderingMode::ViewDirection:
            _appearance.pointRenderOption = PointRenderingMode::ViewDirection;
            break;
        case Parameters::PointRenderingMode::PositionNormal:
            _appearance.pointRenderOption = PointRenderingMode::PositionNormal;
            break;
        }
    }
    else {
        _appearance.pointRenderOption = PointRenderingMode::ViewDirection;
    }

    if (p.renderingMode.has_value()) {
        switch (*p.renderingMode) {
            case Parameters::RenderingMode::Trail:
                _appearance.renderingModes = RenderMode::RenderingModeTrail;
                break;
            case Parameters::RenderingMode::Point:
                _appearance.renderingModes = RenderMode::RenderingModePoint;
                break;
            case Parameters::RenderingMode::PointsTrails:
                _appearance.renderingModes = RenderMode::RenderingModePointTrail;
                break;
        }
    }
    else {
        _appearance.renderingModes = RenderMode::RenderingModeTrail;
    }
    addPropertySubOwner(_appearance);

    _format = codegen::map<kepler::Format>(p.format);

    _startRenderIdx = p.startRenderIdx.value_or(0);
    _startRenderIdx.onChange([this]() {
        if (_contiguousMode) {
            if ((_nOrbits - _startRenderIdx) < _sizeRender) {
                _sizeRender = static_cast<unsigned int>(_nOrbits - _startRenderIdx);
            }
            _updateDataBuffersAtNextRender = true;
        }
    });
    addProperty(_startRenderIdx);

    _sizeRender = p.renderSize.value_or(0u);
    _sizeRender.onChange([this]() {
        if (_contiguousMode) {
            if (_sizeRender > (_nOrbits - _startRenderIdx)) {
                _startRenderIdx = static_cast<unsigned int>(_nOrbits - _sizeRender);
            }
        }
        _updateDataBuffersAtNextRender = true;
    });
    addProperty(_sizeRender);

    _contiguousMode = p.contiguousMode.value_or(false);
    _contiguousMode.onChange([this]() { _updateDataBuffersAtNextRender = true; });
    addProperty(_contiguousMode);

    _path = p.path.string();
    _path.onChange([this]() { _updateDataBuffersAtNextRender = true; });
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

    _updateDataBuffersAtNextRender = true;
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

void RenderableOrbitalKepler::update(const UpdateData& data) {
    if (_updateDataBuffersAtNextRender) {
        updateBuffers();
    }

    if(_appearance.changedRenderType) {
        _forceUpdate = true;
        _appearance.changedRenderType = false;
    }

    std::for_each(
        std::execution::par_unseq,
        _threadIds.cbegin(),
        _threadIds.cend(),
        [&](int threadId) {
            threadedSegmentCalculations(threadId, data);
        }
    );

    _lineDrawCount = static_cast<GLsizei>(_segmentsPerOrbit.size() * 2);
    _forceUpdate = false;
}

void RenderableOrbitalKepler::render(const RenderData& data, RendererTasks&) {
    if (_vertexBufferData.empty()) {
        return;
    }

    if (_renderPoints) {
        glm::vec3 cameraViewDirectionWorld = -data.camera.viewDirectionWorldSpace();
        glm::vec3 cameraUpDirectionWorld = data.camera.lookUpVectorWorldSpace();
        glm::vec3 orthoRight = glm::normalize(
            glm::cross(cameraUpDirectionWorld, cameraViewDirectionWorld)
        );
        if (orthoRight == glm::vec3(0.f)) {
            glm::vec3 otherVector = glm::vec3(
                cameraUpDirectionWorld.y,
                cameraUpDirectionWorld.x,
                cameraUpDirectionWorld.z
            );
            orthoRight = glm::normalize(glm::cross(otherVector, cameraViewDirectionWorld));
        }
        glm::vec3 orthoUp = glm::normalize(glm::cross(cameraViewDirectionWorld, orthoRight));

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
            _uniformPointCache.renderOption,
            _appearance.pointRenderOption
        );
        _pointProgram->setUniform(
            _uniformPointCache.cameraViewDirectionUp,
            orthoUp
        );
        _pointProgram->setUniform(
            _uniformPointCache.cameraViewDirectionRight,
            orthoRight
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

    if (_renderTrails) {
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
        const float fade = std::pow(
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
    _nOrbits = static_cast<unsigned int>(_parameters.size());

    if (_startRenderIdx >= _nOrbits) {
        throw ghoul::RuntimeError(std::format(
            "Start index {} out of range [0, {}]", _startRenderIdx.value(), _nOrbits
        ));
    }

    long long endElement = _startRenderIdx + _sizeRender - 1;
    endElement = (endElement >= _nOrbits) ? _nOrbits - 1 : endElement;
    if (endElement < 0 || endElement >= _nOrbits) {
        throw ghoul::RuntimeError(std::format(
            "End index {} out of range [0, {}]", endElement, _nOrbits
        ));
    }

    _startRenderIdx.setMaxValue(static_cast<unsigned int>(_nOrbits - 1));
    _sizeRender.setMaxValue(static_cast<unsigned int>(_nOrbits));
    if (_sizeRender == 0u) {
        _sizeRender = static_cast<unsigned int>(_nOrbits);
    }

    if (_contiguousMode) {
        if (_startRenderIdx >= _parameters.size() ||
            (_startRenderIdx + _sizeRender) > _parameters.size())
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

    _threadIds.clear();
    _orbitsPerThread.clear();
    _updateHelper.clear();
    _startIndexPoints.clear();
    _segmentSizePoints.clear();
    _vertexBufferOffset.clear();
    _startIndexTrails.clear();
    _segmentSizeTrails.clear();
    _segmentsPerOrbit.clear();

    _updateHelper.resize(_sizeRender);
    _startIndexPoints.resize(_sizeRender);
    _segmentSizePoints.resize(_sizeRender);
    _vertexBufferOffset.resize(_sizeRender);
    _segmentsPerOrbit.resize(_sizeRender);

    // Trail vectors needs double length as it may use two trails per orbit
    _startIndexTrails.resize(_sizeRender * 2);
    _segmentSizeTrails.resize(_sizeRender * 2);
    
    double maxSemiMajorAxis = 0.0;
    size_t nVerticesTotal = 0; 
    for (unsigned int i = 0; i < _sizeRender; i++) {
        // For points rendering as they are always two vertices long
        _segmentSizePoints[i] = 2;

        const double scale = static_cast<double>(_segmentQuality) * 10.0;
        const kepler::Parameters& p = _parameters[i];
        _segmentsPerOrbit[i] = static_cast<int>(
            scale + (scale / std::pow(1.0 - p.eccentricity, 1.2))
        );
        _vertexBufferOffset[i] = static_cast<int>(nVerticesTotal);
        nVerticesTotal += _segmentsPerOrbit[i];

        // Find largest value for bounding sphere
        if (p.semiMajorAxis > maxSemiMajorAxis) {
            maxSemiMajorAxis = p.semiMajorAxis;
        }
    }
    setBoundingSphere(maxSemiMajorAxis * 1000);
    _vertexBufferData.resize(nVerticesTotal);

    std::vector<int> orbitIdHolder;
    orbitIdHolder.resize(_sizeRender);
    std::iota(orbitIdHolder.begin(), orbitIdHolder.end(), 0);

    std::for_each(
        std::execution::par_unseq,
        orbitIdHolder.begin(),
        orbitIdHolder.end(),
        [&](int& index) {
            const kepler::Parameters& orbit = _parameters[index];

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

            const int nVerts = _segmentsPerOrbit[index];
            const int offset = _vertexBufferOffset[index];
            const int nSegments = nVerts - 1;
            for (GLint j = 0; j < nVerts; j++) {
                const double timeOffset = orbit.period *
                    static_cast<double>(j) / static_cast<double>(nSegments);

                const glm::dvec3 position = keplerTranslator.position({
                    {},
                    Time(timeOffset + orbit.epoch),
                    Time(0.0)
                });

                _vertexBufferData[offset + j].x = static_cast<float>(position.x);
                _vertexBufferData[offset + j].y = static_cast<float>(position.y);
                _vertexBufferData[offset + j].z = static_cast<float>(position.z);
                _vertexBufferData[offset + j].time = timeOffset;
                _vertexBufferData[offset + j].epoch = orbit.epoch;
                _vertexBufferData[offset + j].period = orbit.period;
            }

            _updateHelper[index].timePerStep = orbit.period / nSegments;
        }
    );

    // Calculate how many orbits we calculate per thread
    // 1000 per thread (arbitrary) to not create threads that do little to no work
    unsigned int orbitsPerThread = std::max(
        1000,
        static_cast<int>(std::ceil(static_cast<double>(_sizeRender) / _nThreads))
    );

    // Vector that maps thread index to number of orbits to render
    int threadId = 0;
    unsigned int remainingOrbits = _sizeRender;
    while (remainingOrbits >= orbitsPerThread) {
        _threadIds.push_back(threadId);
        _orbitsPerThread.push_back(orbitsPerThread);
        remainingOrbits -= orbitsPerThread;
        threadId++;
    }
    if (remainingOrbits > 0) {
        _threadIds.push_back(threadId);
        _orbitsPerThread.push_back(remainingOrbits);
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
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(TrailVBOLayout), nullptr);

    glEnableVertexAttribArray(1);
    glVertexAttribLPointer(
        1,
        3,
        GL_DOUBLE,
        sizeof(TrailVBOLayout),
        reinterpret_cast<GLvoid*>(offsetof(TrailVBOLayout, TrailVBOLayout::time))
    );

    glBindVertexArray(0);

    _updateDataBuffersAtNextRender = false;
}

void RenderableOrbitalKepler::threadedSegmentCalculations(const int threadId,
                                                                const UpdateData& data)
{
    const int selection = _appearance.renderingModes;
    _renderPoints = (
        selection == RenderMode::RenderingModePoint ||
        selection == RenderMode::RenderingModePointTrail
    );
    _renderTrails = (
        selection == RenderMode::RenderingModeTrail ||
        selection == RenderMode::RenderingModePointTrail
    );

    const float fade = std::pow(
        _appearance.trailFade.maxValue() - _appearance.trailFade,
        2.f
    );
    const float threshold = 1.f - std::pow(0.05f, 1.f / fade);

    int offset = std::accumulate(_orbitsPerThread.begin(),
        _orbitsPerThread.begin() + threadId,
        0
    );
    const int cutoff = offset + _orbitsPerThread[threadId];

    const double now = data.time.j2000Seconds();
    int startVertexIndex = _vertexBufferOffset[offset];
    for (int i = offset; i < cutoff; i++) {
        updateInfo* helper = &_updateHelper[i];
        double upper = helper->timestamp + (helper->timePerStep);
        double lower = helper->timestamp - (helper->timePerStep);
        const bool shouldUpdate = (now >= upper || now <= lower);

        const int nVerts = _segmentsPerOrbit[i];
        if (shouldUpdate || _forceUpdate) {
            // Check how far along the trail we are
            const kepler::Parameters& orbit = _parameters[i];
            const double nRevs = (data.time.j2000Seconds() - orbit.epoch) / orbit.period;
            double frac = static_cast<double>(nRevs - std::trunc(nRevs));
            frac += (frac < 0.0) ? 1.0 : 0.0;

            const int nSegments = nVerts - 1;
            const int pointHead = static_cast<int>(std::floor(frac * nSegments));

            // We can always do this since it has no cost
            _startIndexPoints[i] = startVertexIndex + pointHead;

            // There is a lot of what seems to be "magic numbers" in this section.
            // They will most likely disappear when we change our method of determining
            // the trail fade amount is changed.
            if (_renderTrails) {

                // When rendering a trail we don't know if the trail will pass over
                // the starting point of the orbit or not. If the trail passes over the
                // starting point of the orbit, then we can't draw the entire trail as
                // line strip. Instead we need to divide the line strip into two parts,
                // where p0 and p1 denotes the respctive line strips (parts).
                int p0Start;
                int p0Length;
                int p1Start;
                int p1Length;

                const int trailLength =
                    static_cast<int>(std::ceil(threshold * nSegments));
                if (trailLength == nSegments) {
                    // Whole trail should be visible
                    p0Start = startVertexIndex;
                    p0Length = nVerts;
                    p1Start = 0;
                    p1Length = 0;
                }
                else {
                    const int trailHead = static_cast<int>(std::ceil(frac * nSegments));
                    const int headVertexIndex = startVertexIndex + trailHead + 1;
                    const int correctTrailLength = trailLength + 3; 

                    // Need to do this due to order of vertex data in the vertex buffer
                    int correctVertexIndex = headVertexIndex - correctTrailLength;

                    // If the start of the trail should be at the end of the orbit
                    if (correctVertexIndex < startVertexIndex) {
                        correctVertexIndex += nVerts;
                    }

                    // If the trail is length passes over the last point of the orbit
                    const int lastVertexIndex = startVertexIndex + nVerts;
                    if (correctVertexIndex + correctTrailLength > lastVertexIndex) {
                        p0Start = startVertexIndex;
                        p1Start = correctVertexIndex;
                        if (lastVertexIndex - correctVertexIndex == 1) {
                            p1Length = 0;
                            p0Length = correctTrailLength - 1;
                        }
                        else {
                            p1Length = lastVertexIndex - correctVertexIndex;
                            p0Length = correctTrailLength - p1Length;
                        }
                    }
                    else {
                        // If the entire trail is within the bounds of the orbit
                        p0Start = correctVertexIndex;
                        p0Length = correctTrailLength;
                        p1Start = 0;
                        p1Length = 0;
                    }
                }
                _startIndexTrails[i * 2] = p0Start;
                _segmentSizeTrails[i * 2] = p0Length;
                _startIndexTrails[i * 2 + 1] = p1Start;
                _segmentSizeTrails[i * 2 + 1] = p1Length;
            }

            _updateHelper[i].timestamp = orbit.epoch +
                (std::floor(frac * nSegments) * _updateHelper[i].timePerStep) +
                (std::floor(nRevs) * orbit.period);
        }

        startVertexIndex += nVerts;
    }
}

} // namespace openspace
