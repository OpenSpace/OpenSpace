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
    constexpr openspace::properties::Property::PropertyInfo PathInfo = {
        "Path",
        "Path",
        "The file path to the data file to read",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SegmentQualityInfo = {
        "SegmentQuality",
        "Segment Quality",
        "A segment quality value for the orbital trail. A value from 1 (lowest) to "
        "10 (highest) that controls the number of line segments in the rendering of the "
        "orbital trail. This does not control the direct number of segments because "
        "these automatically increase according to the eccentricity of the orbit",
        // @VISIBILITY(2.5)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "This value specifies the line width of the trail if the selected rendering "
        "method includes lines. If the rendering mode is set to Points, this value is "
        "ignored",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo LineColorInfo = {
        "Color",
        "Color",
        "This value determines the RGB main color for the lines and points of the trail",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo TrailFadeInfo = {
        "TrailFade",
        "Trail Fade",
        "This value determines how fast the trail fades and is an appearance property.",
        // @VISIBILITY(2.5)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo StartRenderIdxInfo = {
        "StartRenderIdx",
        "Contiguous Starting Index of Render",
        "Index of object in renderable group to start rendering (all prior objects will "
        "be ignored)",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RenderSizeInfo = {
        "RenderSize",
        "Contiguous Size of Render Block",
        "Number of objects to render sequentially from StartRenderIdx",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ContiguousModeInfo = {
        "ContiguousMode",
        "Contiguous Mode",
        "If enabled, then the contiguous set of objects starting from StartRenderIdx "
        "of size RenderSize will be rendered. If disabled, then the number of objects "
        "defined by UpperLimit will rendered from an evenly dispersed sample of the "
        "full length of the data file.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(RenderableOrbitalKepler)]] Parameters {
        // [[codegen::verbatim(PathInfo.description)]]
        std::filesystem::path path;

        enum class [[codegen::map(openspace::kepler::Format)]] Format {
            // A NORAD-style Two-Line element
            TLE,
            // Orbit Mean-Elements Message in the KVN notation
            OMM,
            // JPL's Small Bodies Database
            SBDB
        };
        // The file format that is contained in the file
        Format format;

        // [[codegen::verbatim(SegmentQualityInfo.description)]]
        int segmentQuality;

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;

        // [[codegen::verbatim(LineColorInfo.description)]]
        glm::dvec3 color [[codegen::color()]];

        // [[codegen::verbatim(TrailFadeInfo.description)]]
        std::optional<float> trailFade;

        // [[codegen::verbatim(StartRenderIdxInfo.description)]]
        std::optional<int> startRenderIdx;

        // [[codegen::verbatim(RenderSizeInfo.description)]]
        std::optional<int> renderSize;

        // [[codegen::verbatim(ContiguousModeInfo.description)]]
        std::optional<bool> contiguousMode;
    };
#include "renderableorbitalkepler_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableOrbitalKepler::Documentation() {
    return codegen::doc<Parameters>("space_renderableorbitalkepler");
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

    _appearance.lineColor = p.color;
    _appearance.lineFade = p.trailFade.value_or(20.f);
    _appearance.lineWidth = p.lineWidth.value_or(2.f);
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

    _programObject = SpaceModule::ProgramObjectManager.request(
        "OrbitalKepler",
       []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
           return global::renderEngine->buildRenderProgram(
               "OrbitalKepler",
               absPath("${MODULE_SPACE}/shaders/debrisViz_vs.glsl"),
               absPath("${MODULE_SPACE}/shaders/debrisViz_fs.glsl")
           );
       }
   );

    _uniformCache.modelView = _programObject->uniformLocation("modelViewTransform");
    _uniformCache.projection = _programObject->uniformLocation("projectionTransform");
    _uniformCache.lineFade = _programObject->uniformLocation("lineFade");
    _uniformCache.inGameTime = _programObject->uniformLocation("inGameTime");
    _uniformCache.color = _programObject->uniformLocation("color");
    _uniformCache.opacity = _programObject->uniformLocation("opacity");

    updateBuffers();
}

void RenderableOrbitalKepler::deinitializeGL() {
    glDeleteBuffers(1, &_vertexBuffer);
    glDeleteVertexArrays(1, &_vertexArray);

    SpaceModule::ProgramObjectManager.release(
        "OrbitalKepler",
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
    _programObject = nullptr;
}

bool RenderableOrbitalKepler::isReady() const {
    return _programObject != nullptr;
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

    _programObject->activate();
    _programObject->setUniform(_uniformCache.opacity, opacity());
    _programObject->setUniform(_uniformCache.inGameTime, data.time.j2000Seconds());

    _programObject->setUniform(_uniformCache.modelView, calcModelViewTransform(data));

    // Because we want the property to work similar to the planet trails
    const float fade = std::pow(
        _appearance.lineFade.maxValue() - _appearance.lineFade,
        2.f
    );

    _programObject->setUniform(_uniformCache.projection, data.camera.projectionMatrix());
    _programObject->setUniform(_uniformCache.color, _appearance.lineColor);
    _programObject->setUniform(_uniformCache.lineFade, fade);

    glLineWidth(_appearance.lineWidth);

    //glDepthMask(false);
    //glBlendFunc(GL_SRC_ALPHA, GL_ONE)

    GLint* _si = _startIndex.data();
    GLint* _ss = _segmentSize.data();

    glBindVertexArray(_vertexArray);
    glMultiDrawArrays(GL_LINE_STRIP, _si, _ss, static_cast<GLsizei>(_startIndex.size()));
    glBindVertexArray(0);

    _programObject->deactivate();
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
