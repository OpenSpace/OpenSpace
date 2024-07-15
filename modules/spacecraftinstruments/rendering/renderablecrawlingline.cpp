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

#include <modules/spacecraftinstruments/rendering/renderablecrawlingline.h>

#include <modules/spacecraftinstruments/util/imagesequencer.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>

namespace {
    struct VBOData {
        std::array<float, 3> position;
        std::array<float, 4> color;
    };

    struct [[codegen::Dictionary(RenderableCrawlingLine)]] Parameters {
        // The SPICE name of the source of the crawling line. For example, the spacecraft.
        std::string source;

        // The SPICE name of the target of the crawling line.
        std::string target;

        // The SPICE name of the instrument that is used to render the crawling line.
        std::string instrument;

        struct Color {
            // The color at the start of the line.
            glm::vec4 start [[codegen::color()]];

            // The color at the end of the line.
            glm::vec4 end [[codegen::color()]];
        };
        // The colors used for the crawling line, given as one color at the start of
        // the line and one at the end.
        Color color;
    };
#include "renderablecrawlingline_codegen.cpp"
} // namespace

// @TODO:  This class is not properly working anymore and needs to be substantially
//         rewritten. When doing so, make sure that any color property uses three
//         values, not four. The opacity should be handled separately

namespace openspace {

documentation::Documentation RenderableCrawlingLine::Documentation() {
    return codegen::doc<Parameters>("spacecraftinstruments_renderablecrawlingline");
}

RenderableCrawlingLine::RenderableCrawlingLine(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _source = p.source;
    _target = p.target;
    _instrumentName = p.instrument;
    _lineColorBegin = p.color.start;
    _lineColorEnd = p.color.end;
}

bool RenderableCrawlingLine::isReady() const {
    return _program != nullptr;
}

void RenderableCrawlingLine::initializeGL() {
    _program = global::renderEngine->buildRenderProgram(
        "RenderableCrawlingLine",
        absPath("${MODULE_SPACECRAFTINSTRUMENTS}/shaders/crawlingline_vs.glsl"),
        absPath("${MODULE_SPACECRAFTINSTRUMENTS}/shaders/crawlingline_fs.glsl")
    );

    glGenVertexArrays(1, &_vao);
    glGenBuffers(1, &_vbo);

    glBindVertexArray(_vao);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(GL_ARRAY_BUFFER, 2 * sizeof(VBOData), nullptr, GL_DYNAMIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(VBOData), nullptr);

    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        4,
        GL_FLOAT,
        GL_FALSE,
        sizeof(VBOData),
        reinterpret_cast<void*>(offsetof(VBOData, color))
    );

    glBindVertexArray(0);
}

void RenderableCrawlingLine::deinitializeGL() {
    glDeleteVertexArrays(1, &_vao);
    _vao = 0;
    glDeleteBuffers(1, &_vbo);
    _vbo = 0;

    if (_program) {
        global::renderEngine->removeRenderProgram(_program.get());
        _program = nullptr;
    }
}

void RenderableCrawlingLine::render(const RenderData& data, RendererTasks&) {
    if (!_drawLine) {
        return;
    }

    _program->activate();
    _frameCounter++;

    const glm::dmat4 modelViewProjection = calcModelViewProjectionTransform(data);
    _program->setUniform("modelViewProjection", modelViewProjection);

    const int frame = _frameCounter % 60;
    const float fadingFactor = std::sin(frame * glm::pi<float>() / 60.f);
    const float alpha = 0.6f + fadingFactor * 0.4f;

    glLineWidth(2.f);

    _program->setUniform("alpha", alpha);

    glBindVertexArray(_vao);
    glDrawArrays(GL_LINES, 0, 2);
    glBindVertexArray(0);

    _program->deactivate();
}

void RenderableCrawlingLine::update(const UpdateData& data) {
    if (_program->isDirty()) {
        _program->rebuildFromFile();
    }

    const glm::dmat3 tm = SpiceManager::ref().frameTransformationMatrix(
        _instrumentName,
        "ECLIPJ2000",
        data.time.j2000Seconds()
    );

    const SpiceManager::FieldOfViewResult res = SpiceManager::ref().fieldOfView(_source);
    const glm::dvec3 boresight = res.boresightVector;
    const glm::vec4 target = glm::dmat4(tm) * glm::vec4(boresight, 12);

    std::array<VBOData, 2> vboData = {
        VBOData {
            { 0.f, 0.f, 0.f },
            { _lineColorBegin.r, _lineColorBegin.g, _lineColorBegin.b, _lineColorBegin.a }
        },
        VBOData {
            {
                target.x * powf(10, target.w),
                target.y * powf(10, target.w),
                target.z * powf(10, target.w)
            },
            { _lineColorEnd.r,  _lineColorEnd.g,  _lineColorEnd.b,  _lineColorEnd.a }
        }
    };


    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferSubData(GL_ARRAY_BUFFER, 0, 2 * sizeof(VBOData), vboData.data());

    if (ImageSequencer::ref().isReady()) {
        const float imageSequenceTime = ImageSequencer::ref().instrumentActiveTime(
            data.time.j2000Seconds(),
            _instrumentName
        );

        _drawLine = (imageSequenceTime != -1.f);
    }
}

} // namespace openspace
