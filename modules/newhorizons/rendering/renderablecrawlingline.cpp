/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/newhorizons/rendering/renderablecrawlingline.h>

#include <modules/newhorizons/util/imagesequencer.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/opengl/programobject.h>

#include <ghoul/glm.h>

namespace {
    const char* KeySource = "Source";
    const char* KeyTarget = "Target";
    const char* KeyInstrument = "Instrument";
    const char* KeyColor = "Color";
    const char* KeyColorStart = "Start";
    const char* KeyColorEnd = "End";

    struct VBOData {
        float position[3];
        float color[4];
    };

} // namespace

namespace openspace {

documentation::Documentation RenderableCrawlingLine::Documentation() {
    using namespace documentation;
    return {
        "RenderableCrawlingLine",
        "newhorizons_renderable_crawlingline",
        {
            {
                KeySource,
                new StringVerifier,
                "Denotes the SPICE name of the source of the renderable crawling line, "
                "for example, the space craft",
                Optional::No
            },
            {
                KeyTarget,
                new StringVerifier,
                "Denotes the SPICE name of the target of the crawling line",
                Optional::Yes
            },
            {
                KeyInstrument,
                new StringVerifier,
                "Denotes the SPICE name of the instrument that is used to render the "
                "crawling line",
                Optional::No
            },
            {
                KeyColor,
                new TableVerifier({
                    {
                        {
                            KeyColorStart,
                            new DoubleVector4Verifier,
                            "The color at the start of the line",
                            Optional::No
                        },
                        {
                            KeyColorEnd,
                            new DoubleVector4Verifier,
                            "The color at the end of the line",
                            Optional::No
                        }
                    },
                    Exhaustive::Yes
                }),
                "Specifies the colors that are used for the crawling line. One value "
                "determines the starting color of the line, the second value is the "
                "color at the end of the line.",
                Optional::No
            }
        }
    };
}

RenderableCrawlingLine::RenderableCrawlingLine(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _program(nullptr)
    , _imageSequenceTime(-1.f)
    , _vao(0)
    , _frameCounter(0)
    , _vbo(0)
    , _drawLine(false)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableCrawlingLine"
    );

    _source = dictionary.value<std::string>(KeySource);
    _target = dictionary.value<std::string>(KeyTarget);
    _instrumentName = dictionary.value<std::string>(KeyInstrument);

    _lineColorBegin = dictionary.value<glm::vec4>(
        std::string(KeyColor) + "." + KeyColorStart
    );

    _lineColorEnd = dictionary.value<glm::vec4>(
        std::string(KeyColor) + "." + KeyColorEnd
    );
}

bool RenderableCrawlingLine::isReady() const {
    return (_program != nullptr);
}

bool RenderableCrawlingLine::initialize() {
    RenderEngine& renderEngine = OsEng.renderEngine();
    _program = renderEngine.buildRenderProgram(
        "RenderableCrawlingLine",
        "${MODULE_NEWHORIZONS}/shaders/crawlingline_vs.glsl",
        "${MODULE_NEWHORIZONS}/shaders/crawlingline_fs.glsl"
    );

    glGenVertexArrays(1, &_vao);
    glGenBuffers(1, &_vbo);

    glBindVertexArray(_vao);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(GL_ARRAY_BUFFER, 2 * sizeof(VBOData), NULL, GL_DYNAMIC_DRAW);

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

    return true;
}

bool RenderableCrawlingLine::deinitialize(){
    glDeleteVertexArrays(1, &_vao);
    _vao = 0;
    glDeleteBuffers(1, &_vbo);
    _vbo = 0;

    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_program) {
        renderEngine.removeRenderProgram(_program);
        _program = nullptr;
    }

    return true;
}

void RenderableCrawlingLine::render(const RenderData& data) {
    if (!_drawLine) {
        return;
    }

    _program->activate();
    _frameCounter++;

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    glm::dmat4 modelViewProjectionTransform =
        data.camera.projectionMatrix() *
        glm::mat4(data.camera.combinedViewMatrix() *
            modelTransform
        )    
    ;
    //glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    // setup the data to the shader
    //_program->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
    //_program->setUniform("projectionTransform", data.camera.projectionMatrix());
    _program->setUniform("modelViewProjection", modelViewProjectionTransform);

    int frame = _frameCounter % 60;
    float fadingFactor = static_cast<float>(sin((frame * 3.14159) / 60));
    float alpha = 0.6f + fadingFactor*0.4f;

    glLineWidth(2.f);

    _program->setUniform("_alpha", alpha);
    //_program->setUniform("color", _lineColor);
    //setPscUniforms(*_program.get(), data.camera, data.position);

    glBindVertexArray(_vao);

    glDrawArrays(GL_LINES, 0, 2);
    glBindVertexArray(0);
    
    _program->deactivate();
}

void RenderableCrawlingLine::update(const UpdateData& data) {
    if (_program->isDirty()) {
        _program->rebuildFromFile();
    }

    glm::dmat3 transformMatrix = SpiceManager::ref().positionTransformMatrix(
        _source,
        //"ECLIPJ2000",
        "GALACTIC",
        data.time
    );

    glm::dmat3 tm = SpiceManager::ref().frameTransformationMatrix(_instrumentName, "ECLIPJ2000", data.time);

    //_positions[SourcePosition] = { 0.f, 0.f, 0.f, 0.f };

    glm::dvec3 boresight;
    //try {
        SpiceManager::FieldOfViewResult res =
            SpiceManager::ref().fieldOfView(_source);
        boresight = res.boresightVector;

    //}
    //catch (const SpiceManager::SpiceException& e) {
        //LERROR(e.what());
    //}
    
    glm::vec4 target(boresight[0], boresight[1], boresight[2], 12);
    //target = glm::dmat4(tm) * target;

    //_positions[TargetPosition] = target;
    //_positions[TargetPosition] = {
    //    target.x * pow(10, target.w),
    //    target.y * pow(10, target.w),
    //    target.z * pow(10, target.w),
    //    0
    //};

    VBOData vboData[2] = {
        {
            { 0.f, 0.f, 0.f },
            { _lineColorBegin.r, _lineColorBegin.g, _lineColorBegin.b, _lineColorBegin.a }
        },
        {
            { target.x * powf(10, target.w), target.y * powf(10, target.w), target.z * powf(10, target.w) },
            { _lineColorEnd.r,  _lineColorEnd.g,  _lineColorEnd.b,  _lineColorEnd.a }
        }
    };


    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferSubData(
        GL_ARRAY_BUFFER, 
        0,
        2 * sizeof(VBOData),
        vboData
    );
    //glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(psc) * 2, _positions);

    if (ImageSequencer::ref().isReady()) {
        _imageSequenceTime = ImageSequencer::ref().instrumentActiveTime(_instrumentName);
        _drawLine = _imageSequenceTime != -1.f;
    }
}

} // namespace openspace
