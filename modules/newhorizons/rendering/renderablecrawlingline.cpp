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

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <modules/newhorizons/util/imagesequencer.h>

namespace {
    const std::string _loggerCat = "RenderableCrawlingLine";

    const char* KeySource = "Source";
    const char* KeyTarget = "Target";
    const char* KeyInstrument = "Instrument";
    const char* KeyReferenceFrame = "Frame";
    const char* KeyColor = "RGB";

    static const int SourcePosition = 0;
    static const int TargetPosition = 1;
}

namespace openspace {

RenderableCrawlingLine::RenderableCrawlingLine(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _program(nullptr)
    , _imageSequenceTime(-1.f)
    , _vao(0)
    , _vbo(0)
    , _frameCounter(0)
    , _drawLine(false)
{
    dictionary.getValue(KeySource, _source);
    dictionary.getValue(KeyTarget, _target);
    dictionary.getValue(KeyInstrument, _instrumentName);
    dictionary.getValue(KeyReferenceFrame, _referenceFrame);


    if (dictionary.hasKeyAndValue<glm::vec3>(KeyColor)) {
        dictionary.getValue(KeyColor, _lineColor);
    }
    else {
        _lineColor = glm::vec3(1);
    }
}

bool RenderableCrawlingLine::isReady() const {
    bool ready = true;
    ready &= !_source.empty();
    ready &= !_target.empty();
    ready &= !_instrumentName.empty();
    ready &= (_program != nullptr);
    return ready;
}

bool RenderableCrawlingLine::initialize() {
    bool completeSuccess = true;

    RenderEngine& renderEngine = OsEng.renderEngine();
    _program = renderEngine.buildRenderProgram("RenderableCrawlingLine",
        "${MODULE_NEWHORIZONS}/shaders/crawlingline_vs.glsl",
        "${MODULE_NEWHORIZONS}/shaders/crawlingline_fs.glsl");


    if (!_program)
        return false;

    glGenVertexArrays(1, &_vao);
    glGenBuffers(1, &_vbo);

    glBindVertexArray(_vao);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(GL_ARRAY_BUFFER, 2 * sizeof(psc), NULL, GL_DYNAMIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, (void*)0);

    glBindVertexArray(0);

    return completeSuccess;
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
    if (_drawLine) {
        _program->activate();
        _frameCounter++;
        // fetch data
        psc currentPosition = data.position;
        psc campos = data.camera.position();
        glm::mat4 camrot = glm::mat4(data.camera.viewRotationMatrix());

        glm::mat4 transform = glm::mat4(1);

        // setup the data to the shader
        _program->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
        _program->setUniform("ModelTransform", transform);

        int frame = _frameCounter % 60;
        float fadingFactor = static_cast<float>(sin((frame * 3.14159) / 60));
        float alpha = 0.6f + fadingFactor*0.4f;

        glLineWidth(2.f);

        _program->setUniform("_alpha", alpha);
        _program->setUniform("color", _lineColor);
        setPscUniforms(*_program.get(), data.camera, data.position);

        glBindVertexArray(_vao);
        glBindBuffer(GL_ARRAY_BUFFER, _vbo);
        glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(psc) * 2, _positions);

        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, 0);

        glDrawArrays(GL_LINES, 0, 2);
        glBindVertexArray(0);
    
        _program->deactivate();
    }
}

void RenderableCrawlingLine::update(const UpdateData& data) {
    if (_program->isDirty())
        _program->rebuildFromFile();
    glm::dmat3 transformMatrix = SpiceManager::ref().positionTransformMatrix(_source, _referenceFrame, data.time);

    glm::mat4 tmp = glm::mat4(1);
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++){
            tmp[i][j] = static_cast<float>(transformMatrix[i][j]);
        }
    }

    _positions[SourcePosition] = PowerScaledCoordinate::CreatePowerScaledCoordinate(0, 0, 0);

    glm::dvec3 boresight;
    try {
        SpiceManager::FieldOfViewResult res =
            SpiceManager::ref().fieldOfView(_source);
        boresight = res.boresightVector;
        
    }
    catch (const SpiceManager::SpiceException& e) {
        LERROR(e.what());
    }
    
    glm::vec4 target(boresight[0], boresight[1], boresight[2], 12);
    target = tmp * target;

    _positions[TargetPosition] = target;

    if (ImageSequencer::ref().isReady()) {
        _imageSequenceTime = ImageSequencer::ref().instrumentActiveTime(_instrumentName);
        _drawLine = _imageSequenceTime != -1.f;
    }
}


}
