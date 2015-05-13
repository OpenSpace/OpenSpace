/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include <openspace/rendering/renderablecrawlingline.h>

#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/imagesequencer2.h>
#include <imgui.h>

namespace {
	const std::string _loggerCat = "RenderableCrawlingLine";

    const std::string KeySource = "Source";
    const std::string KeyTarget = "Target";
    const std::string KeyInstrument = "Instrument";
    const std::string KeyReferenceFrame = "Frame";
	const std::string keyColor = "RGB";

    static const int SourcePosition = 0;
    static const int TargetPosition = 1;
}

namespace openspace {

RenderableCrawlingLine::RenderableCrawlingLine(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _program(nullptr)
    , _programIsDirty(false)
    , _imageSequenceTime(-1.f)
    , _vao(0)
    , _vbo(0)
{
    dictionary.getValue(KeySource, _source);
    dictionary.getValue(KeyTarget, _target);
    dictionary.getValue(KeyInstrument, _instrumentName);
    dictionary.getValue(KeyReferenceFrame, _referenceFrame);


	if (dictionary.hasKeyAndValue<glm::vec3>(keyColor))
		dictionary.getValue(keyColor, _lineColor);
	else
		_lineColor = glm::vec3(1);
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
	_frameCounter = 0;
    bool completeSuccess = true;
    _program = ghoul::opengl::ProgramObject::Build("RenderableCrawlingLine",
        "${SHADERS}/modules/crawlingline/crawlingline_vs.glsl",
        "${SHADERS}/modules/crawlingline/crawlingline_fs.glsl"
    );
    if (!_program)
        return false;
    _program->setProgramObjectCallback([&](ghoul::opengl::ProgramObject*){ _programIsDirty = true; });

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
	return true;
}

void RenderableCrawlingLine::render(const RenderData& data) {
	if (_drawLine) {
	    _program->activate();
		_frameCounter++;
	    // fetch data
	    psc currentPosition = data.position;
	    psc campos = data.camera.position();
	    glm::mat4 camrot = data.camera.viewRotationMatrix();

	    glm::mat4 transform = glm::mat4(1);

	    // setup the data to the shader
	    _program->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
	    _program->setUniform("ModelTransform", transform);

		int frame = _frameCounter % 60;
		float fadingFactor = static_cast<float>(sin((frame * pi_c()) / 60));
		float alpha = 0.6f + fadingFactor*0.4f;

		glLineWidth(2.f);

        _program->setUniform("_alpha", alpha);
		_program->setUniform("color", _lineColor);
	    setPscUniforms(_program, &data.camera, data.position);

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
    glm::dmat3 transformMatrix = glm::dmat3(1);
	openspace::SpiceManager::ref().getPositionTransformMatrix(_source, _referenceFrame, data.time, transformMatrix);

	glm::mat4 tmp = glm::mat4(1);
	for (int i = 0; i < 3; i++) {
		for (int j = 0; j < 3; j++){
			tmp[i][j] = static_cast<float>(transformMatrix[i][j]);
		}
	}

	_positions[SourcePosition] = PowerScaledCoordinate::CreatePowerScaledCoordinate(0, 0, 0);

	std::string shape, instrument;
	std::vector<glm::dvec3> bounds;
	glm::dvec3 boresight;

	bool found = openspace::SpiceManager::ref().getFieldOfView(_source, shape, instrument, boresight, bounds);
	glm::vec4 target(boresight[0], boresight[1], boresight[2], 12);
	target = tmp * target;

	_positions[TargetPosition] = target;

    if (ImageSequencer2::ref().isReady()) {
        _imageSequenceTime = ImageSequencer2::ref().instrumentActiveTime(_instrumentName);
        _drawLine = _imageSequenceTime != -1.f;
    }
}


}