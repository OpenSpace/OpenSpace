/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

#include <openspace/rendering/renderablefieldlines.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/kameleonwrapper.h>

namespace {
	std::string _loggerCat = "RenderableFieldlines";
}

namespace openspace {

RenderableFieldlines::RenderableFieldlines(const ghoul::Dictionary& dictionary) :
			RenderableVolume(dictionary), _VAO(0), _programUpdateOnSave(false) {
	_shaderMutex = new std::mutex;

	_filename = "";
	if(dictionary.hasKey("Volume")) {
		if(dictionary.getValue("Volume", _filename)) {
			_filename = findPath(_filename);
		}
	}

	LDEBUG("filename: " << _filename);

	if(dictionary.hasKey("Hints"))
		dictionary.getValue("Hints", _hintsDictionary);

	std::string vshaderpath = "";
	std::string fshaderpath = "";

	if (dictionary.hasKey("Shaders")) {
		ghoul::Dictionary shaderDictionary;
		if(dictionary.getValue("Shaders", shaderDictionary)) {
			if (shaderDictionary.hasKey("VertexShader")) {
				shaderDictionary.getValue("VertexShader", vshaderpath);
			}
			if (shaderDictionary.hasKey("FragmentShader")) {
				shaderDictionary.getValue("FragmentShader", fshaderpath);
			}

			vshaderpath = findPath(vshaderpath);
			fshaderpath = findPath(fshaderpath);

			_vertexSourceFile = new ghoul::filesystem::File(vshaderpath, false);
			_fragmentSourceFile = new ghoul::filesystem::File(fshaderpath, false);

			_fieldlinesProgram = new ghoul::opengl::ProgramObject("FieldlinesProgram");
			ghoul::opengl::ShaderObject* vertexShader = new ghoul::opengl::ShaderObject(ghoul::opengl::ShaderObject::ShaderTypeVertex,vshaderpath);
			ghoul::opengl::ShaderObject* fragmentShader = new ghoul::opengl::ShaderObject(ghoul::opengl::ShaderObject::ShaderTypeFragment,fshaderpath);
			_fieldlinesProgram->attachObject(vertexShader);
			_fieldlinesProgram->attachObject(fragmentShader);
		}
	}

	if(dictionary.hasKey("UpdateOnSave")) {
		dictionary.getValue("UpdateOnSave", _programUpdateOnSave);
	}

	setBoundingSphere(PowerScaledScalar::CreatePSS(5));
}

RenderableFieldlines::~RenderableFieldlines() {

}

bool RenderableFieldlines::initialize() {
	assert(_filename != "");
	KameleonWrapper kameleon(_filename, KameleonWrapper::Model::BATSRUS);
	std::vector<glm::vec3> seedPoints;

	for (int x = -3; x <= 3; x+=3) {
		for (int y = -3; y <= 3; y+=3) {
			for (int z = -3; z <= 3; z+=3) {
				seedPoints.push_back(glm::vec3((float)x, (float)y, (float)z));
			}
		}
	}

	std::vector<std::vector<glm::vec3> > fieldlines = kameleon.getFieldLines("bx", "by", "bz", seedPoints);
	std::vector<glm::vec3> vertexData;
	LDEBUG("Fieldlines.size() = " << fieldlines.size());

	int prevEnd = 0;

	for (int i = 0; i < fieldlines.size(); i++) {
		_lineStart.push_back(prevEnd);
		_lineCount.push_back(fieldlines[i].size());
		prevEnd = prevEnd + fieldlines[i].size();

		vertexData.insert( vertexData.end(), fieldlines[i].begin(), fieldlines[i].end());
	}

	GLuint vertexPositionBuffer;
	glGenVertexArrays(1, &_VAO); // generate array
	glBindVertexArray(_VAO); // bind array
	glGenBuffers(1, &vertexPositionBuffer); // generate buffer
	glBindBuffer(GL_ARRAY_BUFFER, vertexPositionBuffer); // bind buffer
	glBufferData(GL_ARRAY_BUFFER, vertexData.size()*sizeof(glm::vec3), &vertexData.front(), GL_STATIC_DRAW);

	// Vertex positions
	GLuint vertexLocation = 0;
	glEnableVertexAttribArray(vertexLocation);
	glVertexAttribPointer(vertexLocation, 3, GL_FLOAT, GL_FALSE, 3*sizeof(GLfloat), reinterpret_cast<void*>(0));

	glBindBuffer(GL_ARRAY_BUFFER, 0); //unbind buffer
	glBindVertexArray(0); //unbind array

	_fieldlinesProgram->compileShaderObjects();
	_fieldlinesProgram->linkProgramObject();

	return true;
}

bool RenderableFieldlines::deinitialize() {
	return true;
}

void RenderableFieldlines::render(const Camera* camera,	const psc& thisPosition) {
	glm::mat4 transform = camera->viewProjectionMatrix();
	glm::mat4 camTransform = camera->viewRotationMatrix();
	psc relative = thisPosition-camera->position();

	transform = transform*camTransform;
	transform = glm::translate(transform, relative.vec3());
	transform = glm::scale(transform, glm::vec3(0.1));

	_shaderMutex->lock();
	_fieldlinesProgram->activate();
	_fieldlinesProgram->setUniform("modelViewProjection", transform);


	glBindVertexArray(_VAO);
	glMultiDrawArrays(GL_LINE_STRIP, &_lineStart[0], &_lineCount[0], _lineStart.size());
	glBindVertexArray(0);

	_fieldlinesProgram->deactivate();
	_shaderMutex->unlock();
}

void RenderableFieldlines::update() {
}

void RenderableFieldlines::safeShaderCompilation() {
}

} // namespace openspace
