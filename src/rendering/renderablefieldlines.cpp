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
			Renderable(dictionary), _VAO(0), _programUpdateOnSave(false) {
	_shaderMutex = new std::mutex;

	if(dictionary.hasKey("Fieldlines")) {
		ghoul::Dictionary fieldlines;
		if(dictionary.getValue("Fieldlines", fieldlines)) {
			for(auto key: fieldlines.keys()) {
				ghoul::Dictionary fieldline;
				if(fieldlines.getValue(key, fieldline)) {
					if (fieldline.hasKey("File")) {
						std::string file = "";
						if (fieldline.getValue("File", file)) {
							file = findPath(file);
							if (file != "") {

								// parse hints
								ghoul::Dictionary hintsDictionary;
								if(fieldline.hasKey("Hints"))
									fieldline.getValue("Hints", hintsDictionary);

								// TODO Vectors of filenames and dictionaries
								_filename = file;
								_hintsDictionary = hintsDictionary;

								ghoul::Dictionary seedpointsDictionary;
								if (fieldline.hasKey("Seedpoints") && fieldline.getValue("Seedpoints", seedpointsDictionary)) {
									glm::vec3 tmpVal;
									for (int i = 0; i < seedpointsDictionary.keys().size(); ++i) {
										fieldline.getValue("Seedpoints."+std::to_string(i+1), tmpVal);
										_seedPoints.push_back(tmpVal);
									}
								}
							} else
								LERROR("File not found!");
						}
					}
				}
			}
		}
	}

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
	assert(_hintsDictionary.size() != 0);
	assert(_seedPoints.size() != 0);

	std::vector<glm::vec3> seedPoints;
	for (int x = -6; x <= 6; x+=3) {
		for (int y = -6; y <= 6; y+=3) {
			for (int z = -6; z <= 6; z+=3) {
				seedPoints.push_back(glm::vec3((float)x, (float)y, (float)z));
			}
		}
	}

	std::string modelString;
	std::vector<std::vector<glm::vec3> > fieldlinesData;
	float stepSize;
	std::string xVariable, yVariable, zVariable;
	KameleonWrapper::Model model;

	if (_hintsDictionary.hasKey("Model") && _hintsDictionary.getValue("Model", modelString)) {
		if (modelString == "BATSRUS") {
			model = KameleonWrapper::Model::BATSRUS;
		} else if (modelString == "ENLIL") {
			LWARNING("ENLIL model not supported for fieldlines");
			return false;
		} else {
			LWARNING("Hints does not specify a valid 'Model'");
			return false;
		}

		if (_hintsDictionary.hasKey("Variables")) {
			bool xVar, yVar, zVar;
			xVar = _hintsDictionary.getValue("Variables.1", xVariable);
			yVar = _hintsDictionary.getValue("Variables.2", yVariable);
			zVar = _hintsDictionary.getValue("Variables.3", zVariable);

			if (!xVar || !yVar || !zVar) {
				LWARNING("Error reading variables! Must be 3 and must exist in CDF data");
				return false;
			}
		} else {
			LWARNING("Hints does not specify  valid 'Variables'");
			return false;
		}

		if (!_hintsDictionary.hasKey("Stepsize") || !_hintsDictionary.getValue("Stepsize", stepSize)) {
			LDEBUG("No stepsize set for fieldlines. Setting to default value (0.5)");
			stepSize = 0.5;
		}
	}

	KameleonWrapper kw(_filename, model);
	fieldlinesData = kw.getFieldLines(xVariable, yVariable, zVariable, _seedPoints, stepSize);

	std::vector<glm::vec3> vertexData;
	int prevEnd = 0;

	for (int i = 0; i < fieldlinesData.size(); i++) {
		_lineStart.push_back(prevEnd);
		_lineCount.push_back(fieldlinesData[i].size());
		prevEnd = prevEnd + fieldlinesData[i].size();

		vertexData.insert( vertexData.end(), fieldlinesData[i].begin(), fieldlinesData[i].end());
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

	//	------ SETUP SHADERS -----------------
	auto privateCallback = [this](const ghoul::filesystem::File& file) {
		safeShaderCompilation();
	};
	if(_programUpdateOnSave) {
		_vertexSourceFile->setCallback(privateCallback);
		_fragmentSourceFile->setCallback(privateCallback);
	}

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
	transform = glm::rotate(transform, 90.0f, glm::vec3(1.0, 0.0, 0.0));
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
	_shaderMutex->lock();
	_fieldlinesProgram->rebuildFromFile();
	_fieldlinesProgram->compileShaderObjects();
	_fieldlinesProgram->linkProgramObject();
	_shaderMutex->unlock();
}

} // namespace openspace
