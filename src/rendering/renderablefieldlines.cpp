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
			Renderable(dictionary), _VAO(0), _programUpdateOnSave(false),_update(false) {

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

								// read hints into dictionary
								ghoul::Dictionary hintsDictionary;
								if(fieldline.hasKey("Hints"))
									fieldline.getValue("Hints", hintsDictionary);

								// TODO Vectors of filenames and dictionaries
								_filenames.push_back(file);
								_hintsDictionaries.push_back(hintsDictionary);

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
/*
	_seedpointsProgram = new ghoul::opengl::ProgramObject("SeedpointsProgram");
	ghoul::opengl::ShaderObject* seedpointVertexShader = new ghoul::opengl::ShaderObject(ghoul::opengl::ShaderObject::ShaderTypeVertex,
			"/home/hhellteg/openspace/openspace-data/scene/fieldlines/seedPoints.vert");
	ghoul::opengl::ShaderObject* seedpointFragmentShader = new ghoul::opengl::ShaderObject(ghoul::opengl::ShaderObject::ShaderTypeFragment,
			"/home/hhellteg/openspace/openspace-data/scene/fieldlines/seedPoints.frag");
	_seedpointsProgram->attachObject(seedpointVertexShader);
	_seedpointsProgram->attachObject(seedpointFragmentShader);
*/
	if(dictionary.hasKey("UpdateOnSave")) {
		dictionary.getValue("UpdateOnSave", _programUpdateOnSave);
	}

	setBoundingSphere(PowerScaledScalar::CreatePSS(5));
}

RenderableFieldlines::~RenderableFieldlines() {

}

bool RenderableFieldlines::initialize() {
	assert(_filenames.size() != 0);
	assert(_hintsDictionaries.size() != 0);

	int prevEnd = 0;
	std::vector<glm::vec3> vertexData;
	std::vector<std::vector<glm::vec3> > fieldlinesData;

	for (int i = 0; i < _filenames.size(); ++i) {
		fieldlinesData = getFieldlinesData(_filenames[i], _hintsDictionaries[i]);

		for (int j = 0; j < fieldlinesData.size(); j++) {
			_lineStart.push_back(prevEnd);
			_lineCount.push_back(fieldlinesData[j].size()/2.0);
			prevEnd = prevEnd + fieldlinesData[j].size()/2.0;

			_seedpointStart.push_back(j);
			_seedpointCount.push_back(1);

			vertexData.insert( vertexData.end(), fieldlinesData[j].begin(), fieldlinesData[j].end());
		}
	}

	LDEBUG("Vertex orginal : " << vertexData.size()/2.0);

	//	------ FIELDLINES -----------------
	GLuint vertexPositionBuffer;
	glGenVertexArrays(1, &_VAO); // generate array
	glBindVertexArray(_VAO); // bind array
	glGenBuffers(1, &vertexPositionBuffer); // generate buffer
	glBindBuffer(GL_ARRAY_BUFFER, vertexPositionBuffer); // bind buffer
	glBufferData(GL_ARRAY_BUFFER, vertexData.size()*sizeof(glm::vec3), &vertexData.front(), GL_STATIC_DRAW);

	// Vertex positions
	GLuint vertexLocation = 0;
	glEnableVertexAttribArray(vertexLocation);
	glVertexAttribPointer(vertexLocation, 3, GL_FLOAT, GL_FALSE, 2*sizeof(glm::vec3), reinterpret_cast<void*>(0));

	// Texture coordinates
	GLuint texcoordLocation = 1;
	glEnableVertexAttribArray(texcoordLocation);
	glVertexAttribPointer(texcoordLocation, 3, GL_FLOAT, GL_FALSE, 2*sizeof(glm::vec3), (void*)(sizeof(glm::vec3)));

	glBindBuffer(GL_ARRAY_BUFFER, 0); //unbind buffer
	glBindVertexArray(0); //unbind array

//	//	------ SEEDPOINTS -----------------
	GLuint seedpointPositionBuffer;
	glGenVertexArrays(1, &_seedpointVAO); // generate array
	glBindVertexArray(_seedpointVAO); // bind array
	glGenBuffers(1, &seedpointPositionBuffer); // generate buffer
	glBindBuffer(GL_ARRAY_BUFFER, seedpointPositionBuffer); // bind buffer
	glBufferData(GL_ARRAY_BUFFER, _seedPoints.size()*sizeof(glm::vec3), &_seedPoints.front(), GL_STATIC_DRAW);

	// Vertex positions
	GLuint seedpointLocation = 0;
	glEnableVertexAttribArray(seedpointLocation);
	glVertexAttribPointer(seedpointLocation, 3, GL_FLOAT, GL_FALSE, sizeof(glm::vec3), reinterpret_cast<void*>(0));

	glBindBuffer(GL_ARRAY_BUFFER, 0); //unbind buffer
	glBindVertexArray(0); //unbind array

	//	------ SETUP SHADERS -----------------
	auto privateCallback = [this](const ghoul::filesystem::File& file) {
		_update = true;
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

	if(_update) {
		_update = false;
		safeShaderCompilation();
	}

	glm::mat4 transform = camera->viewProjectionMatrix();
	glm::mat4 camTransform = camera->viewRotationMatrix();
	psc relative = thisPosition-camera->position();

	transform = transform*camTransform;
	transform = glm::translate(transform, relative.vec3());
	transform = glm::rotate(transform, -90.0f, glm::vec3(1.0, 0.0, 0.0)); // Model has positive Z as up
	transform = glm::scale(transform, glm::vec3(0.036*0.5*0.5));
	//transform = glm::scale(transform, glm::vec3(0.1)); // Scale to avoid depth buffer problems

	// Activate shader
	_fieldlinesProgram->activate();
	_fieldlinesProgram->setUniform("modelViewProjection", transform);

	//	------ FIELDLINES -----------------
	glBindVertexArray(_VAO);
	glMultiDrawArrays(GL_LINE_STRIP, &_lineStart[0], &_lineCount[0], _lineStart.size());
	glBindVertexArray(0);

	//	------ SEEDPOINTS -----------------
	// glBindVertexArray(_seedpointVAO);
	// glPointSize(5);
	// glMultiDrawArrays(GL_POINTS, &_lineStart[0], &_lineCount[0], _seedPoints.size());
	// glBindVertexArray(0);


	// Deactivate shader
	_fieldlinesProgram->deactivate();
}

void RenderableFieldlines::update() {
}

void RenderableFieldlines::safeShaderCompilation() {
	_fieldlinesProgram->rebuildFromFile();
	_fieldlinesProgram->compileShaderObjects();
	_fieldlinesProgram->linkProgramObject();
}

std::vector<std::vector<glm::vec3> > RenderableFieldlines::getFieldlinesData(std::string filename, ghoul::Dictionary hintsDictionary) {
	std::string modelString;
	float stepSize = 0.5; // default if no stepsize is specified in hints
	std::string xVariable, yVariable, zVariable;
	KameleonWrapper::Model model;
	std::vector<std::vector<glm::vec3> > fieldlinesData;

	if (hintsDictionary.hasKey("Model") && hintsDictionary.getValue("Model", modelString)) {
		if (modelString == "BATSRUS") {
			model = KameleonWrapper::Model::BATSRUS;
		} else if (modelString == "ENLIL") {
			LWARNING("ENLIL model not supported for fieldlines");
			return fieldlinesData;
		} else {
			LWARNING("Hints does not specify a valid 'Model'");
			return fieldlinesData;
		}

		if (hintsDictionary.hasKey("Variables")) {
			bool xVar, yVar, zVar;
			xVar = hintsDictionary.getValue("Variables.1", xVariable);
			yVar = hintsDictionary.getValue("Variables.2", yVariable);
			zVar = hintsDictionary.getValue("Variables.3", zVariable);

			if (!xVar || !yVar || !zVar) {
				LWARNING("Error reading variables! Must be 3 and must exist in CDF data");
				return fieldlinesData;
			}
		} else {
			LWARNING("Hints does not specify  valid 'Variables'");
			return fieldlinesData;
		}

		if (!hintsDictionary.hasKey("Stepsize") || !hintsDictionary.getValue("Stepsize", stepSize)) {
			LDEBUG("No stepsize set for fieldlines. Setting to default value (" << stepSize << ")");
		}
		ghoul::Dictionary seedpointsDictionary;
		if (hintsDictionary.hasKey("Seedpoints") && hintsDictionary.getValue("Seedpoints", seedpointsDictionary)) {
			glm::vec3 seedPos;
			for (auto index : seedpointsDictionary.keys()) {
				hintsDictionary.getValue("Seedpoints."+index, seedPos);
				_seedPoints.push_back(seedPos);
			}
		}
	}

	KameleonWrapper kw(filename, model);
	fieldlinesData = kw.getFieldLines(xVariable, yVariable, zVariable, _seedPoints, stepSize);
	return fieldlinesData;
}

} // namespace openspace
