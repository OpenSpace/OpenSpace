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

//standard includes 
#include <iostream>
#include <fstream>
#include <limits>
#include <vector>

// open space includes
#include <openspace/rendering/stars/renderablestars.h>
#include <openspace/util/constants.h>

#include <ghoul/opengl/texturereader.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/engine/openspaceengine.h>
#include <sgct.h>

#define printOpenGLError() printOglError(__FILE__, __LINE__)

#define _USE_MATH_DEFINES
#include <math.h>

int printOglError(char *file, int line)
{

	GLenum glErr;
	int    retCode = 0;

	glErr = glGetError();
	if (glErr != GL_NO_ERROR)
	{
		printf("glError in file %s @ line %d: %s\n",
			file, line, gluErrorString(glErr));
		retCode = 1;
	}
	return retCode;
}

namespace {
	const std::string _loggerCat = "RenderableStars";
}

namespace openspace {
RenderableStars::RenderableStars(const ghoul::Dictionary& dictionary)
	: Renderable(dictionary)
	, _colorTexturePath("colorTexture", "Color Texture")
	, _programObject(nullptr)
	, _texture(nullptr)
{

	//setBoundingSphere(PowerScaledScalar::CreatePSS(100));
	std::string path;
	dictionary.getValue(constants::renderablestars::keyPathModule, path);

	std::string texturePath = "";
	if (dictionary.hasKey("Textures.Color")) {
		dictionary.getValue("Textures.Color", texturePath);
		_colorTexturePath = path + "/" + texturePath;
		std::cout << _colorTexturePath.value() << std::endl;
	}
	dictionary.getValue(constants::renderablestars::keySpeckFile, path);

	_speckPath = FileSys.absolutePath(path);

	addProperty(_colorTexturePath);
	_colorTexturePath.onChange(std::bind(&RenderableStars::loadTexture, this));
}

RenderableStars::~RenderableStars(){
	deinitialize();
}


std::ifstream& RenderableStars::skipToLine(std::ifstream& file, unsigned int num){
	file.seekg(std::ios::beg);
	for (int i = 0; i < num - 1; ++i){
		file.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
	}
	return file;
}

bool RenderableStars::readSpeckFile(const std::string& path){
	std::ifstream file;
	std::string str, starDescription, datastr;
	std::vector<std::string> strvec;
	std::vector<float> positions;
	std::vector<float> doubleData;
	std::vector<float> luminocities;
	std::vector<float> absoluteMagnitudes;
	std::vector<float> apparentMagnitudes;

    int count = 0;
	
	const std::string absPath = FileSys.absolutePath(path);
	std::string::size_type last
		= absPath.find_last_of(ghoul::filesystem::FileSystem::PathSeparator);
	if (last == std::string::npos) return false;
	std::string cacheName = absPath.substr(last + 1, absPath.size() - absPath.rfind('.') - 1);
	std::string basePath = absPath.substr(0, last);
	cacheName = basePath + "\\" + cacheName + ".bin";

	// check if cache exists, if not create it. 
	if (!FileSys.fileExists(cacheName)){ 
	//if (FileSys.fileExists(cacheName)){             // TODO: fix so that reads/writes cache. 
		std::ofstream cache;
		cache.open(cacheName, std::ios::binary);
	
		file.open(absPath);
		if (!file.is_open()){
			LERROR("Failed to open spec file for : '" << path << "'");
			return false;
		}
		// count metadata lines to skip.
		do{
			getline(file, str);
			count++;
		} while (str[0] != ' ');
		// set seek pointer to first line with actual data
		skipToLine(file, count);
		count = 0;
	
		do{ 
			getline(file, str);
			if (file.eof()) break;
			// split the line on pound symbol. 
		    std::size_t mid = str.find('#');
			if (mid != std::string::npos){
				datastr = str.substr(0, mid);
				std::size_t end = str.find('\n');
				if (end == std::string::npos)
					starDescription = str.substr(mid, end);
			} 
			// split data string on whitespace to vector
			std::istringstream ss(datastr);
			std::copy(std::istream_iterator<std::string>(ss),
					  std::istream_iterator<std::string>(),
					  std::back_inserter<std::vector<std::string> >(strvec));
			ss.clear();
			// conv. string vector to doubles
			doubleData.reserve(strvec.size());
			transform(strvec.begin(), strvec.end(), back_inserter(doubleData),
					  [](std::string const& val) {return std::stod(val); });

			luminocities.push_back(doubleData[3]); 
			absoluteMagnitudes.push_back(doubleData[4]);
			apparentMagnitudes.push_back(doubleData[5]);

			// convert to powerscaled coordinate
			const psc powerscaled = 
				PowerScaledCoordinate::CreatePowerScaledCoordinate(doubleData[0], 
																   doubleData[1], 
																   doubleData[2]);
			for (int i = 0; i < 4; i++){
				positions.push_back(powerscaled[i]);
				cache << ' ' << powerscaled[i];
			}
			
			strvec.clear();
			doubleData.clear();
			count++;
		} while (file.good());
	}
	else // read cached positions
	{
		LINFO("Found cached data, loading");
		file.open(cacheName, std::ios::binary);
		while (file.good()){
			if (file.eof()) break;
			count++;
			float cachedValue;
			file >> cachedValue;
			positions.push_back(cachedValue);
		}
	}
	// pass in the vectors internal array to create vbo method
	v_size = positions.size();
	
	glGenVertexArrays(1, &_vaoID);
	glGenBuffers(1, &_vboID);

	glBindVertexArray(_vaoID);

	glBindBuffer(GL_ARRAY_BUFFER, _vboID);
	glBufferData(GL_ARRAY_BUFFER, v_size*sizeof(GLfloat), &positions[0], GL_DYNAMIC_DRAW);

	positionAttrib = _programObject->attributeLocation("in_position");

	glBindVertexArray(0);


	return true;
}

bool RenderableStars::initialize(){
	bool completeSuccess = true;
	if (_programObject == nullptr)
		completeSuccess &= OsEng.ref().configurationManager().getValue("StarProgram", _programObject);
	
	if (!readSpeckFile(_speckPath)) 
		LERROR("Failed to read speck file for path : '" << _speckPath << "'");

	loadTexture();
	completeSuccess &= (_texture != nullptr);

	return completeSuccess;
}

bool RenderableStars::deinitialize(){
	delete _texture;
	_texture = nullptr;
	return true;	
}

void RenderableStars::render(const Camera* camera, const psc& thisPosition){
	assert(_programObject);

	// activate shader
	_programObject->activate();

	// fetch data
	psc currentPosition = glm::vec4(0);// thisPosition; // NOTE : currentPosition now same as Earth. 
	psc campos = camera->position();
	glm::mat4 camrot = camera->viewRotationMatrix();
	PowerScaledScalar scaling = camera->scaling();
	// scale the planet to appropriate size since the planet is a unit sphere
	glm::mat4 transform = glm::mat4(1);

	//PowerScaledScalar scaling = glm::vec2(1, -4);
	
	transform = glm::rotate(
		transform, 8.1f * static_cast<float>(sgct::Engine::instance()->getTime()),
		glm::vec3(0.0f, 1.0f, 0.0f));
	
	_programObject->setUniform("model", camera->modelMatrix());
	_programObject->setUniform("view", camera->viewMatrix());
	_programObject->setUniform("projection", camera->projectionMatrix());

	//_programObject->setUniform("ViewProjection", camera->viewProjectionMatrix());
	_programObject->setUniform("ModelTransform", transform);
	_programObject->setUniform("campos", campos.vec4());
	_programObject->setUniform("objpos", currentPosition.vec4());
	_programObject->setUniform("camrot", camrot);
	_programObject->setUniform("scaling", scaling.vec2());
	_programObject->setUniform("Color", glm::vec3(1, 0, 1));

	//glPointSize(1.0);
	
	GLint vertsToDraw = v_size / 4;
	
	glBindVertexArray(_vaoID);
	glEnableVertexAttribArray(positionAttrib);     // use specific input in shader
		glBindBuffer(GL_ARRAY_BUFFER, _vboID);     // bind vbo 
		glVertexAttribPointer(positionAttrib, 4, 
			                  GL_FLOAT, GL_FALSE, 0, (void*)0);
		glDrawArrays(GL_POINTS, 0, vertsToDraw);
		glBindBuffer(GL_ARRAY_BUFFER, 0);
	glDisableVertexAttribArray(positionAttrib);
	glBindVertexArray(0);
	
	// Bind texure
	ghoul::opengl::TextureUnit unit;
	unit.activate();
	_texture->bind();
	_programObject->setUniform("texture1", unit);
	
	_programObject->deactivate();
}

void RenderableStars::loadTexture(){
	delete _texture;
	_texture = nullptr;
	if (_colorTexturePath.value() != "") {
		_texture = ghoul::opengl::loadTexture(absPath(_colorTexturePath));
		if (_texture) {
			LDEBUG("Loaded texture from '" << absPath(_colorTexturePath) << "'");
			_texture->uploadTexture();
		}
	}
}

void RenderableStars::update()
{
}

	
}