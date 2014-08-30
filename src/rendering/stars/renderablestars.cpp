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
#define _USE_MATH_DEFINES
#include <math.h>

#define printOpenGLError() printOglError(__FILE__, __LINE__)
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

#define SPRITES

namespace {
	const std::string _loggerCat = "RenderableStars";
}

namespace openspace {
RenderableStars::RenderableStars(const ghoul::Dictionary& dictionary)
	: Renderable(dictionary)
	, _colorTexturePath("colorTexture", "Color Texture")
	, _programObject(nullptr)
	, _programObjectPoints(nullptr)
	, _texture(nullptr)
{
	//setBoundingSphere(PowerScaledScalar::CreatePSS(100)); // <---- do we need this?
	std::string path;
	dictionary.getValue(constants::renderablestars::keyPathModule, path);

	std::string texturePath = "";
	if (dictionary.hasKey("Textures.Color")) {
		dictionary.getValue("Textures.Color", texturePath);
		_colorTexturePath = path + "/" + texturePath;
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
	std::vector<float> floatingPointData;

    int count = 0;
	
	const std::string absPath = FileSys.absolutePath(path);
	std::string::size_type last
		= absPath.find_last_of(ghoul::filesystem::FileSystem::PathSeparator);
	if (last == std::string::npos) return false;
	std::string cacheName = absPath.substr(last + 1, absPath.size() - absPath.rfind('.') - 1);
	std::string basePath = absPath.substr(0, last);
	cacheName = basePath + "\\" + cacheName + ".bin";

	/**
		TODO:
		Right now we are not reading cache files as luminocity, appmag etc are not cached 
		in the first place. This is the reason for longer loading times, will get fixed.

		The READ LOGIC is simple:
		1. skip metadata 
		2. read everything in line until # symbol (nongeneric reader)
		3. split line on whitespaces
		4. (FOR NOW) grab x,y,z only
		5. convert x,y,z to floats
		6. convert m -> pc
		7. convert to psc
		8. pass to vector
		9. pass vectors internal arr for vbo creation
	*/

	//if (!FileSys.fileExists(cacheName)){ 
	if (FileSys.fileExists(cacheName)){             // TODO: fix so that reads/writes cache. 
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
			// split the line on pound symbol. we only want data.
		    std::size_t mid = str.find('#');
			if (mid != std::string::npos){
				datastr = str.substr(0, mid);
				std::size_t end = str.find('\n');
				if (end == std::string::npos)
					starDescription = str.substr(mid, end);
			} 
			// split data string on whitespace -> push to to vector
			std::istringstream ss(datastr);
			std::copy(std::istream_iterator<std::string>(ss),
					  std::istream_iterator<std::string>(),
					  std::back_inserter<std::vector<std::string> >(strvec));
			ss.clear();
			// conv. string vector to doubles
			floatingPointData.reserve(strvec.size());
			transform(strvec.begin(), strvec.end(), back_inserter(floatingPointData),
					  [](std::string const& val) {return std::stod(val); });

			// store data concerning apparent luminocity, brigthness etc. 

			// convert to powerscaled coordinate
			psc powerscaled = 
				PowerScaledCoordinate::CreatePowerScaledCoordinate(floatingPointData[0], 
																   floatingPointData[1], 
																   floatingPointData[2]);
			// Convert parsecs -> meter
			// Could convert floatingPointData instead 
			// (possible as 3.4 × 10^38 is max rep nr of float)
			PowerScaledScalar parsecsToMetersFactor = glm::vec2(0.308567758, 17);
			//powerscaled *= parsecsToMetersFactor; //<--- buggy. 
			powerscaled[0] *= parsecsToMetersFactor[0];
			powerscaled[1] *= parsecsToMetersFactor[0];
			powerscaled[2] *= parsecsToMetersFactor[0];
			powerscaled[3] += parsecsToMetersFactor[1];
			
			// We use vector to store data
			// needs no preallocation and has tightly packed arr.
			for (int i = 0; i < 4; i++){
				positions.push_back(powerscaled[i]);
				cache << ' ' << powerscaled[i];
			}
			// will need more elegant solution here. // TODO
			positions.push_back(floatingPointData[3]);
			positions.push_back(floatingPointData[4]);
			positions.push_back(floatingPointData[5]);
			
			strvec.clear();
			floatingPointData.clear();
			count++;
		} while (file.good());
	}
	else // read cached positions, NOTE: this is not fully functional. 
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
	v_size = positions.size(); // account for size of extra data. 

	// create vbo (now positions ONLY)
	glGenVertexArrays(1, &_vaoID);
		glGenBuffers(1, &_vboID);
		glBindVertexArray(_vaoID);
		glBindBuffer(GL_ARRAY_BUFFER, _vboID);
		glBufferData(GL_ARRAY_BUFFER, v_size*sizeof(GLfloat), &positions[0], GL_DYNAMIC_DRAW); // x,y,z,lum,appmag,absol..

	glBindVertexArray(0);
	return true;
}

bool RenderableStars::initialize(){
	// We use two shaders, compiled and linked by scenegraph.cpp
	// 1. StarProgram  - Generates quads with png image of halo
	// 2. PointProgram - Generates gl_Points in geom shader.

	bool completeSuccess = true;
	if (_programObject == nullptr)
		completeSuccess &= OsEng.ref().configurationManager().getValue("StarProgram", _programObject);
	if (_programObjectPoints == nullptr)
		completeSuccess &= OsEng.ref().configurationManager().getValue("PointProgram", _programObjectPoints);
	
	// Run read routine.
	if (!readSpeckFile(_speckPath)) 
		LERROR("Failed to read speck file for path : '" << _speckPath << "'");

#ifdef SPRITES
	loadTexture();
#endif
	completeSuccess &= (_texture != nullptr);

	return completeSuccess;
}

bool RenderableStars::deinitialize(){
	delete _texture;
	_texture = nullptr;
	return true;	
}

#define GLSPRITES
#define GLPOINTS
void RenderableStars::render(const Camera* camera, const psc& thisPosition){
	assert(_programObject);
	printOpenGLError();
	// activate shader
	_programObject->activate();

	// fetch data
	psc currentPosition = glm::vec4(0);// thisPosition; // NOTE : currentPosition now same as Earth. 
	psc campos = camera->position();
	glm::mat4 camrot = camera->viewRotationMatrix();
	PowerScaledScalar scaling = camera->scaling();
	// scale the planet to appropriate size since the planet is a unit sphere
	glm::mat4 transform = glm::mat4(1); 

	// why?
	scaling = glm::vec2(1, -22);
	//scaling = glm::vec2(1, -4);

	GLint vertsToDraw = v_size / 7; // account for data size. 
	GLsizei stride = sizeof(GLfloat) * 7; // 7 component stride
	
	transform = glm::rotate(transform, 
		                    1.1f * static_cast<float>(sgct::Engine::instance()->getTime()),  
							glm::vec3(0.0f, 1.0f, 0.0f));
    
	positionAttrib = _programObject->attributeLocation("in_position");

	// disable depth test, enable additative blending
	glDisable(GL_DEPTH_TEST);
	glEnable(GL_BLEND);
	glBlendFunc(GL_ONE_MINUS_DST_COLOR, GL_ONE); 

// ---------------------- RENDER HALOS -----------------------------
#ifdef GLSPRITES	
	_programObject->setUniform("ViewProjection", camera->viewProjectionMatrix());
	_programObject->setUniform("ModelTransform", transform);
	_programObject->setUniform("campos", campos.vec4());
	_programObject->setUniform("objpos", currentPosition.vec4());
	_programObject->setUniform("camrot", camrot);
	_programObject->setUniform("scaling", scaling.vec2());

	// Bind texure
	ghoul::opengl::TextureUnit unit;
	unit.activate();
	_texture->bind();
	_programObject->setUniform("texture1", unit);

	// activate the VBO. 
	glBindVertexArray(_vaoID);
	glBindBuffer(GL_ARRAY_BUFFER, _vboID);
	glEnableVertexAttribArray(positionAttrib);  
		glVertexAttribPointer(positionAttrib, 4, GL_FLOAT, GL_FALSE, stride, (void*)0);
		glDrawArrays(GL_POINTS, 0, vertsToDraw);
	glDisableVertexAttribArray(positionAttrib);
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindVertexArray(0);

	glDisable(GL_BLEND);
	glEnable(GL_DEPTH_TEST);
#endif
	_programObject->deactivate();

// ---------------------- RENDER POINTS -----------------------------
#ifdef GLPOINTS
	_programObjectPoints->activate();

	_programObjectPoints->setUniform("ViewProjection", camera->viewProjectionMatrix());
	_programObjectPoints->setUniform("ModelTransform", transform);
	_programObjectPoints->setUniform("campos", campos.vec4());
	_programObjectPoints->setUniform("objpos", currentPosition.vec4());
	_programObjectPoints->setUniform("camrot", camrot);
	_programObjectPoints->setUniform("scaling", scaling.vec2());

	glEnable(GL_PROGRAM_POINT_SIZE_EXT); //Allows shader to determine pointsize. 
	brightnessDataAttrib = _programObjectPoints->attributeLocation("in_brightness");

	glBindVertexArray(_vaoID);
	glBindBuffer(GL_ARRAY_BUFFER, _vboID);
	glEnableVertexAttribArray(positionAttrib);  
	glEnableVertexAttribArray(brightnessDataAttrib);
		glVertexAttribPointer(positionAttrib, 4, GL_FLOAT, GL_FALSE, stride, (void*)0); // xyz
		glVertexAttribPointer(brightnessDataAttrib, 3, GL_FLOAT, GL_FALSE, stride, (void*)(4 * sizeof(GLfloat)));
		glDrawArrays(GL_POINTS, 0, vertsToDraw);
	glDisableVertexAttribArray(positionAttrib);
	glDisableVertexAttribArray(brightnessDataAttrib);
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindVertexArray(0);
	
	_programObjectPoints->deactivate();
#endif
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