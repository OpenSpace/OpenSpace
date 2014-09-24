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
#include <iomanip>      // std::setw


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
int printOglError(char *file, int line){
	GLenum glErr;
	int    retCode = 0;
	glErr = glGetError();
	if (glErr != GL_NO_ERROR){
		printf("glError in file %s @ line %d: %s\n",
			file, line, gluErrorString(glErr));
		retCode = 1;
	}
	return retCode;
}


#define GLSPRITES
#define GLPOINTS

namespace {
	const std::string _loggerCat = "RenderableStars";
}

namespace openspace {
RenderableStars::RenderableStars(const ghoul::Dictionary& dictionary)
	: Renderable(dictionary)
	, _colorTexturePath("colorTexture", "Color Texture")
	, _haloProgram(nullptr)
	, _pointProgram(nullptr)
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

//these two methods exist only to debug geom shaders code. 
float log10(float x){ return log(x)/ log(10); }
psc psc_addition(psc v1, psc v2) {
	float k = 10.f;
	float ds = v2[3] - v1[3];
	if (ds >= 0) {
		float p = pow(k, -ds);
		return psc(v1[0]*p + v2[0], v1[1]*p + v2[1], v1[2]*p + v2[2], v2[3]);
	}
	else {
		float p = pow(k, ds);
		return psc(v1[0] + v2[0]*p, v1[1] + v2[1]*p, v1[2] + v2[2]*p, v1[3]);
	}
}


bool RenderableStars::readSpeckFile(const std::string& path){

	bool readCache = false;

	std::ifstream file;
	std::string str, starDescription, datastr;
	std::vector<std::string> strvec;
	std::vector<float> starcluster;
	std::vector<float> floatingPointData;

    int count = 0;
	std::cout << path << std::endl;
	const std::string absPath = FileSys.absolutePath(path);
	std::string::size_type last
		= absPath.find_last_of(ghoul::filesystem::FileSystem::PathSeparator);
	if (last == std::string::npos) return false;
	std::string cacheName = absPath.substr(last + 1, absPath.size() - absPath.rfind('.') - 1);
	std::string basePath = absPath.substr(0, last);
	cacheName = basePath + "\\" + cacheName + ".bin";

	/**
		The READ LOGIC:
		1. skip metadata 
		2. read everything in line until # symbol (nongeneric reader)
		3. split line on whitespaces
		4. convert x,y,z to floats
		5. convert m -> pc
		6. convert to psc
		7. pass to vector
		8. pass vectors internal arr for vbo creation

		TODO:
		Right now we are not reading cache files as luminocity, appmag etc are not cached
		in the first place. This is the reason for longer loading times, will get fixed.
	*/

	//if (!FileSys.fileExists(cacheName)){ 
	if (!readCache){  // dumb boolean for now.            
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
			psc powerscaled = PowerScaledCoordinate::CreatePowerScaledCoordinate(floatingPointData[0], 
												  							     floatingPointData[1], 
												  							     floatingPointData[2]);
			// Convert parsecs -> meter
			// Could convert floatingPointData instead ??
			// (possible as 3.4 × 10^38 is max rep nr of float)
			PowerScaledScalar parsecsToMetersFactor = glm::vec2(0.308567758, 17);
			//powerscaled *= parsecsToMetersFactor; // <--- buggy dont use. 
			powerscaled[0] *= parsecsToMetersFactor[0];
			powerscaled[1] *= parsecsToMetersFactor[0];
			powerscaled[2] *= parsecsToMetersFactor[0];
			powerscaled[3] += parsecsToMetersFactor[1];
			
			// TESTING DISTANCE MODULUS FORMULA LOGIC (see: star_ge.glsl) ------------------------------------
					//psc campos(3.17657, 0.0, 0.0, 4.0); // same input as in shader.
					psc campos(0.8, 0.0, 0.0, 4.0);
					psc psc_position = powerscaled;

					float k = 10.f;

					psc cam_negative = psc(-campos[0], -campos[1], -campos[2], campos[3]);
					psc result = psc_addition(psc_position, cam_negative);
					// checked with wolfram, correct. 
					//std::cout << result << std::endl;
					float x, y, z, w;
					x = result[0];
					y = result[1];
					z = result[2];
					w = result[3];
		
					glm::vec2 dpc_psc( sqrt(x*x + y*y + z*z), w );
					// checked with wolfram, correct. 

					dpc_psc[0] *= 0.324077929;
					dpc_psc[1] += -18;

					float parsecDist = dpc_psc[0] * pow(10, dpc_psc[1]);
					// checked with wolfram, 100% correct to the 8th decimal. 

					float M = floatingPointData[3];
					float apparent = (M - 5.0f * (1.f - log10(parsecDist)));
					// checked with wolfram, 100% correct to the 7th decimal. 

					// WORKS, REPRODUCE ON SHADER!!!!!!

					//std::cout << std::setw(3) << round(apparent) << " ";
					//if (count % 15 == 0) std::cout << std::endl;
			// --------------------------------------------------------------------------------
			
			// We use std::vector to store data
			// needs no preallocation and has tightly packed arr.
			for (int i = 0; i < 4; i++){
				starcluster.push_back(powerscaled[i]);
				cache << ' ' << powerscaled[i];
			}
			// will need more elegant solution here.
			starcluster.push_back(floatingPointData[3]);
			starcluster.push_back(floatingPointData[4]);
			starcluster.push_back(floatingPointData[5]); 

			strvec.clear();
			floatingPointData.clear();
			count++;
		} while (file.good());
	}else{
		LINFO("Found cached data, loading");
		file.open(cacheName, std::ios::binary);
		while (file.good()){
			if (file.eof()) break;
			count++;
			float cachedValue;
			file >> cachedValue;
			starcluster.push_back(cachedValue);
		}
	}
	v_stride    = 7;                      // stride in VBO, set manually for now.
	v_size      = starcluster.size();     // size of VBO
	v_total     = v_size / v_stride;      // total number of vertecies 

	// create vao and interleaved vbo from vectors internal array
	generateBufferObjects(&starcluster[0]);
	
	return true;
}

void RenderableStars::generateBufferObjects(const void* data){
	// generate and buffer data 
	glGenVertexArrays(1, &_vaoID);
	glGenBuffers(1, &_vboID);
	glBindVertexArray(_vaoID);
	glBindBuffer(GL_ARRAY_BUFFER, _vboID);
	glBufferData(GL_ARRAY_BUFFER, v_size*sizeof(GLfloat), data, GL_STATIC_DRAW); // order : x, y, z, lum, appmag, absmag

	positionAttrib       = _haloProgram->attributeLocation( "in_position"   );
	brightnessDataAttrib = _haloProgram->attributeLocation( "in_brightness" );

	GLint postest = _pointProgram->attributeLocation("in_position");
	GLint britest = _pointProgram->attributeLocation("in_brightness");

	assert(postest == positionAttrib); // assume pointer locations same for both programs. 
	assert(britest == brightnessDataAttrib);

	GLsizei stride = sizeof(GLfloat) * v_stride;

	glBindVertexArray(_vaoID);                                                                                // vao holds ref. to vbo
	glBindBuffer(GL_ARRAY_BUFFER, _vboID);																	  // bind vbo
	glEnableVertexAttribArray(positionAttrib);                                                                // enable acess attribute in_position
	glEnableVertexAttribArray(brightnessDataAttrib);                                                          // enable acess attribute in_brigthness
	glVertexAttribPointer(positionAttrib, 4, GL_FLOAT, GL_FALSE, stride, (void*)0);                           // psc coordinates
	glVertexAttribPointer(brightnessDataAttrib, 3, GL_FLOAT, GL_FALSE, stride, (void*)(4 * sizeof(GLfloat))); // brigthness properties 
	glBindBuffer(GL_ARRAY_BUFFER, 0);																		  // unbind
	glBindVertexArray(0);
}

bool RenderableStars::initialize(){
	bool completeSuccess = true;

	// 1. StarProgram  - Generates quads with png image of halo
	if (_haloProgram == nullptr)
		completeSuccess &= OsEng.ref().configurationManager().getValue("StarProgram", _haloProgram);

	// 2. PointProgram - Generates gl_Points in geom shader.
	if (_pointProgram == nullptr)
		completeSuccess &= OsEng.ref().configurationManager().getValue("PointProgram", _pointProgram);
	
	// Run read star-datafile routine.
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

//#define TMAT
void RenderableStars::render(const Camera* camera, const psc& thisPosition){
	assert(_haloProgram);
	printOpenGLError();
	// activate shader
	_haloProgram->activate();

//	psc vantagePointTestTarget = PowerScaledCoordinate::CreatePowerScaledCoordinate(8000, 0.0, 0.0);
	//	psc campos = vantagePointTestTarget;

	// fetch data
	psc currentPosition       = thisPosition;
	psc campos                = camera->position();
	glm::mat4 camrot          = camera->viewRotationMatrix();
	PowerScaledScalar scaling = camera->scaling();
	glm::mat4 transform       = glm::mat4(1); 
	//scaling                   = glm::vec2(1, -22);  
	scaling = glm::vec2(1, -19);  

#ifdef TMAT
	transform = glm::rotate(transform, 
		                    1.1f * static_cast<float>(sgct::Engine::instance()->getTime()),  
							glm::vec3(0.0f, 1.0f, 0.0f));
#endif
	// disable depth test, enable additative blending
	glDisable(GL_DEPTH_TEST);
	glEnable(GL_BLEND);
	glBlendFunc(GL_ONE_MINUS_DST_COLOR, GL_ONE); 

#ifdef GLSPRITES
	glm::mat4 modelMatrix      = camera->modelMatrix();
	glm::mat4 viewMatrix       = camera->viewMatrix();
	glm::mat4 projectionMatrix = camera->projectionMatrix();

// ---------------------- RENDER HALOS -----------------------------
	_haloProgram->setUniform("model", modelMatrix);
	_haloProgram->setUniform("view", viewMatrix);
	_haloProgram->setUniform("projection", projectionMatrix);

	//_haloProgram->setUniform("ViewProjection", camera->viewProjectionMatrix());
	_haloProgram->setUniform("ModelTransform", transform);
	_haloProgram->setUniform("campos", campos.vec4());
	_haloProgram->setUniform("objpos", currentPosition.vec4());
	_haloProgram->setUniform("camrot", camrot);
	_haloProgram->setUniform("scaling", scaling.vec2());

	// Bind texure
	ghoul::opengl::TextureUnit unit;
	unit.activate();
	_texture->bind();
	_haloProgram->setUniform("texture1", unit);

	// activate the VBO. 
	glBindVertexArray(_vaoID);
		glDrawArrays(GL_POINTS, 0, v_total);  
	glBindVertexArray(0);
#endif
	_haloProgram->deactivate();

#ifdef GLPOINTS
	/*
// ---------------------- RENDER POINTS -----------------------------
	_pointProgram->activate();

	_pointProgram->setUniform("ViewProjection", camera->viewProjectionMatrix());
	_pointProgram->setUniform("ModelTransform", transform);
	_pointProgram->setUniform("campos", campos.vec4());
	_pointProgram->setUniform("objpos", currentPosition.vec4());
	_pointProgram->setUniform("camrot", camrot);
	//_pointProgram->setUniform("scaling", scaling.vec2());

	glEnable(GL_PROGRAM_POINT_SIZE_EXT); // Allows shader to determine pointsize. 

	//glEnable(GL_POINT_SMOOTH);         // decrepated in core profile, workaround in frag.
	glBindVertexArray(_vaoID); 
		glDrawArrays(GL_POINTS, 0, v_total*7);
	glBindVertexArray(0);
	
	glDisable(GL_BLEND);
	
	_pointProgram->deactivate();
	*/
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