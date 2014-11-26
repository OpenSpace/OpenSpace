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

#include <openspace/rendering/stars/renderablestars.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/util/constants.h>

#include <ghoul/filesystem/filesystem>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/opengl/texturereader.h>
#include <ghoul/opengl/textureunit.h>

#include <fstream>

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
	std::string texturePath = "";
	if (dictionary.hasKey(constants::renderablestars::keyTexture)) {
		dictionary.getValue(constants::renderablestars::keyTexture, texturePath);
		_colorTexturePath = absPath(texturePath);
	}

	bool success = dictionary.getValue(constants::renderablestars::keyFile, _speckPath);
	if (!success) {
		LERROR("SpeckDataSource did not contain key '" <<
			constants::renderablestars::keyFile << "'");
		return;
	}
	_speckPath = absPath(_speckPath);

	addProperty(_colorTexturePath);
	_colorTexturePath.onChange(std::bind(&RenderableStars::loadTexture, this));
}

RenderableStars::~RenderableStars() {
}

std::ifstream& skipToLine(std::ifstream& file, unsigned int num){
	file.seekg(std::ios::beg);
	for (size_t i = 0; i < num - 1; ++i){
		file.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
	}
	return file;
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

bool RenderableStars::initialize() {
	bool completeSuccess = true;

	// 1. StarProgram  - Generates quads with png image of halo
	if (_haloProgram == nullptr)
		completeSuccess &= OsEng.ref().configurationManager().getValue("StarProgram", _haloProgram);

	// 2. PointProgram - Generates gl_Points in geom shader.
	if (_pointProgram == nullptr)
		completeSuccess &= OsEng.ref().configurationManager().getValue("PointProgram", _pointProgram);
	
	completeSuccess &= loadData();
	const std::vector<float>& data = this->data();

	v_stride = 7;                      // stride in VBO, set manually for now.
	v_size = static_cast<int>(data.size());     // size of VBO
	v_total = v_size / v_stride;      // total number of vertecies 

	// create vao and interleaved vbo from vectors internal array
	generateBufferObjects(&data[0]);


	//// Run read star-datafile routine.
	//if (!readSpeckFile(_speckPath)) 
	//	LERROR("Failed to read speck file for path : '" << _speckPath << "'");

	loadTexture();
	completeSuccess &= (_texture != nullptr);

	return completeSuccess;
}

bool RenderableStars::deinitialize(){
	glDeleteBuffers(1, &_vboID);
	glDeleteVertexArrays(1, &_vaoID);

	delete _texture;
	_texture = nullptr;
	return true;	
}

//#define TMAT
void RenderableStars::render(const RenderData& data){
	if(!_haloProgram)
		return;
	if(!_texture)
		return;
	assert(_haloProgram);
	//printOpenGLError();
	// activate shader
	_haloProgram->activate();

	// fetch data
	//scaling                   = glm::vec2(1, -22);  
	glm::vec2 scaling = glm::vec2(1, -19);  

#ifdef TMAT
	transform = glm::rotate(transform, 
		                    1.1f * static_cast<float>(sgct::Engine::instance()->getTime()),  
							glm::vec3(0.0f, 1.0f, 0.0f));
#endif
	// disable depth test, enable additative blending
	glDisable(GL_DEPTH_TEST);
	glEnable(GL_BLEND);
	glBlendFunc(GL_ONE_MINUS_DST_COLOR, GL_ONE);

	glm::mat4 modelMatrix      = data.camera.modelMatrix();
	glm::mat4 viewMatrix       = data.camera.viewMatrix();
	glm::mat4 projectionMatrix = data.camera.projectionMatrix();

// ---------------------- RENDER HALOS -----------------------------
	_haloProgram->setUniform("model", modelMatrix);
	_haloProgram->setUniform("view", viewMatrix);
	_haloProgram->setUniform("projection", projectionMatrix);

	//_haloProgram->setUniform("ViewProjection", camera->viewProjectionMatrix());
	_haloProgram->setUniform("ModelTransform", glm::mat4(1));
	setPscUniforms(_haloProgram, &data.camera, data.position);
	_haloProgram->setUniform("scaling", scaling);

	// Bind texure
	ghoul::opengl::TextureUnit unit;
	unit.activate();
	_texture->bind();
	_haloProgram->setUniform("texture1", unit);

	// activate the VBO. 
	glBindVertexArray(_vaoID);
		glDrawArrays(GL_POINTS, 0, v_total);  
	glBindVertexArray(0);
	_haloProgram->deactivate();

//#ifdef GLPOINTS
//
//// ---------------------- RENDER POINTS -----------------------------
//	_pointProgram->activate();
//
//	_pointProgram->setUniform("ViewProjection", data.camera.viewProjectionMatrix);
//	_pointProgram->setUniform("ModelTransform", transform);
//	_pointProgram->setUniform("campos", campos.vec4());
//	_pointProgram->setUniform("objpos", currentPosition.vec4());
//	_pointProgram->setUniform("camrot", camrot);
//	//_pointProgram->setUniform("scaling", scaling.vec2());
//
//	glEnable(GL_PROGRAM_POINT_SIZE_EXT); // Allows shader to determine pointsize. 
//
//	//glEnable(GL_POINT_SMOOTH);         // decrepated in core profile, workaround in frag.
//	glBindVertexArray(_vaoID); 
//		glDrawArrays(GL_POINTS, 0, v_total*7);
//	glBindVertexArray(0);
//	
//	glDisable(GL_BLEND);
//	
//	_pointProgram->deactivate();
//	glEnable(GL_DEPTH_TEST);
//
//#endif
	glDisable(GL_BLEND);
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

void RenderableStars::update(const UpdateData& data)
{
	
}

const std::vector<float>& RenderableStars::data() const {
	return _data;
}


//RenderableStars::SpeckDataSource(const ghoul::Dictionary& dictionary)
//	: DataSource()
//	, _file("")
//{
//	bool success = dictionary.getValue(constants::renderablestars::datasource::keyFile, _file);
//	if (!success) {
//		LERROR("SpeckDataSource did not contain key '" <<
//			constants::renderablestars::datasource::keyFile << "'");
//		return;
//	}
//	_file = absPath(_file);
//}

bool RenderableStars::loadData() {
	std::string _file = _speckPath;
	std::string cachedFile = "";
	FileSys.cacheManager()->getCachedFile(_file, cachedFile, true);

	bool hasCachedFile = FileSys.fileExists(cachedFile);
	if (hasCachedFile) {
		LINFO("Cached file '" << cachedFile << "' used for Speck file '" << _file << "'");

		bool success = loadCachedFile(cachedFile);
		if (success)
			return true;
		else
			FileSys.cacheManager()->removeCacheFile(_file);
			// Intentional fall-through to the 'else' computation to generate the cache
			// file for the next run
	}
	else {
		LINFO("Cache for Speck file '" << _file << "' not found");
	}
	LINFO("Loading Speck file '" << _file << "'");

	bool success = readSpeckFile();
	if (!success)
		return false;

	LINFO("Saving cache");
	success = saveCachedFile(cachedFile);

	return success;
}

bool RenderableStars::readSpeckFile() {
	std::string _file = _speckPath;
	std::ifstream file(_file);
	if (!file.good()) {
		LERROR("Failed to open Speck file '" << _file << "'");
		return false;
	}

	int nValues = 0;

	// The beginning of the speck file has a header that either contains comments
	// (signaled by a preceding '#') or information about the structure of the file
	// (signaled by the keywords 'datavar', 'texturevar', and 'texture')
	std::string line = "";
	while (true) {
		std::ifstream::streampos position = file.tellg();
		std::getline(file, line);

		if (line[0] == '#')
			continue;

		if (line.substr(0, 7) != "datavar" &&
			line.substr(0, 10) != "texturevar" &&
			line.substr(0, 7) != "texture")
		{
			// we read a line that doesn't belong to the header, so we have to jump back
			// before the beginning of the current line
			file.seekg(position);
			break;
		}

		if (line.substr(0, 7) == "datavar") {
			// datavar lines are structured as follows:
			// datavar # description
			// where # is the index of the data variable; so if we repeatedly overwrite
			// the 'nValues' variable with the latest index, we will end up with the total
			// number of values (+3 since X Y Z are not counted in the Speck file index)
			std::stringstream str(line);

			std::string dummy;
			str >> dummy;
			str >> nValues;
			nValues += 1; // We want the number, but the index is 0 based
		}
	}

	nValues += 3; // X Y Z are not counted in the Speck file indices

	do {
		std::vector<float> values(nValues);

		std::getline(file, line);
		std::stringstream str(line);

		for (int i = 0; i < nValues; ++i)
			str >> values[i];

		// Extract the position (in parsecs)
		psc position = PowerScaledCoordinate::CreatePowerScaledCoordinate(
							values[0],
							values[1],
							values[2]
						);
		
		// Convert parsecs -> meter
		PowerScaledScalar parsecsToMetersFactor = PowerScaledScalar(0.308567758f, 17.f);
		position[0] *= parsecsToMetersFactor[0];
		position[1] *= parsecsToMetersFactor[0];
		position[2] *= parsecsToMetersFactor[0];
		position[3] += parsecsToMetersFactor[1];

		// Push the position into the data array
		_data.push_back(position[0]);
		_data.push_back(position[1]);
		_data.push_back(position[2]);
		_data.push_back(position[3]);

		// Push the other values into the array
		_data.push_back(values[3]); // colorb_v
		_data.push_back(values[4]); // luminance
		_data.push_back(values[5]); // absolute magnitude
	} while (!file.eof());

	return true;
}

bool RenderableStars::loadCachedFile(const std::string& file) {
	std::ifstream fileStream(file, std::ifstream::binary);
	if (fileStream.good()) {
		int32_t nValues = 0;
		fileStream.read(reinterpret_cast<char*>(&nValues), sizeof(int32_t));

		_data.resize(nValues);
		fileStream.read(reinterpret_cast<char*>(&_data[0]), nValues * sizeof(_data[0]));

		bool success = fileStream.good();
		return success;
	}
	else {
		LERROR("Error opening file '" << file << "' for loading cache file");
		return false;
	}
}

bool RenderableStars::saveCachedFile(const std::string& file) const {
	std::ofstream fileStream(file, std::ofstream::binary);
	if (fileStream.good()) {
		int32_t nValues = static_cast<int32_t>(_data.size());
		if (nValues == 0) {
			LERROR("Error writing cache: No values were loaded");
			return false;
		}
		fileStream.write(reinterpret_cast<const char*>(&nValues), sizeof(int32_t));

		size_t nBytes = nValues * sizeof(_data[0]);
		fileStream.write(reinterpret_cast<const char*>(&_data[0]), nBytes);

		bool success = fileStream.good();
		return success;
	}
	else {
		LERROR("Error opening file '" << file << "' for save cache file");
		return false;
	}
}

}