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

#include <ghoul/filesystem/filesystem.h>
#include <openspace/engine/openspaceengine.h>
#include <sgct.h>

namespace {
	const std::string _loggerCat = "RenderableStars";
}

namespace openspace {
RenderableStars::RenderableStars(const ghoul::Dictionary& dictionary)
	: Renderable(dictionary),
	_programObject(nullptr){

	std::string path;
	dictionary.getValue(constants::renderablestars::keySpeckFile, path);
	_speckPath = absPath(path);
}

RenderableStars::~RenderableStars(){
	deinitialize();
}

GLuint createVBO(const void* data, int dataSize, GLenum target, GLenum usage){
	GLuint id = 0;  // 0 is reserved, glGenBuffersARB() will return non-zero id if success
	glGenBuffers(1, &id);                        // create a vbo
	glBindBuffer(target, id);                    // activate vbo id to use
	glBufferData(target, dataSize, data, usage); // upload data to video card

	// check data size in VBO is same as input array, if not return 0 and delete VBO
	int bufferSize = 0;
	glGetBufferParameteriv(target, GL_BUFFER_SIZE, &bufferSize);
	if (dataSize != bufferSize)
	{
		glDeleteBuffers(1, &id);
		id = 0;
		printf("[createVBO()] Data size is mismatch with input array\n");
	}
	glBindBuffer(target, 0);
	return id;      // return VBO id
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
	std::string str, name, datastr;
	std::vector<std::string> strvec;
	std::vector<double> positions;
	std::vector<double> doubleData;
    int count = 0;

	file.open(path);

	if (!file.is_open()){
		LERROR("Failed to open spec file for : '" << path << "'");
		return false;
	}
	// count metadata lines.
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
				name = str.substr(mid, end);
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

		// convert to powerscaled coordinate
		psc powerscaled = PowerScaledCoordinate::CreatePowerScaledCoordinate(doubleData[0],
			                                                                 doubleData[1], 
														                     doubleData[2]);
		for ( int i = 0; i < 4; i++ ) 
			positions.push_back(powerscaled[i]);
		strvec.clear();
		doubleData.clear();
		count++;
	} while (file.good());

	// pass in the vectors internal array to create vbo method
	// v_size = positions.size();
	//_starPositionsVBO = createVBO(&positions[0], v_size*sizeof(GLdouble), GL_ARRAY_BUFFER, GL_DYNAMIC_DRAW);

	return true;
}

bool RenderableStars::initialize(){
	bool completeSuccess = true;
	if (_programObject == nullptr)
		completeSuccess &= OsEng.ref().configurationManager().getValue("pscShader", _programObject);
	
	if (!readSpeckFile(_speckPath)) 
		LERROR("Failed to read speck file for path : '" << _speckPath << "'");

	/*
	loadTexture();
	completeSuccess &= (_texture != nullptr);

	completeSuccess &= _geometry->initialize(this);
	*/
	return completeSuccess;
}

bool RenderableStars::deinitialize(){
	// TODO: set private VBO and deinitialize here.. 
	/*_geometry->deinitialize();
	delete _geometry;
	_geometry = nullptr;
	delete _texture;
	_texture = nullptr;*/
	glDeleteBuffers(1, &_starPositionsVBO);
	return true;
}

void RenderableStars::render(const Camera* camera, const psc& thisPosition){
	assert(_programObject);

	// activate shader
	_programObject->activate();

	psc currentPosition = thisPosition;
	psc campos = camera->position();
	glm::mat4 camrot = camera->viewRotationMatrix();
	PowerScaledScalar scaling = camera->scaling();

	glm::mat4 transform = glm::mat4(1);
	_programObject->setUniform("ViewProjection", camera->viewProjectionMatrix());
	_programObject->setUniform("ModelTransform", transform);
	_programObject->setUniform("campos", campos.vec4());
	_programObject->setUniform("objpos", currentPosition.vec4());
	_programObject->setUniform("camrot", camrot);
	_programObject->setUniform("scaling", scaling.vec2());
	
	glColor4f(1.f, 1.f, 1.f, 1.f);
	glBindBuffer(GL_ARRAY_BUFFER, _starPositionsVBO);
	glVertexPointer(4, GL_FLOAT, 0, 0);
	glEnableClientState(GL_VERTEX_ARRAY);

	glDrawArrays(GL_POINTS, 0, 4*v_size);
	glDisableClientState(GL_VERTEX_ARRAY);
	glBindBuffer(GL_ARRAY_BUFFER, 0);


	/*
	// REQUIRES OWN SHADER. 
	GLint vertexLoc = _programObject->attributeLocation("in_position");
	glEnableVertexAttribArray(vertexLoc);
	glBindBuffer(GL_ARRAY_BUFFER, _starPositionsVBO);
	glVertexAttribPointer(vertexLoc,                 // attribute
								  4,                 // size
								  GL_DOUBLE,          // type
								  GL_FALSE,          // normalized?
								  0,                 // stride
								  (void*)0);

	glDrawElements(GL_POINTS,         // mode
		           v_size/4,            // count
				   GL_DOUBLE,   // type
				   (void*)0);         // element array buffer offset
				   */
	_programObject->deactivate();

}

void RenderableStars::update()
{
}

	
}