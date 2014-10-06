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
#include <openspace/rendering/renderablepath.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/constants.h>

#include <ghoul/opengl/texturereader.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/filesystem/filesystem.h>

#include <openspace/util/spicemanager.h>
#include <iomanip>
#include <utility>      // std::move

namespace {
	const std::string _loggerCat = "RenderablePath";
}

namespace openspace{
	RenderablePath::RenderablePath(const ghoul::Dictionary& dictionary)
	: Renderable(dictionary)
	, _colorTexturePath("colorTexture", "Color Texture")
	, _programObject(nullptr)
	, _texture(nullptr)
	, _vaoID(0)
	, _vBufferID(0)
	, _iBufferID(0)
	, _mode(GL_LINE_STRIP){


	double lightTime = 0.0;
	double planetYear = 31536000;
	SpiceManager::ref().getETfromDate("2005 jan 09 00:00:00", _time);
	// -------------------------------------- ^ this has to be simulation start-time, not passed in here though --

	double et = _time - planetYear;

	int segments = 40; // note to self: code not look nice. cleanup for clarity later.
	psc pscpos, pscvel;

	_isize = (segments+2);
	_vsize = (segments+2);
	//_varray = new float[_vsize];
	_iarray = new int[_isize];

	_updated = new bool[_vsize];
	std::fill(_updated, _updated + _vsize, false);

	//static_assert(sizeof(Vertex) == 64, "The size of the Vertex needs to be 64 for performance");
	
	_increment = planetYear / segments;
	for (int i = 0; i < segments+1; i++){
		
		SpiceManager::ref().getTargetState("EARTH", "SUN", "GALACTIC", "LT+S", et, pscpos, pscvel, lightTime);
		
		psc tmppos = glm::vec4(i, i, i, 7);
		_varray.push_back(tmppos[0]);
		_varray.push_back(tmppos[1]);
		_varray.push_back(tmppos[2]);
		_varray.push_back(tmppos[3]);

		//memcpy(_varray[i].location, glm::value_ptr(pscpos.vec4()), 4 * sizeof(double));
		glm::vec4 color = glm::vec4(1, (i % 2 == 0), 1, 1);
		
		_varray.push_back(color[0]);
		_varray.push_back(color[1]);
		_varray.push_back(color[2]);
		_varray.push_back(color[3]);

		_intervals.push_back(std::pair<int, double>(i, et));

		_iarray[i] = i; // remove indx in this class at some point!
		et += _increment;
	}	
	_stride = 8;
	_vsize = _varray.size();
	_vtotal = static_cast<int>(_vsize / _stride);
	/*
	std::cout << "before : " << std::endl;
	for (int i = 0; i < _vsize-1; i++){
		std::cout << _varray[i] << std::endl;
	}
	
	
	/// how to std::move()
	int m = 8;
	for (int i = _vsize-1; i+1-m != 0; i--){
		
	std:: cout << i << "  " << i-m << std::endl;
	_varray[i] = std::move(_varray[i - m]);
	}
	/*
	std::cout << "after : " << std::endl;
	for (int i = 0; i < _vsize - 1; i++){
		std::cout << _varray[i] << std::endl;
	}
	*/
}

RenderablePath::~RenderablePath(){
	deinitialize();
}

	bool RenderablePath::initialize(){
	bool completeSuccess = true;
	if (_programObject == nullptr)
		completeSuccess
		&= OsEng.ref().configurationManager().getValue("EphemerisProgram", _programObject);

	loadTexture();
	completeSuccess &= (_texture != nullptr);
	
	// Initialize and upload to graphics card
	glGenVertexArrays(1, &_vaoID);
	glGenBuffers(1, &_vBufferID);
	glGenBuffers(1, &_iBufferID);

	glBindVertexArray(_vaoID);
	
	glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
	glBufferData(GL_ARRAY_BUFFER, _vsize * sizeof(GLfloat), NULL, GL_STREAM_DRAW); // orphaning the buffer, sending NULL data.
	glBufferSubData(GL_ARRAY_BUFFER, 0, _vsize * sizeof(GLfloat), &_varray[0]);
	
	GLsizei st = sizeof(GLfloat) * _stride;


	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, st, (void*)0);
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, st, (void*)(4 * sizeof(GLfloat)));


	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iBufferID);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, _isize * sizeof(int), _iarray, GL_STATIC_DRAW);

	glBindVertexArray(0);
	
	return completeSuccess;
}

bool RenderablePath::deinitialize(){
	delete _texture;
	_texture = nullptr;
	return true;
}

void RenderablePath::render(const RenderData& data){
	assert(_programObject);
	_programObject->activate();

	// fetch data
	psc currentPosition = data.position;
	psc campos = data.camera.position();
	glm::mat4 camrot = data.camera.viewRotationMatrix();
	// PowerScaledScalar scaling = camera->scaling();
	PowerScaledScalar scaling = glm::vec2(1, -6);

	glm::mat4 transform = glm::mat4(1);

	// setup the data to the shader
	//_programObject->setUniform("objectVelocity", pscvel.vec4());

	_programObject->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
	_programObject->setUniform("ModelTransform", transform);
	_programObject->setUniform("campos", campos.vec4());
	_programObject->setUniform("objpos", currentPosition.vec4());
	_programObject->setUniform("camrot", camrot);
	_programObject->setUniform("scaling", scaling.vec2());
	
	if (_oldTime != _time){
		int m = 8;
		for (int i = _vsize - 1; i + 1 - m != 0; i--){
			_varray[i] = std::move(_varray[i - m]);
		}
		memcpy(&_varray[0], glm::value_ptr(_pscpos.vec4()), 4 * sizeof(double));
		_varray[4] = 1;
		_varray[5] = 1;
		_varray[6] = 1;
		_varray[7] = 1;
	}_oldTime = _time;


	glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
	glBufferSubData(GL_ARRAY_BUFFER, 0, _vsize * sizeof(GLfloat), &_varray[0]);
	

	glBindVertexArray(_vaoID); 
	glDrawArrays(_mode, 0, _vtotal);
	glBindVertexArray(0);

	glPointSize(2.f);

	glBindVertexArray(_vaoID);
	glDrawArrays(GL_POINTS, 0, _vtotal);
	glBindVertexArray(0);

	_programObject->deactivate();
}

void RenderablePath::update(const UpdateData& data){
	double lightTime;
	_time = data.time;

	SpiceManager::ref().getTargetState("EARTH", "SUN", "GALACTIC", "LT+S", data.time, _pscpos, _pscvel, lightTime);
}

void RenderablePath::loadTexture()
{
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

}