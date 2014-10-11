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
#include <openspace/rendering/renderableephemeris.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/constants.h>

#include <ghoul/opengl/texturereader.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/filesystem/filesystem.h>

#include <openspace/util/spicemanager.h>
#include <iomanip>
#include <utility>      // std::move


namespace {
	const std::string _loggerCat = "RenderableEphemeris";
}

namespace openspace{
RenderableEphemeris::RenderableEphemeris(const ghoul::Dictionary& dictionary)
	: Renderable(dictionary)
	, _colorTexturePath("colorTexture", "Color Texture")
	, _programObject(nullptr)
	, _texture(nullptr)
	, _vaoID(0)
	, _vBufferID(0)
	, _iBufferID(0)
	, _mode(GL_LINES){


	double lightTime = 0.0;
	double planetYear = 31536000;
	SpiceManager::ref().getETfromDate("2005 nov 01 00:00:00", _time);
	// -------------------------------------- ^ this has to be simulation start-time, not passed in here though --

	double et = _time - planetYear;

	int segments = 365; // note to self: code not look nice. cleanup for clarity later.
	int indx = 0;
	psc pscpos, pscvel;

	_isize = (segments)*2;
	_vsize = (segments)*2;
	_varray = new Vertex[_vsize];
	_iarray = new int[_isize];

	_updated = new bool[_vsize];
	std::fill(_updated, _updated + _vsize, false);

	static_assert(sizeof(Vertex) == 64, "The size of the Vertex needs to be 64 for performance");
	
	// get first position, ephemeris start-point
	SpiceManager::ref().getTargetState("EARTH", "SUN", "GALACTIC", "LT+S", et, pscpos, pscvel, lightTime);

	memcpy(_varray[indx].location, glm::value_ptr(pscpos.vec4()), 4 * sizeof(double));
	memcpy(_varray[indx].velocity, glm::value_ptr(glm::vec4(1, 1, 0, 1)), 4 * sizeof(double));

	_intervals.push_back(std::pair<int, double>(indx, et));

	_iarray[indx] = indx;
	indx++;

	_increment = planetYear / segments;
	for (int i = 0; i < segments; i++){
		et += _increment;
		SpiceManager::ref().getTargetState("EARTH", "SUN", "GALACTIC", "LT+S", et, pscpos, pscvel, lightTime);
		for (int k = 0; k < 2; k++){
			if (i == segments - 1 && k == 1) { // do copy first to last
				break;
			}
			memcpy(_varray[indx].location, glm::value_ptr(pscpos.vec4()), 4 * sizeof(double));
			memcpy(_varray[indx].velocity, glm::value_ptr(glm::vec4(1, 1, 0, 1)), 4 * sizeof(double));

			_intervals.push_back(std::pair<int, double>(indx, et));

			_iarray[indx] = indx;
			_index[k] = indx;
			indx++;
		}
	}
	_delta = _vsize;
	
	/// testing std::move()
	int array1[10] = { 10, 9, 8 , 7, 6, 5, 4, 3, 2 , 1 };

	int size =  sizeof(array1)/(sizeof(int));
	std::cout << "before : ";
	for (int i = 0; i < 10; i++){
		std::cout << array1[i] << " ";
	}
	for (int i = size-1; i != 0; i--){
		array1[i] = std::move(array1[i-1]);
		if (i == 1){
			array1[0] = 11;
		}
	}
	std::cout << "after : ";
	for (int i = 0; i < 10; i++){
		std::cout << array1[i] << " ";
	}
	std::cout << std::endl;
	
}

RenderableEphemeris::~RenderableEphemeris(){
	deinitialize();
}

bool RenderableEphemeris::initialize(){
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
	glBufferData(GL_ARRAY_BUFFER, _vsize * sizeof(Vertex), NULL, GL_STREAM_DRAW); // orphaning the buffer, sending NULL data.
	glBufferSubData(GL_ARRAY_BUFFER, 0, _vsize * sizeof(Vertex), _varray);
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(Vertex), reinterpret_cast<const GLvoid*>(offsetof(Vertex, location)));
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, sizeof(Vertex), reinterpret_cast<const GLvoid*>(offsetof(Vertex, velocity)));


	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iBufferID);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, _isize * sizeof(int), _iarray, GL_STATIC_DRAW);

	glBindVertexArray(0);
	
	return completeSuccess;
}

bool RenderableEphemeris::deinitialize(){
	delete _texture;
	_texture = nullptr;
	return true;
}

void RenderableEphemeris::nextIndex(){

	if (_previousTime != _time){
		int skip = _index[1] - _prev[1];

		_prev[0] = _index[0];
		_prev[1] = _index[1];

		double t = _time - 31536000;
		// Better optimization:

		int x = (_index[0] > _index[1]) ? _index[0] : _index[1];
		if (x >= _vsize - 1 || skip + x > _vsize ) x = 1;

		for (x; x < _vsize; x++){
			double t1 = _intervals[x - 1].second;
			double t2 = _intervals[x].second;

			if (t1 != t2 && t >= t1 && t <= t2){
				_index[0] = (x - 2 < 0) ? _vsize - 1 : x - 2;
				_index[1] = x - 1;
				break;
			}
		}
	}
	_previousTime = _time;
}


void RenderableEphemeris::render(const RenderData& data){
	assert(_programObject);
	_programObject->activate();


	// setup the data to the shader
	//_programObject->setUniform("objectVelocity", pscvel.vec4());

	_programObject->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
	_programObject->setUniform("ModelTransform", glm::mat4(1));
	setPscUniforms(_programObject, &data.camera, data.position);

	nextIndex();
	
	
	for (int i = 0; i < _vsize; i++){
		_varray[i].velocity[0] -= 0.00008;
		_varray[i].velocity[1] -= 0.00006;
		_varray[i].velocity[2] -= 0.00004;
	}
	
	if (_delta > 0){
		int i = _index[0];
		int j = _index[1];

		while (i != _prev[0]){
			if (_updated[i] == false && _updated[j] == false){

				_updated[i] = true;
				_updated[j] = true;

				_intervals[i].second += 31536000; // not the cleanest solution but works. need dt
				_intervals[j].second += 31536000;

				// DEBUGGING COLOR CODING
				memcpy(_varray[i].velocity, glm::value_ptr(glm::vec4(0, 0, 1, 1)), 4 * sizeof(double));    // blue if updated
				memcpy(_varray[j].velocity, glm::value_ptr(glm::vec4(0, 0, 1, 1)), 4 * sizeof(double));

			}
			i = (i - 2 < 0) ? _vsize - 1 : i - 2;
			j = (j - 1 < 0) ? _vsize - 1 : j - 2;

			std::cout << i << " " << j << std::endl;
		}
	}
	/*
	if (_updated[_index[1]] == false && _updated[_index[0]] == false){
		
		_updated[_index[0]] = true;
		_updated[_index[1]] = true;
			
		_intervals[_index[0]].second = _time;
		_intervals[_index[1]].second = _time;
			
		memcpy(_varray[_index[0]].location, glm::value_ptr(_pscpos.vec4()), 4 * sizeof(double));
		memcpy(_varray[_index[1]].location, glm::value_ptr(_pscpos.vec4()), 4 * sizeof(double));
		
	//	memcpy(_varray[_index[0]].velocity, glm::value_ptr(glm::vec4(1, 1, 1, 1)), 4 * sizeof(double));
	//	memcpy(_varray[_index[1]].velocity, glm::value_ptr(glm::vec4(1, 1, 1, 1)), 4 * sizeof(double));
		
		// DEBUGGING COLOR CODING
		
		memcpy(_varray[_index[0]].velocity, glm::value_ptr(glm::vec4(0, 0, 1, 1)), 4 * sizeof(double));    // blue if updated
		memcpy(_varray[_index[1]].velocity, glm::value_ptr(glm::vec4(0, 0, 1, 1)), 4 * sizeof(double));
			
		memcpy(_varray[_index[2]].velocity, glm::value_ptr(glm::vec4(1, 0, 0, 1)), 4 * sizeof(double));    // red
		memcpy(_varray[_index[3]].velocity, glm::value_ptr(glm::vec4(1, 0, 0, 1)), 4 * sizeof(double));
			
		_updated[_index[2]] = false;
		_updated[_index[3]] = false;
	}
	*/


	glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
	glBufferSubData(GL_ARRAY_BUFFER, 0, _vsize * sizeof(Vertex), _varray);
	

	//psc pointPos(_varray[_index].location[0], _varray[_index].location[1], _varray[_index].location[2], _varray[_index].location[3]);
	//std::cout << "dist, indx : " << distance(_pscpos, pointPos) << ", " << _index << std::endl;


	glBindVertexArray(_vaoID); 
	glDrawArrays(_mode, 0, GL_UNSIGNED_INT);
	glBindVertexArray(0);

	glPointSize(2.f);

	glBindVertexArray(_vaoID);
	glDrawArrays(GL_POINTS, 0, GL_UNSIGNED_INT);
	glBindVertexArray(0);

	_programObject->deactivate();
}

void RenderableEphemeris::update(const UpdateData& data){
	double lightTime;
	_time = data.time;

	SpiceManager::ref().getTargetState("EARTH", "SUN", "GALACTIC", "LT+S", data.time, _pscpos, _pscvel, lightTime);
}

void RenderableEphemeris::loadTexture()
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