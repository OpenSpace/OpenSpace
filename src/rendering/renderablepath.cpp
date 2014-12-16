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

#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/filesystem/filesystem.h>

#include <openspace/util/spicemanager.h>
#include <iomanip>
#include <utility>      
namespace {
	const std::string _loggerCat = "RenderablePath";
	//constants
	const std::string keyBody = "Body";
	const std::string keyObserver = "Observer";
	const std::string keyFrame = "Frame";
	const std::string keyPathModule = "ModulePath";
	const std::string keyColor = "RGB";


}
#define DEBUG
namespace openspace{
RenderablePath::RenderablePath(const ghoul::Dictionary& dictionary)
	: Renderable(dictionary)
	, _programObject(nullptr)
	, _vaoID(0)
	, _vBufferID(0)
	, _iBufferID(0)
{

	dictionary.getValue(keyBody, _target);
	dictionary.getValue(keyObserver, _observer);
	dictionary.getValue(keyFrame, _frame);

	// not used now, will be though.
	// dictionary.getValue(keyTropicalOrbitPeriod, _tropic);
	// dictionary.getValue(keyEarthOrbitRatio, _ratio);
	// dictionary.getValue(keyDayLength, _day);

	// values in modfiles set from here
	// http://nssdc.gsfc.nasa.gov/planetary/factsheet/marsfact.html

	// white is default col
	if (!dictionary.getValue(keyColor, _c)){
		_c = glm::vec3(0.0);
	}
	else{
		_r = 1 / _c[0];
		_g = 1 / _c[1];
		_b = 1 / _c[2];
	}
}
bool RenderablePath::fullYearSweep(){
	double lightTime = 0.0;
	SpiceManager::ref().getETfromDate("2006 jan 20 19:00:00", _time);

	std::cout << _time << std::endl;

	// -------------------------------------- ^ this has to be simulation start-time, not passed in here though --
	//SpiceManager::ref().getETfromDate("2008 apr 01 00:00:00", et2);
	//psc nhpos, nhvel;
	//SpiceManager::ref().getTargetState("NEW HORIZONS", "SUN", "J2000", "LT+S", et2, _pscpos, _pscvel, lightTime);

	double et = _time;
	int segments = 200000;
	_increment = 86400;

	_isize = (segments + 2);
	_vsize = (segments + 2);
	_iarray.clear();

	int indx = 0;
	for (int i = 0; i < segments + 1; i++){
		std::cout << i << std::endl;
		bool gotData = SpiceManager::ref().getTargetPosition(_target, _observer, _frame, "LT+S", et, _pscpos, lightTime);

#ifndef NDEBUG
		if (!gotData) {
			LERROR("Could not fetch data from spice!");
			return false;
		}
#endif

		if (_pscpos[0] != 0 && _pscpos[1] != 0 && _pscpos[2] != 0 && _pscpos[3] != 1){
			_pscpos[3] += 3;
			_varray.push_back(_pscpos[0]);
			_varray.push_back(_pscpos[1]);
			_varray.push_back(_pscpos[2]);
			_varray.push_back(_pscpos[3]);

#ifndef DEBUG
		_varray.push_back(1.f - ((double)i / _tropic * _r));
		_varray.push_back(1.f - ((double)i / _tropic * _g));
		_varray.push_back(1.f - ((double)i / _tropic * _b));
		_varray.push_back(1.f - ((double)i / _tropic));
#else
			_varray.push_back(1.f);
			_varray.push_back(1.f);
			_varray.push_back(1.f);
			_varray.push_back(0.5f);
#endif	
			indx++;
			_iarray.push_back(indx);
		}
		else{
			std::string date;
			SpiceManager::ref().getDateFromET(et, date);
			std::cout << "STOPPED AT: " << date << std::endl;
			break;
		}
		et += _increment;
	}
	_stride = 8;
	_vsize = _varray.size();
	_vtotal = static_cast<int>(_vsize / _stride);
	return true;
}

RenderablePath::~RenderablePath(){
}

bool RenderablePath::isReady() const {
	bool ready = true;
	ready &= (_programObject != nullptr);
	return ready;
}


bool RenderablePath::initialize(){

	if (_target.empty() || _observer.empty() || _frame.empty()) {
		LERROR("The following keys need to be set in the Dictionary. Cannot initialize!");
		LERROR(keyBody << ": " << _target);
		LERROR(keyObserver << ": " << _observer);
		LERROR(keyFrame << ": " << _frame);
		return false;
	}
	// Does checking if can fetch spice data (debug mode only)
	if (!fullYearSweep()) 
		return false;

	// If the programobject is fetched after the string checking, then 
	// the isReady function will properly reflect the state of this object
	// -- jonasstrandstedt
	bool completeSuccess = true;
	if (_programObject == nullptr)
		completeSuccess
		&= OsEng.ref().configurationManager().getValue("EphemerisProgram", _programObject);

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
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, _isize * sizeof(int), _iarray.data(), GL_STATIC_DRAW);

	glBindVertexArray(0);

	return completeSuccess;
}

bool RenderablePath::deinitialize(){
	glDeleteVertexArrays(1, &_vaoID);
	glDeleteBuffers(1, &_vBufferID);
	glDeleteBuffers(1, &_iBufferID);
	return true;
}

void RenderablePath::render(const RenderData& data){
	_programObject->activate();

	// fetch data
	psc currentPosition = data.position;
	psc campos = data.camera.position();
	glm::mat4 camrot = data.camera.viewRotationMatrix();
	// PowerScaledScalar scaling = camera->scaling();
	//PowerScaledScalar scaling = glm::vec2(1, -6);

	glm::mat4 transform = glm::mat4(1);

	// setup the data to the shader
	//_programObject->setUniform("objectVelocity", pscvel.vec4());
	_programObject->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
	_programObject->setUniform("ModelTransform", transform);
	setPscUniforms(_programObject, &data.camera, data.position);

/*	glBindVertexArray(_vaoID);
	glDrawArrays(GL_LINE_STRIP, 0, _vtotal);
	glBindVertexArray(0);
*/	
	glPointSize(2.f);

	glBindVertexArray(_vaoID);
	glDrawArrays(GL_POINTS, 0, _vtotal);
	glBindVertexArray(0);
	
	_programObject->deactivate();
}

void RenderablePath::update(const UpdateData& data){
#ifndef NDEBUG
	if (_target.empty() || _observer.empty() || _frame.empty())
		return;
#endif
	double lightTime;

	_time = data.time;
	_delta = data.delta;

	SpiceManager::ref().getTargetState(_target, _observer, _frame, "LT+S", data.time, _pscpos, _pscvel, lightTime);
}


}