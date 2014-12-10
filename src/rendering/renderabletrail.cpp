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
#include <openspace/rendering/renderabletrail.h>

#include <sgct.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/constants.h>

#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/filesystem/filesystem.h>

#include <openspace/util/spicemanager.h>
#include <iomanip>
#include <utility>   
/* TODO for this class:
*  In order to add geometry shader (for pretty-draw),
*  need to pack each consecutive point pair into a vec2
*  in order to draw quad between them. 
*/

namespace {
	const std::string _loggerCat = "RenderableTrail";
	//constants
		const std::string keyBody                = "Body";
		const std::string keyObserver            = "Observer";
		const std::string keyFrame               = "Frame";
		const std::string keyPathModule          = "ModulePath";
		const std::string keyColor               = "RGB";
		const std::string keyTropicalOrbitPeriod = "TropicalOrbitPeriod";
		const std::string keyEarthOrbitRatio     = "EarthOrbitRatio";
		const std::string keyDayLength           = "DayLength";

}
//#define DEBUG
namespace openspace{
	RenderableTrail::RenderableTrail(const ghoul::Dictionary& dictionary)
		: Renderable(dictionary)
		, _colorTexturePath("colorTexture", "Color Texture")
		, _programObject(nullptr)
		, _texture(nullptr)
		, _vaoID(0)
		, _vBufferID(0)
		, _iBufferID(0)
		, _mode(GL_LINE_STRIP){

		bool b1 = dictionary.getValue(keyBody, _target);
		bool b2 = dictionary.getValue(keyObserver, _observer);
		bool b3 = dictionary.getValue(keyFrame, _frame);
		bool b4 = dictionary.getValue(keyTropicalOrbitPeriod, _tropic);
		bool b5 = dictionary.getValue(keyEarthOrbitRatio, _ratio);
		bool b6 = dictionary.getValue(keyDayLength, _day);

		assert(b1 == true);
		assert(b2 == true);
		assert(b3 == true);
		assert(b4 == true);
		assert(b5 == true);
		assert(b6 == true);

		//assert(dictionary.getValue(keyBody               , _target));
		//assert(dictionary.getValue(keyObserver, _observer));
		//assert(dictionary.getValue(keyFrame              , _frame));
		//assert(dictionary.getValue(keyTropicalOrbitPeriod, _tropic));
		//assert(dictionary.getValue(keyEarthOrbitRatio    , _ratio));
		//assert(dictionary.getValue(keyDayLength          , _day));//not used now, will be though.
		// values in modfiles set from here
		// http://nssdc.gsfc.nasa.gov/planetary/factsheet/marsfact.html


	//white is default col
	if (!dictionary.getValue(keyColor, _c)){
		_c = glm::vec3(0.0);
	}else{ //to understand, ref to line 115.  
		_r = 1 / _c[0];
		_g = 1 / _c[1];
		_b = 1 / _c[2];
	}
}
	
/* This algorithm estimates and precomputes the number of segments required for
*  any planetary object in space, given a tropical orbit period and earth-to-planet 
*  orbit ratio. In doing so, it finds the exact increment of time corresponding
*  to a planetary year. 
*  Therefore all planets need said constants, for other objects we need a different,
*  and most likely heuristic measure to easily estimate a nodal time-increment.
*  Trivial, yet - a TODO. 
*/
void RenderableTrail::fullYearSweep(){
	double lightTime = 0.0;
	double et = _startTrail;
	double planetYear = 31540000 * _ratio;
	int segments = _tropic;

	_increment = planetYear / _tropic;
	
	_isize = (segments + 2);
	_vsize = (segments + 2);
	_iarray = new int[_isize];
	
	for (int i = 0; i < segments+2; i++){
		SpiceManager::ref().getTargetState(_target, _observer, _frame, "LT+S", et, _pscpos, _pscvel, lightTime);
		_pscpos[3] += 3;

		for (int k = 0; k < 4; k++)
		_varray.push_back(_pscpos[k]);

#ifndef DEBUG
		float p = (float)i / _tropic;
		_varray.push_back(1.f - p * _r);
		_varray.push_back(1.f - p * _g);
		_varray.push_back(1.f - p * _b);
		_varray.push_back(1.f - p);
#else
		_varray.push_back(1.f );
		_varray.push_back(1.f );
		_varray.push_back(1.f );
		_varray.push_back(1.f );
#endif
 		_iarray[i] = i;
		if (i != 0) //very first point needs to be alllocated twice.
		et -= _increment;
	}
	_stride = 8;
	_vsize = _varray.size();
	_vtotal = static_cast<int>(_vsize / _stride);
}

RenderableTrail::~RenderableTrail(){
	deinitialize();
}

bool RenderableTrail::isReady() const {
	return _programObject != nullptr;
}

void RenderableTrail::sendToGPU(){
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
}


bool RenderableTrail::initialize(){
	bool completeSuccess = true;
	if (_programObject == nullptr)
		completeSuccess
		&= OsEng.ref().configurationManager().getValue("EphemerisProgram", _programObject);
	
	//TEXTURES DISABLED FOR NOW
	//loadTexture();
	completeSuccess &= (_texture != nullptr);

	// SpiceManager::ref().getETfromDate("2006 Aug 22 17:00:00", _startTrail);
	SpiceManager::ref().getETfromDate("2007 feb 26 17:30:00", _startTrail);
	_dtEt = _startTrail;

	fullYearSweep();
	sendToGPU();

	return completeSuccess;
}

bool RenderableTrail::deinitialize(){
	delete _texture;
	_texture = nullptr;
	return true;
}

// Tried interpolation but then realised this still gives straight lines (latenight thing).
// Not allowed Splines so therefore - query spice for each point (bah...) 
// From psc paper:
/*
psc pscInterpolate(psc p0, psc p1, float t){
	assert(t >= 0 && t <= 1);

	float s = (1.f - t)*p0[3] + t*p1[3];

	float x = ((1.f - t)*p0[0] + t*p1[0]);
	float y = ((1.f - t)*p0[1] + t*p1[1]);
	float z = ((1.f - t)*p0[2] + t*p1[2]);

	return PowerScaledCoordinate::PowerScaledCoordinate(x,y,z,s);
}
*/

void RenderableTrail::updateTrail(){
	int m = _stride;
	float *begin = &_varray[0];
	//float *end = &_varray[_vsize - 1] + 1;

	// update only when time progresses
	if (_oldTime != _time){
		// if time progressed more than N _increments 
		while (_dtEt < _time){
			// get intermediary points
			psc dtPoint;
			SpiceManager::ref().getTargetState(_target, _observer, _frame, "NONE", _dtEt, dtPoint, _pscvel, lightTime);
			dtPoint[3] += 3;
			
			// overwrite the old position
			memcpy(begin, glm::value_ptr(dtPoint.vec4()), 4 * sizeof(float));

			// shift array
			for (int k = _vsize-m; k > 0; k -= m){
				memcpy(&_varray[k], &_varray[k - m], 4 * sizeof(float));
			}
			// keep track of progression
			_dtEt += _increment;
		}
		//add earths current position
		memcpy(&_varray[0], glm::value_ptr(_pscpos.vec4()), 4 * sizeof(float));
		_varray[4] = 1.f;
		_varray[5] = 1.f;
		_varray[6] = 1.f;
		_varray[7] = 1.f;

	}_oldTime = _time;

	// update GPU
	// NOTE: vbo interleaved, makes possible color update more efficient - tightly packed.
	// if NO color update : would be more efficient to have these as separate 
	// => N/2 updates per drawcall.
	glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
	glBufferSubData(GL_ARRAY_BUFFER, 0, _vsize * sizeof(GLfloat), &_varray[0]);
}

void RenderableTrail::render(const RenderData& data){
	assert(_programObject);
	_programObject->activate();

	// fetch data
	psc currentPosition = data.position;
	psc campos          = data.camera.position();
	glm::mat4 camrot    = data.camera.viewRotationMatrix();

	glm::mat4 transform = glm::mat4(1);

	// setup the data to the shader
	//_programObject->setUniform("objectVelocity", pscvel.vec4());
	_programObject->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
	_programObject->setUniform("ModelTransform", transform);
	setPscUniforms(_programObject, &data.camera, data.position);

	updateTrail();
	
	glBindVertexArray(_vaoID); 
	glDrawArrays(_mode, 0, _vtotal);
	glBindVertexArray(0);
	
	glPointSize(2.f);
	// nodes of equal time
	glBindVertexArray(_vaoID);
	glDrawArrays(GL_POINTS, 0, _vtotal);
	glBindVertexArray(0);
	
	_programObject->deactivate();
}

void RenderableTrail::update(const UpdateData& data){
	_time  = data.time;
	_delta = data.delta;
	
	SpiceManager::ref().getTargetState(_target, _observer, _frame, "NONE", data.time, _pscpos, _pscvel, lightTime);
	_pscpos[3] += 3; // KM to M
	
}

void RenderableTrail::loadTexture()
{
	delete _texture;
	_texture = nullptr;
	if (_colorTexturePath.value() != "") {
		_texture = ghoul::io::TextureReader::ref().loadTexture(absPath(_colorTexturePath));
		if (_texture) {
			LDEBUG("Loaded texture from '" << absPath(_colorTexturePath) << "'");
			_texture->uploadTexture();
		}
	}
}

}