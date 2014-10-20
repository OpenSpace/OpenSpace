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
		, _colorTexturePath("colorTexture", "Color Texture")
		, _programObject(nullptr)
		, _texture(nullptr)
		, _vaoID(0)
		, _vBufferID(0)
		, _iBufferID(0)
		, _mode(GL_LINE_STRIP){

		assert(dictionary.getValue(keyBody, _target));
		assert(dictionary.getValue(keyObserver, _observer));
		assert(dictionary.getValue(keyFrame, _frame));
		/*assert(dictionary.getValue(keyTropicalOrbitPeriod, _tropic));
		assert(dictionary.getValue(keyEarthOrbitRatio, _ratio));
		assert(dictionary.getValue(keyDayLength, _day));//not used now, will be though.
		// values in modfiles set from here*/
		// http://nssdc.gsfc.nasa.gov/planetary/factsheet/marsfact.html

		//white is default col
		if (!dictionary.getValue(keyColor, _c)){
			_c = glm::vec3(0.0);
		}
		else{
			_r = 1 / _c[0];
			_g = 1 / _c[1];
			_b = 1 / _c[2];
		}
}
void RenderablePath::fullYearSweep(){
		double lightTime = 0.0;
		SpiceManager::ref().getETfromDate("2006 jan 20 19:00:00", _time);

		// -------------------------------------- ^ this has to be simulation start-time, not passed in here though --
		double et2 = 0;
		//SpiceManager::ref().getETfromDate("2008 apr 01 00:00:00", et2);
		//psc nhpos, nhvel;
		//SpiceManager::ref().getTargetState("NEW HORIZONS", "SUN", "J2000", "LT+S", et2, _pscpos, _pscvel, lightTime);

		double et = _time;
		int segments = 200000;
		_increment = 86400;

		_isize = (segments + 2);
		_vsize = (segments + 2);
		_iarray = new int[_isize];

		int indx = 0;
		for (int i = 0; i < segments + 1; i++){
			SpiceManager::ref().getTargetState(_target, _observer, _frame, "LT+S", et, _pscpos, _pscvel, lightTime);
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
				_iarray[indx] = indx;
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
	}

	RenderablePath::~RenderablePath(){
		deinitialize();
	}

	bool RenderablePath::initialize(){
		bool completeSuccess = true;
		if (_programObject == nullptr)
			completeSuccess
			&= OsEng.ref().configurationManager().getValue("EphemerisProgram", _programObject);

		//TEXTURES DISABLED FOR NOW
		//loadTexture();
		completeSuccess &= (_texture != nullptr);

		fullYearSweep();

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
		setPscUniforms(_programObject, &data.camera, data.position);

/*		glBindVertexArray(_vaoID);
		glDrawArrays(_mode, 0, _vtotal);
		glBindVertexArray(0);
	*/	
		glPointSize(2.f);

		glBindVertexArray(_vaoID);
		glDrawArrays(GL_POINTS, 0, _vtotal);
		glBindVertexArray(0);
		
		_programObject->deactivate();
	}

	void RenderablePath::update(const UpdateData& data){
		double lightTime;
		_time = data.time;
		_delta = data.delta;
		int newhorizons = 0;

		SpiceManager::ref().getTargetState(_target, _observer, _frame, "LT+S", data.time, _pscpos, _pscvel, lightTime);
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