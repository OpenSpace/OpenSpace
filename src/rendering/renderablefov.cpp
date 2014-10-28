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
#include <openspace/rendering/renderablefov.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/constants.h>

#include <ghoul/opengl/texturereader.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/filesystem/filesystem.h>

#include <openspace/util/spicemanager.h>
#include <iomanip>
#include <utility>      
namespace {
	const std::string _loggerCat = "RenderableFov";
	//constants
		const std::string keyBody                = "Body";
		const std::string keyObserver            = "Observer";
		const std::string keyFrame               = "Frame";
		const std::string keyPathModule          = "ModulePath";
		const std::string keyColor               = "RGB";

}
//#define DEBUG
namespace openspace{
	RenderableFov::RenderableFov(const ghoul::Dictionary& dictionary)
		: Renderable(dictionary)
		, _colorTexturePath("colorTexture", "Color Texture")
		, _programObject(nullptr)
		, _texture(nullptr)
		, _vaoID(0)
		, _vBufferID(0)
		, _iBufferID(0)
		, _mode(GL_LINES){

		assert(dictionary.getValue(keyBody               , _target));
		assert(dictionary.getValue(keyObserver           , _observer));
		assert(dictionary.getValue(keyFrame              , _frame));
}
void RenderableFov::fullYearSweep(){
	
	int points = 8;
	_stride = 8;
	_isize = points;
	_iarray = new int[_isize];

	for (int i = 0; i < points; i++){
		for (int j = 0; j < 4; j++){
			_varray.push_back(0); // pos
		}
		for (int j = 0; j < 4; j++){
			_varray.push_back(0); // col
		}
		_iarray[i] = i;
	}

	_stride = 8;
	_vsize = _varray.size();
	_vtotal = static_cast<int>(_vsize / _stride);
}

RenderableFov::~RenderableFov(){
	deinitialize();
}

void RenderableFov::sendToGPU(){
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


bool RenderableFov::initialize(){
	bool completeSuccess = true;
	if (_programObject == nullptr)
		completeSuccess &= OsEng.ref().configurationManager().getValue("EphemerisProgram", _programObject);
	
	 fullYearSweep();
	 sendToGPU();

	return completeSuccess;
}

bool RenderableFov::deinitialize(){
	delete _texture;
	_texture = nullptr;
	return true;
}

void RenderableFov::updateData(){
	glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
	glBufferSubData(GL_ARRAY_BUFFER, 0, _vsize * sizeof(GLfloat), &_varray[0]);
}

void RenderableFov::render(const RenderData& data){
	assert(_programObject);
	_programObject->activate();

	// fetch data
	glm::mat4 transform(1);

	glm::mat4 tmp = glm::mat4(1);
	glm::mat4 roty = glm::rotate(transform, 90.f, glm::vec3(0, 1, 0));
	glm::mat4 rotx = glm::rotate(transform, -90.f, glm::vec3(1, 0, 0));

	glm::mat4 scale = glm::scale(transform, glm::vec3(1, -1, 1));

	for (int i = 0; i < 3; i++){
		for (int j = 0; j < 3; j++){
			tmp[i][j] = _stateMatrix[i][j];
		}
	}

	// setup the data to the shader
	_programObject->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
	_programObject->setUniform("ModelTransform", transform);
	setPscUniforms(_programObject, &data.camera, data.position);

	if (_oldTime != _time){
		//boresight vector
		std::string shape, instrument;
		std::vector<glm::dvec3> bounds;
		glm::dvec3 boresight;

		bool found = openspace::SpiceManager::ref().getFieldOfView("NH_LORRI", shape, instrument, boresight, bounds);
		if (!found) LERROR("Could not locate instrument"); // fixlater

		float size = 4 * sizeof(float);
		float *begin = &_varray[0];

		glm::vec4 origin(0);
		glm::vec4 col_gray(0.3, 0.3, 0.3, 1);
		glm::vec4 col_start(1.00, 0.89, 0.00, 1);
		glm::vec4 col_end(1.00, 0.29, 0.00, 1);
	//	glm::vec4 bsight_t(boresight[0], boresight[1], boresight[2], data.position[3] - 3);

		float sc = 2.4;
		int indx = 0;
		for (int i = 0; i < 4; i++){
			// might as well take glm. Would be nice if we had just one type to deal with here...
			psc dir = PowerScaledCoordinate::PowerScaledCoordinate(bounds[i][0], bounds[i][1], bounds[i][2], 1);
			psc intercept, interceptVector;
			double targetEpoch;
			found = openspace::SpiceManager::ref().getSurfaceIntercept("JUPITER", _target, instrument,
																	   _frame, "ELLIPSOID", "XCN+S",
																	   _time, targetEpoch, dir, intercept, interceptVector);
			interceptVector[3] += 3;
			glm::vec4 corner(bounds[i][0], bounds[i][1], bounds[i][2], data.position[3]);

			corner = tmp*corner;

			//cleanup later.
			if (found){
				memcpy(&_varray[indx], glm::value_ptr(origin), size);
				indx += 4;
				memcpy(&_varray[indx], glm::value_ptr(col_start), size);
				indx += 4;
				memcpy(&_varray[indx], glm::value_ptr(interceptVector.vec4()), size);
				indx += 4;
				memcpy(&_varray[indx], glm::value_ptr(col_end), size);
				indx += 4;
			}else{
				memcpy(&_varray[indx], glm::value_ptr(origin), size);
				indx += 4;
				memcpy(&_varray[indx], glm::value_ptr(col_gray), size);
				indx += 4;
				memcpy(&_varray[indx], glm::value_ptr(corner), size);
				indx += 4;
				memcpy(&_varray[indx], glm::value_ptr(glm::vec4(0)), size);
				indx += 4;
			}
		}
		updateData();
	}
	_oldTime = _time;

	glBindVertexArray(_vaoID); 
	glDrawArrays(_mode, 0, _vtotal);
	glBindVertexArray(0);


	glPointSize(3.f);
	glBindVertexArray(_vaoID);
	glDrawArrays(GL_POINTS, 0, _vtotal);
	glBindVertexArray(0);

	_programObject->deactivate();
}

void RenderableFov::update(const UpdateData& data){
	double lightTime;
	_time  = data.time;
	_delta = data.delta;

	openspace::SpiceManager::ref().getPositionTransformMatrix("NH_LORRI", "GALACTIC", data.time, _stateMatrix);
}

void RenderableFov::loadTexture()
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