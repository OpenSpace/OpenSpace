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
#include <chrono>

namespace {
	const std::string _loggerCat = "RenderableFov";
	//constants
		const std::string keyBody                 = "Body";
		const std::string keyFrame                = "Frame";
		const std::string keyPathModule           = "ModulePath";
		const std::string keyColor                = "RGB";
		const std::string keyInstrument           = "Instrument.Name";
		const std::string keyInstrumentMethod     = "Instrument.Method";
		const std::string keyInstrumentAberration = "Instrument.Aberration";


}
//#define DEBUG
namespace openspace{
	// colors, move later
	glm::vec4 origin(0);
	glm::vec4 col_gray(0.3, 0.3, 0.3, 1);
	glm::vec4 col_start(1.00, 0.89, 0.00, 1);
	glm::vec4 col_end(1.00, 0.29, 0.00, 1);
	glm::vec4 col_sq(1.00, 0.29, 0.00, 1);

	glm::vec4 col_proj(1, 1, 1, 1);

	RenderableFov::RenderableFov(const ghoul::Dictionary& dictionary)
		: Renderable(dictionary)
		, _colorTexturePath("colorTexture", "Color Texture")
		, _programObject(nullptr)
		, _texture(nullptr)
		, _vaoID1(0)
		, _vboID1(0)
		, _iboID1(0)
		, _vaoID2(0)
		, _vboID2(0)
		, _iboID2(0)
		, _mode(GL_LINES){

		assert(dictionary.getValue(keyBody                 , _spacecraft));
		assert(dictionary.getValue(keyFrame                , _frame));
		assert(dictionary.getValue(keyInstrument           , _instrumentID));
		assert(dictionary.getValue(keyInstrumentMethod     , _method));
		assert(dictionary.getValue(keyInstrumentAberration , _aberrationCorrection));
}
void RenderableFov::allocateData(){
	int points = 8;
	_stride = points;
	_isize = points;
	_iarray1 = new int[_isize];
	for (int i = 0; i < points; i++){
		for (int j = 0; j < 4; j++){
			_varray1.push_back(0); // pos
		}
		for (int j = 0; j < 4; j++){
			_varray1.push_back(0); // col
		}
		_iarray1[i] = i;
	}

	_stride = 8;
	_vsize = _varray1.size();
	_vtotal = static_cast<int>(_vsize / _stride);

	// allocate second vbo data 
	int cornerPoints = 5;
	_isize2 = cornerPoints;
	_iarray2 = new int[_isize2];
	for (int i = 0; i < _isize2; i++){
		_iarray2[i] = i;
	}
	_varray2.resize(40);
	_vsize2  = 40;
	_vtotal2 = 5;
	_isteps = 5;
}

RenderableFov::~RenderableFov(){
	deinitialize();
}

bool RenderableFov::initialize(){
	bool completeSuccess = true;
	if (_programObject == nullptr)
		completeSuccess &= OsEng.ref().configurationManager().getValue("EphemerisProgram", _programObject);

	allocateData();
	sendToGPU();

	return completeSuccess;
}

bool RenderableFov::deinitialize(){
	delete _texture;
	_texture = nullptr;
	return true;
}
void RenderableFov::sendToGPU(){
	// Initialize and upload to graphics card
	glGenVertexArrays(1, &_vaoID1);
	glGenBuffers(1, &_vboID1);
	glGenBuffers(1, &_iboID1);

	glBindVertexArray(_vaoID1);
	glBindBuffer(GL_ARRAY_BUFFER, _vboID1);
	glBufferData(GL_ARRAY_BUFFER, _vsize * sizeof(GLfloat), NULL, GL_STREAM_DRAW); // orphaning the buffer, sending NULL data.
	glBufferSubData(GL_ARRAY_BUFFER, 0, _vsize * sizeof(GLfloat), &_varray1[0]);

	GLsizei st = sizeof(GLfloat) * _stride;

	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, st, (void*)0);
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, st, (void*)(4 * sizeof(GLfloat)));

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iboID1);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, _isize * sizeof(int), _iarray1, GL_STATIC_DRAW);
	glBindVertexArray(0);

	// second vbo
	glGenVertexArrays(1, &_vaoID2);
	glGenBuffers(1, &_vboID2);
	glGenBuffers(1, &_iboID2);

	glBindVertexArray(_vaoID2);
	glBindBuffer(GL_ARRAY_BUFFER, _vboID2);
	glBufferData(GL_ARRAY_BUFFER, _vsize2 * sizeof(GLfloat), NULL, GL_STREAM_DRAW); // orphaning the buffer, sending NULL data.
	glBufferSubData(GL_ARRAY_BUFFER, 0, _vsize2 * sizeof(GLfloat), &_varray2[0]);

	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, st, (void*)0);
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, st, (void*)(4 * sizeof(GLfloat)));

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iboID2);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, _isize2 * sizeof(int), _iarray2, GL_STATIC_DRAW);
	glBindVertexArray(0);
}

void RenderableFov::printFovArray(){
	int tmp = 0;
	for (int i = 0; i < _varray2.size(); i++){
		std::cout << _varray2[i] << " ";
		tmp++;
		if (tmp == 4){
			std::cout << std::endl;
			tmp = 0;
		}
	}
	std::cout << "\n";
}

void RenderableFov::insertPoint(std::vector<float>& arr, psc& p, glm::vec4& c){
	for (int i = 0; i < 4; i++){
		arr.push_back(p[i]);
	}
	for (int i = 0; i < 4; i++){
		arr.push_back(c[i]);
	}
	_nrInserted++;
}

double RenderableFov::distanceBetweenPoints(psc p1, psc p2){
	PowerScaledScalar dist = (p1 - p2).length();
	return dist[0] * pow(10, dist[1]);
}

double RenderableFov::distanceBetweenPoints(glm::dvec3 p1, glm::dvec3 p2){
	glm::dvec3 tmp = p1 - p2;
	return sqrt(tmp[0] * tmp[0] + tmp[1] * tmp[1] + tmp[2] * tmp[2]);
}

psc RenderableFov::pscInterpolate(psc p0, psc p1, float t){
	assert(t >= 0 && t <= 1);
	float t2 = (1.f - t);
	return PowerScaledCoordinate::PowerScaledCoordinate(t2*p0[0] + t*p1[0],
														t2*p0[1] + t*p1[1],	
														t2*p0[2] + t*p1[2], 
														t2*p0[3] + t*p1[3]);
}
glm::dvec3 RenderableFov::interpolate(glm::dvec3 p0, glm::dvec3 p1, float t){
	assert(t >= 0 && t <= 1);
	float t2 = (1.f - t);
	return glm::dvec3(p0.x*t2 + p1.x*t, p0.y*t2 + p1.y*t, p0.z*t2 + p1.z*t);
}

psc RenderableFov::checkForIntercept(glm::dvec3 ray){
	double targetEt;
	glm::dvec3 ip, iv;
	bool intercepted = openspace::SpiceManager::ref().getSurfaceIntercept(_fovTarget, _spacecraft, _instrumentID,
		_frame, _method, _aberrationCorrection, _time, targetEt, ray, ip, iv);
	psc interceptVector = PowerScaledCoordinate::CreatePowerScaledCoordinate(iv[0], iv[1], iv[2]);
	interceptVector[3] += 3;

	return interceptVector;
}

psc RenderableFov::orthogonalProjection(glm::dvec3 vecFov){
	glm::dvec3 vecToTarget;
	double lt;
	SpiceManager::ref().getTargetPosition(_fovTarget, _spacecraft, _frame, _aberrationCorrection, _time, vecToTarget, lt);
	openspace::SpiceManager::ref().frameConversion(vecFov, _instrumentID, _frame, _time);
	glm::dvec3 p = openspace::SpiceManager::ref().orthogonalProjection(vecToTarget, vecFov);

	psc projection = PowerScaledCoordinate::CreatePowerScaledCoordinate(p[0], p[1], p[2]);
	projection[3] += 3;

	return projection;
}

glm::dvec3 RenderableFov::bisection(glm::dvec3 p1, glm::dvec3 p2, double tolerance){
	//check if point is on surface
	double targetEt;
	glm::dvec3 ip, iv;
	glm::dvec3 half = interpolate(p1, p2, 0.5f);
	bool intercepted = openspace::SpiceManager::ref().getSurfaceIntercept(_fovTarget, _spacecraft, _instrumentID,
													_frame, _method, _aberrationCorrection, _time, targetEt, half, ip, iv);
	if (distanceBetweenPoints(_previousHalf, half) < tolerance){
		_previousHalf = glm::dvec3(0);
		return half;
	}
	_previousHalf = half;
	//recursive search
	if (intercepted == false){
		return bisection(p1, half, tolerance);
	}else{
		return bisection(half, p2, tolerance);
	}
}

/*
psc RenderableFov::sphericalInterpolate(glm::dvec3 p0, glm::dvec3 p1, float t){
	double targetEt, lt;
	glm::dvec3 ip, iv;
	psc targetPos; 
	SpiceManager::ref().getTargetPosition("JUPITER", _spacecraft, _frame, _aberrationCorrection, _time, targetPos, lt);

	openspace::SpiceManager::ref().getSurfaceIntercept(_fovTarget, _spacecraft, _instrumentID,
		_frame, _method, _aberrationCorrection, _time, targetEt, p0, ip, iv);
	psc psc0 = PowerScaledCoordinate::CreatePowerScaledCoordinate(iv[0], iv[1], iv[2]);
	openspace::SpiceManager::ref().getSurfaceIntercept(_fovTarget, _spacecraft, _instrumentID,
		_frame, _method, _aberrationCorrection, _time, targetEt, p1, ip, iv);
	psc psc1 = PowerScaledCoordinate::CreatePowerScaledCoordinate(iv[0], iv[1], iv[2]);
	psc0[3] += 3;
	psc1[3] += 3;

	psc0 -= targetPos;
	psc1 -= targetPos;

	double angle = psc0.angle(psc1);

	std::cout << angle << std::endl;

	double sin_a = sin(angle); // opt
	double l[2] = { sin((1.f - t)*angle) / sin_a, sin((t)*angle) / sin_a };

	std::cout << l[0] << " " <<  l[1] << std::endl;

	float s = ((t-1)*psc0[3] + (t)*psc1[3]);
	float x = (l[0]*psc0[0] + l[1]*psc1[0]);
	float y = (l[0]*psc0[1] + l[1]*psc1[1]);
	float z = (l[0]*psc0[2] + l[1]*psc1[2]);

	psc interpolated = PowerScaledCoordinate::PowerScaledCoordinate(x, y, z, 10);
	return interpolated;
}
*/

void RenderableFov::fovProjection(bool H[], std::vector<glm::dvec3> bounds){
	_nrInserted = 0;
	_varray2.clear();// empty the array

	double t;
	double tolerance = 0.0000001; // very low tolerance factor
	psc interceptVector;
	glm::dvec3 mid; 
	glm::dvec3 interpolated;
	glm::dvec3 current;
	glm::dvec3 next;
	
	for (int i = 0; i < 4; i++){
		int k = (i + 1 > 3) ? 0 : i + 1;
		current = bounds[i];
		next    = bounds[k];
		if (H[i] == false){ 
			insertPoint(_varray2, orthogonalProjection(current), glm::vec4(1));
		}
		if (H[i] == true && H[i + 1] == false){ 
			mid = bisection(current, next, tolerance);
			for (int j = 1; j <= _isteps; j++){
				t = ((double)j / _isteps);
				interpolated = interpolate(current, mid, t);
				interceptVector = (j < _isteps) ? checkForIntercept(interpolated) : orthogonalProjection(interpolated);
				insertPoint(_varray2, interceptVector, col_sq);
			}
		}
		if (H[i] == false && H[i+1] == true){
			mid = bisection(next, current, tolerance);
			for (int j = 1; j <= _isteps; j++){
				t = ((double)j / _isteps);
				interpolated = interpolate(mid, next, t);
				interceptVector = (j > 1) ? checkForIntercept(interpolated) : orthogonalProjection(interpolated);
				insertPoint(_varray2, interceptVector, col_sq);
			}
		}
		if (H[i] == true && H[i + 1] == true){
			for (int j = 0; j <= _isteps; j++){
				t = ((double)j / _isteps);
				interpolated = interpolate(current, next, t);
				interceptVector = checkForIntercept(interpolated);
				insertPoint(_varray2, interceptVector, col_sq);
			}
		}
	}

	if (_nrInserted == 0){
		_rebuild = false;
	}else{
		_rebuild = true;
		//update size etc;
		_vtotal2 = _nrInserted;
		_isize2  = _nrInserted;
		_vsize2  = _varray2.size();
		_iarray2 = new int[_isize2];
		for (int i = 0; i < _isize2; i++)
			_iarray2[i] = i;
	}

}


void RenderableFov::updateData(){
	glBindBuffer(GL_ARRAY_BUFFER, _vboID1);
	glBufferSubData(GL_ARRAY_BUFFER, 0, _vsize * sizeof(GLfloat), &_varray1[0]);

	if (!_rebuild){
		glBindBuffer(GL_ARRAY_BUFFER, _vboID2);
		glBufferSubData(GL_ARRAY_BUFFER, 0, _vsize2 * sizeof(GLfloat), &_varray2[0]);
	}else{
		glGenVertexArrays(1, &_vaoID2);
		glGenBuffers(1, &_vboID2);
		glGenBuffers(1, &_iboID2);

		glBindVertexArray(_vaoID2);
		glBindBuffer(GL_ARRAY_BUFFER, _vboID2);
		glBufferData(GL_ARRAY_BUFFER, _vsize2 * sizeof(GLfloat), NULL, GL_STREAM_DRAW); // orphaning the buffer, sending NULL data.
		glBufferSubData(GL_ARRAY_BUFFER, 0, _vsize2 * sizeof(GLfloat), &_varray2[0]);

		GLsizei st = sizeof(GLfloat) * _stride;

		glEnableVertexAttribArray(0);
		glEnableVertexAttribArray(1);
		glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, st, (void*)0);
		glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, st, (void*)(4 * sizeof(GLfloat)));

		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iboID2);
		glBufferData(GL_ELEMENT_ARRAY_BUFFER, _isize2 * sizeof(int), _iarray2, GL_STATIC_DRAW);
		glBindVertexArray(0);
	}
}



void RenderableFov::render(const RenderData& data){
	assert(_programObject);
	_programObject->activate();

	// fetch data
	glm::mat4 transform(1);

	glm::mat4 tmp = glm::mat4(1);
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

		bool found = openspace::SpiceManager::ref().getFieldOfView(_instrumentID, shape, instrument, boresight, bounds);
		if (!found) LERROR("Could not locate instrument"); // fixlater

		float size = 4 * sizeof(float);

		int indx = 0;
		bool interceptTag[5];
		bool withinFOV;

		std::string potential[5] = { "JUPITER", "IO", "EUROPA", "GANYMEDE", "CALLISTO" };
		_fovTarget = "JUPITER"; //default
		for (int i = 0; i < 5; i++){
			withinFOV = openspace::SpiceManager::ref().targetWithinFieldOfView(_instrumentID, potential[i], _spacecraft, _method,
																				_aberrationCorrection, _time);
			if (withinFOV){
				_fovTarget = potential[i];
				break;
			}
		}

		for (int i = 0; i < 4; i++){
			glm::dvec3 ip, iv;
			double targetEpoch;
			// need to keep it explicit to keep my mind from exploding.
			interceptTag[i] = openspace::SpiceManager::ref().getSurfaceIntercept(_fovTarget, _spacecraft, _instrumentID,
				                          _frame, _method, _aberrationCorrection, _time, targetEpoch, bounds[i], ip, iv);

			if (!interceptTag[i]) _projectionBounds[i] = orthogonalProjection(bounds[i]);

			psc interceptVector = PowerScaledCoordinate::CreatePowerScaledCoordinate(iv[0], iv[1], iv[2]);
			interceptVector[3] += 3;
			glm::vec4 corner(bounds[i][0], bounds[i][1], bounds[i][2], data.position[3]);
			
			corner = tmp*corner; 

			if (interceptTag[i]){
				// INTERCEPTIONS
				memcpy(&_varray1[indx], glm::value_ptr(origin), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(col_start), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(interceptVector.vec4()), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(col_end), size);
				indx += 4;
			}else if (withinFOV){
				// FOV LARGER THAN OBJECT
				memcpy(&_varray1[indx], glm::value_ptr(origin), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(glm::vec4(0,0,1,1)), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(_projectionBounds[i].vec4()), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(glm::vec4(0, 0.5, 0.7, 1)), size);
				indx += 4;
			}else{
				// "INFINITE" FOV
				memcpy(&_varray1[indx], glm::value_ptr(origin), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(col_gray), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(corner), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(glm::vec4(0)), size);
				indx += 4;
			}
		}
		interceptTag[4] = interceptTag[0]; // 0 & 5 same point
		fovProjection(interceptTag, bounds);
		updateData();
	}
	_oldTime = _time;

	glLineWidth(1.f);
	glBindVertexArray(_vaoID1);
	glDrawArrays(_mode, 0, _vtotal);
	glBindVertexArray(0);

	//render points
	glPointSize(2.f);
	glBindVertexArray(_vaoID1);
	glDrawArrays(GL_POINTS, 0, _vtotal);
	glBindVertexArray(0);

	//second vbo
	glLineWidth(1.f);
	glBindVertexArray(_vaoID2);
	glDrawArrays(GL_LINE_LOOP, 0, _vtotal2);
	glBindVertexArray(0);
	/*
	glPointSize(3.f);
	glBindVertexArray(_vaoID2);
	glDrawArrays(GL_POINTS, 0, _vtotal2);
	glBindVertexArray(0);
	*/	
	_programObject->deactivate();
}

void RenderableFov::update(const UpdateData& data){
	double lightTime;
	_time  = data.time;
	_delta = data.delta;

	openspace::SpiceManager::ref().getPositionTransformMatrix(_instrumentID, _frame, data.time, _stateMatrix);
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