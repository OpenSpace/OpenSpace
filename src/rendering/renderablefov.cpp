/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/constants.h>
#include <openspace/util/spicemanager.h>

#include <openspace/util/imagesequencer.h>

#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/filesystem/filesystem.h>

#include <openspace/query/query.h>

#include <openspace/util/spicemanager.h>
#include <iomanip>
#include <utility>   
#include <chrono>

namespace {
	const std::string _loggerCat              = "RenderableFov";
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
	glm::vec4 col_sq;
	glm::vec4 c_project;
	glm::vec4 col_end;
	glm::vec4 blue;
	glm::vec4 col_gray;
	glm::vec4 col_start;

	RenderableFov::RenderableFov(const ghoul::Dictionary& dictionary)
		: Renderable(dictionary)
		, _colorTexturePath("colorTexture", "Color Texture")
		, _programObject(nullptr)
		, _texture(nullptr)
		, _mode(GL_LINES){

		bool b1 = dictionary.getValue(keyBody                 , _spacecraft);
		bool b2 = dictionary.getValue(keyFrame                , _frame);
		bool b3 = dictionary.getValue(keyInstrument           , _instrumentID);
		bool b4 = dictionary.getValue(keyInstrumentMethod     , _method);
		bool b5 = dictionary.getValue(keyInstrumentAberration , _aberrationCorrection);

		assert(b1 == true);
		assert(b2 == true);
		assert(b3 == true);
		assert(b4 == true);
		assert(b5 == true);
}
void RenderableFov::allocateData(){ 
	int points = 8;
	_stride[0] = points;
	_isize[0] = points;
	_iarray1[0] = new int[_isize[0]];
	for (int i = 0; i < points; i++){
		for (int j = 0; j < 4; j++){
			_varray1.push_back(0); // pos
		}
		for (int j = 0; j < 4; j++){
			_varray1.push_back(0); // col
		}
		_iarray1[0][i] = i;
	}

	_stride[0] = 8;
	_vsize[0] = _varray1.size();
	_vtotal[0] = static_cast<int>(_vsize[0] / _stride[0]);

	// allocate second vbo data 
	int cornerPoints = 5;
	_isize[1] = cornerPoints;
	_iarray1[1] = new int[_isize[1]];
	for (int i = 0; i < _isize[1]; i++){
		_iarray1[1][i] = i;
	}
	_varray2.resize(40);
	_vsize[1] = 40;
	_vtotal[1] = 5;
	_isteps = 10;
}

RenderableFov::~RenderableFov(){
	deinitialize();
}


bool RenderableFov::initialize(){
	bool completeSuccess = true;
	if (_programObject == nullptr)
		completeSuccess &= OsEng.ref().configurationManager()->getValue("FovProgram", _programObject);

	allocateData();
	sendToGPU();

	return completeSuccess;
}

bool RenderableFov::deinitialize(){
	delete _texture;
	_texture = nullptr;
	return true;
}

bool RenderableFov::isReady() const {
	return _programObject != nullptr;
}

void RenderableFov::sendToGPU(){
	// Initialize and upload to graphics card
	glGenVertexArrays(1, &_vaoID[0]);
	glGenBuffers(1, &_vboID[0]);
	glGenBuffers(1, &_iboID[0]);

	glBindVertexArray(_vaoID[0]);
	glBindBuffer(GL_ARRAY_BUFFER, _vboID[0]);
	glBufferData(GL_ARRAY_BUFFER, _vsize[0] * sizeof(GLfloat), NULL, GL_STATIC_DRAW); // orphaning the buffer, sending NULL data.
	glBufferSubData(GL_ARRAY_BUFFER, 0, _vsize[0] * sizeof(GLfloat), &_varray1[0]);

	GLsizei st = sizeof(GLfloat) * _stride[0];

	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, st, (void*)0);
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, st, (void*)(4 * sizeof(GLfloat)));

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iboID[0]);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, _isize[0] * sizeof(int), _iarray1, GL_STATIC_DRAW);
	glBindVertexArray(0);

	// second vbo
	glGenVertexArrays(1, &_vaoID[1]);
	glGenBuffers(1, &_vboID[1]);
	glGenBuffers(1, &_iboID[1]);

	glBindVertexArray(_vaoID[1]);
	glBindBuffer(GL_ARRAY_BUFFER, _vboID[1]);
	glBufferData(GL_ARRAY_BUFFER, _vsize[1] * sizeof(GLfloat), NULL, GL_STATIC_DRAW); // orphaning the buffer, sending NULL data.
	glBufferSubData(GL_ARRAY_BUFFER, 0, _vsize[1] * sizeof(GLfloat), &_varray2[0]);

	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, st, (void*)0);
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, st, (void*)(4 * sizeof(GLfloat)));

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iboID[1]);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, _isize[1] * sizeof(int), _iarray1[1], GL_STATIC_DRAW);
	glBindVertexArray(0);
}
// various helper methods

void RenderableFov::insertPoint(std::vector<float>& arr, psc& p, glm::vec4& c){
	for (int i = 0; i < 4; i++){
		arr.push_back(p[i]);
	}
	for (int i = 0; i < 4; i++){
		arr.push_back(c[i]);
	}
	_nrInserted++;
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
// This method is the current bottleneck.
psc RenderableFov::checkForIntercept(glm::dvec3 ray){
	double targetEt;
	bool intercepted = openspace::SpiceManager::ref().getSurfaceIntercept(_fovTarget, _spacecraft, _instrumentID,
																	      _frame, _method, _aberrationCorrection, 
																		  _time, targetEt, ray, ipoint, ivec);
	_interceptVector = PowerScaledCoordinate::CreatePowerScaledCoordinate(ivec[0], ivec[1], ivec[2]);
	_interceptVector[3] += 3;

	return _interceptVector;
}
// Orthogonal projection next to planets surface, can also be optimized. 
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
// Bisection method, simple recurtion
glm::dvec3 RenderableFov::bisection(glm::dvec3 p1, glm::dvec3 p2, double tolerance){
	//check if point is on surface
	double targetEt;
	glm::dvec3 half = interpolate(p1, p2, 0.5f);
	bool intercepted = openspace::SpiceManager::ref().getSurfaceIntercept(_fovTarget, _spacecraft, _instrumentID,
													                      _frame, _method, _aberrationCorrection, 
																		  _time, targetEt, half, ipoint, ivec);
	if (glm::distance(_previousHalf, half) < tolerance){
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
	README:
	There are 4 different cases as each boundary  vector can either  have  detected
	an  intercept or is outside of the planets surface. When no such intercepts are
	detected the algorithm performs an orthogonal  projection to 'clip' the current
	fov vector next to the planets surface.  If two or more intercepts are detected
	the algorithm continues with the bisection method O(logn) for points [Pn, Pn+1]
	to locate the point Pb where  the  orthogonal  plane  meets the planets surface 
	(within  ~20  iterations this  will  narrow  down  to  centimeter  resolution). 
	Upon finding Pb a linear interpolation is performed for [Pn, Pb], at this stage 
	the points are located on a straight line between the surface intercept and the 
	surface-bound  fov-corner.  In  order  to  correctly  place these points on the 
	targets surface,  each consecutive point is queried for a surface intercept and 
	thereby moved to the hull. 
*/
void RenderableFov::fovProjection(bool H[], std::vector<glm::dvec3> bounds){
	_nrInserted = 0;
	_varray2.clear();// empty the array

	double t;
	double tolerance = 0.0000001; // very low tolerance factor
	
	glm::dvec3 mid; 
	glm::dvec3 interpolated;
	glm::dvec3 current;
	glm::dvec3 next;
	
	for (int i = 0; i < 4; i++){
		int k = (i + 1 > 3) ? 0 : i + 1;
		current = bounds[i];
		next    = bounds[k];
		if (H[i] == false){ // If point is non-interceptive, project it. 
			insertPoint(_varray2, orthogonalProjection(current), glm::vec4(1));
		}
		if (H[i] == true && H[i + 1] == false){ // current point is interceptive, next is not
			// find outer most point for interpolation
			mid = bisection(current, next, tolerance);
			for (int j = 1; j <= _isteps; j++){
				t = ((double)j / _isteps);
				// TODO: change the interpolate scheme to place points not on a straight line but instead 
				//       using either slerp or some other viable method (goal: eliminate checkForIntercept -method)
				interpolated = interpolate(current, mid, t);
				_interceptVector = (j < _isteps) ? checkForIntercept(interpolated) : orthogonalProjection(interpolated);
				insertPoint(_varray2, _interceptVector, col_sq);
			}
		}
		if (H[i] == false && H[i+1] == true){ // current point is non-interceptive, next is
			mid = bisection(next, current, tolerance);
			for (int j = 1; j <= _isteps; j++){
				t = ((double)j / _isteps);
				interpolated = interpolate(mid, next, t);
				_interceptVector = (j > 1) ? checkForIntercept(interpolated) : orthogonalProjection(interpolated);
				insertPoint(_varray2, _interceptVector, col_sq);
			}
		}
		if (H[i] == true && H[i + 1] == true){ // both points intercept
			for (int j = 0; j <= _isteps; j++){
				t = ((double)j / _isteps);
				interpolated = interpolate(current, next, t);
				_interceptVector = checkForIntercept(interpolated);
				insertPoint(_varray2, _interceptVector, col_sq);
			}
		}
	}
	// only if new points are inserted are we interested in rebuilding the 
	// vbo. Note that this can be optimized but is left as is for now. 
	if (_nrInserted == 0){
		_rebuild = false;
	}else{
		_rebuild = true;
		//update size etc;
		_vtotal[1] = _nrInserted;
		_isize[1]  = _nrInserted;
		_vsize[1]  = _varray2.size();
		_iarray1[1] = new int[_isize[1]];
		for (int i = 0; i < _isize[1]; i++)
			_iarray1[1][i] = i;
	}

}

void RenderableFov::updateData(){
	glBindBuffer(GL_ARRAY_BUFFER, _vboID[0]);
	glBufferSubData(GL_ARRAY_BUFFER, 0, _vsize[0] * sizeof(GLfloat), &_varray1[0]);

	if (!_rebuild){
		glBindBuffer(GL_ARRAY_BUFFER, _vboID[1]);
		glBufferSubData(GL_ARRAY_BUFFER, 0, _vsize[1] * sizeof(GLfloat), &_varray2[0]);
	}else{
		glBindVertexArray(_vaoID[1]);
		glBindBuffer(GL_ARRAY_BUFFER, _vboID[1]);
		glBufferData(GL_ARRAY_BUFFER, _vsize[1] * sizeof(GLfloat), NULL, GL_STATIC_DRAW); // orphaning the buffer, sending NULL data.
		glBufferSubData(GL_ARRAY_BUFFER, 0, _vsize[1] * sizeof(GLfloat), &_varray2[0]);

		GLsizei st = sizeof(GLfloat) * _stride[0];

		glEnableVertexAttribArray(0);
		glEnableVertexAttribArray(1);
		glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, st, (void*)0);
		glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, st, (void*)(4 * sizeof(GLfloat)));

		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iboID[1]);
		glBufferData(GL_ELEMENT_ARRAY_BUFFER, _isize[1] * sizeof(int), _iarray1[1], GL_STATIC_DRAW);
		glBindVertexArray(0);
	}
}
void RenderableFov::computeColors(){
	double t2 = openspace::ImageSequencer::ref().getNextCaptureTime();
	double diff = (t2 - _time);
	double t = 0.0;
	if (diff <= 7.0) t = 1.f - diff / 7.0;

	if (diff < 0) t = 0.0;

	c_project = glm::vec4(0.0, 1.0, 0.00,1);
	col_end   = glm::vec4(1.00, 0.29, 0.00, 1);
	blue      = glm::vec4(0, 0.5, 0.7, 1);
	col_gray  = glm::vec4(0.3, 0.3, 0.3, 1);
	col_start = glm::vec4(1.00, 0.89, 0.00, 1);
	col_sq    = glm::vec4(1.00, 0.29, 0.00, 1);

	col_end.x = c_project.x*t + col_end.x*(1 - t);
	col_end.y = c_project.y*t + col_end.y*(1 - t);
	col_end.z = c_project.z*t + col_end.z*(1 - t);

	blue.x = c_project.x*t + blue.x*(1 - t);
	blue.y = c_project.y*t + blue.y*(1 - t);
	blue.z = c_project.z*t + blue.z*(1 - t);

	col_sq.x = c_project.x*t + col_sq.x*(1 - t);
	col_sq.y = c_project.y*t + col_sq.y*(1 - t);
	col_sq.z = c_project.z*t + col_sq.z*(1 - t);
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

	// update only when time progresses.
	if (_oldTime != _time){
		std::string shape, instrument;
		std::vector<glm::dvec3> bounds;
		glm::dvec3 boresight;
		
		// fetch data for specific instrument (shape, boresight, bounds etc)
		bool found = openspace::SpiceManager::ref().getFieldOfView(_instrumentID, shape, instrument, boresight, bounds);
		if (!found) LERROR("Could not locate instrument"); // fixlater

		float size = 4 * sizeof(float);
		int indx = 0;

		// set target based on visibility to specific instrument,
		// from here on the _fovTarget is the target for all spice functions.
		//std::string potential[5] = { "Jupiter", "Io", "Europa", "Ganymede", "Callisto" };
		std::string potential[2] = { "Pluto", "Charon" };

		_fovTarget = potential[0]; //default
		for (int i = 0; i < 2; i++){
			_withinFOV = openspace::SpiceManager::ref().targetWithinFieldOfView(_instrumentID, potential[i], 
																				_spacecraft, _method,
																				_aberrationCorrection, _time);
			if (_withinFOV){
				_fovTarget = potential[i];
				break;
			}
		}
		
		computeColors();
		double t2 = openspace::ImageSequencer::ref().getNextCaptureTime();
		double diff = (t2 - _time);
		double t = 0.0;
		if (diff <= 30.0) t = 1.f - diff / 30.0;

		double targetEpoch;
		// for each FOV vector
		for (int i = 0; i < 4; i++){
			// compute surface intercept
			_interceptTag[i] = openspace::SpiceManager::ref().getSurfaceIntercept(_fovTarget, _spacecraft, _instrumentID,
				                                                                  _frame, _method, _aberrationCorrection, 
																				  _time, targetEpoch, bounds[i], ipoint, ivec);
			// if not found, use the orthogonal projected point
			if (!_interceptTag[i]) _projectionBounds[i] = orthogonalProjection(bounds[i]);
	
			// VBO1 : draw vectors representing outer points of FOV. 
			if (_interceptTag[i]){
				_interceptVector = PowerScaledCoordinate::CreatePowerScaledCoordinate(ivec[0], ivec[1], ivec[2]);
				_interceptVector[3] += 3;
				//_interceptVector = pscInterpolate(_interceptVector, bsvec, t);
				// INTERCEPTIONS
				memcpy(&_varray1[indx], glm::value_ptr(glm::vec4(0)), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(col_start), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(_interceptVector.vec4()), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(col_end), size);
				indx += 4;
			}
			else if (_withinFOV){
				// FOV OUTSIDE OBJECT
				memcpy(&_varray1[indx], glm::value_ptr(glm::vec4(0)), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(glm::vec4(0,0,1,1)), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(_projectionBounds[i].vec4()), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(blue), size);
				indx += 4;
			}else{
				glm::vec4 corner(bounds[i][0], bounds[i][1], bounds[i][2], data.position[3]+1);
				corner = tmp*corner;
				// "INFINITE" FOV
				memcpy(&_varray1[indx], glm::value_ptr(glm::vec4(0)), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(col_gray), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(corner), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(glm::vec4(0)), size);
				indx += 4;
			}
		}
		_interceptTag[4] = _interceptTag[0]; // 0 & 5 same point
		// Draw surface square! 
		fovProjection(_interceptTag, bounds);
		updateData();
	}
	_oldTime = _time;
	
	glLineWidth(1.f);
	glBindVertexArray(_vaoID[0]);
	glDrawArrays(_mode, 0, _vtotal[0]);
	glBindVertexArray(0);

	//render points
	glPointSize(2.f);
	glBindVertexArray(_vaoID[0]);
	glDrawArrays(GL_POINTS, 0, _vtotal[0]);
	glBindVertexArray(0);
	
	//second vbo
	glLineWidth(2.f);
	glBindVertexArray(_vaoID[1]);
	glDrawArrays(GL_LINE_LOOP, 0, _vtotal[1]);
	glBindVertexArray(0);
	
	/*glPointSize(5.f);
	glBindVertexArray(_vaoID2);
	glDrawArrays(GL_POINTS, 0, _vtotal2);
	glBindVertexArray(0);
	*/
	_programObject->deactivate();
}

void RenderableFov::update(const UpdateData& data){
	double lightTime;
	_time  = data.time;
	openspace::SpiceManager::ref().getPositionTransformMatrix(_instrumentID, _frame, data.time, _stateMatrix);
}

void RenderableFov::loadTexture()
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