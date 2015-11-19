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

#include <modules/newhorizons/rendering/renderablefov.h>

#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/constants.h>
#include <openspace/util/spicemanager.h>

#include <modules/newhorizons/util/imagesequencer.h>
#include <openspace/util/time.h>


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
    const std::string keyPotentialTargets     = "PotentialTargets";
}

namespace openspace {

RenderableFov::RenderableFov(const ghoul::Dictionary& dictionary)
	: Renderable(dictionary)
	, _lineWidth("lineWidth", "Line Width", 1.f, 1.f, 20.f)
	, _drawSolid("solidDraw", "Draw as Quads", false)
	, _programObject(nullptr)
	, _texture(nullptr)
	, _drawFOV(false)
    , _mode(GL_LINES)
{
    bool success = dictionary.getValue(keyBody, _spacecraft);
    ghoul_assert(success, "");

    success = dictionary.getValue(keyFrame, _frame);
    ghoul_assert(success, "");

    success = dictionary.getValue(keyInstrument, _instrumentID);
    ghoul_assert(success, "");

    success = dictionary.getValue(keyInstrumentMethod, _method);
    ghoul_assert(success, "");

    std::string a = "NONE";
    success = dictionary.getValue(keyInstrumentAberration, a);
    a = SpiceManager::AberrationCorrection(a);
    ghoul_assert(success, "");

    ghoul::Dictionary potentialTargets;
    success = dictionary.getValue(keyPotentialTargets, potentialTargets);
    ghoul_assert(success, "");

    _potentialTargets.resize(potentialTargets.size());
    for (int i = 0; i < potentialTargets.size(); ++i) {
        std::string target;
        potentialTargets.getValue(std::to_string(i + 1), target);
        _potentialTargets[i] = target;
    }

    addProperty(_lineWidth);
	addProperty(_drawSolid);
}

void RenderableFov::allocateData() { 
	std::string shape, instrument;
	// fetch data for specific instrument (shape, boresight, bounds etc)
    try {
        SpiceManager::ref().getFieldOfView(_instrumentID, shape, instrument, _boresight, _bounds);

        _stride = 8;
        
        _projectionBounds.resize(_bounds.size());
        int initBoundPoints = 2 * (_bounds.size() + 1);
        _fovBounds.resize(initBoundPoints*_stride);
        _vBoundsSize = static_cast<unsigned int>(_fovBounds.size());
        // allocate second vbo data
        _fovPlane.resize(40);
        _vPlaneSize = 40;
        _isteps = 10; // Interpolation steps per intersecting segment

    }
    catch (const SpiceManager::SpiceKernelException& e) {
        LERROR(e.what());
    }
}

RenderableFov::~RenderableFov() {
	deinitialize();
}

bool RenderableFov::initialize() {
	bool completeSuccess = true;
    if (_programObject == nullptr) {
        _programObject = ghoul::opengl::ProgramObject::Build("FovProgram",
            "${MODULE_NEWHORIZONS}/shaders/fov_vs.glsl",
            "${MODULE_NEWHORIZONS}/shaders/fov_fs.glsl");
        if (!_programObject)
            return false;
    }
	allocateData();
	sendToGPU();
	return completeSuccess;
}

bool RenderableFov::deinitialize() {
	return true;
}

bool RenderableFov::isReady() const {
	return _programObject != nullptr;
}

void RenderableFov::sendToGPU() {
	// Initialize and upload to graphics card

	// FOV lines
	glGenVertexArrays(1, &_fovBoundsVAO);
	glGenBuffers(1, &_fovBoundsVBO);
	glBindVertexArray(_fovBoundsVAO);
	glBindBuffer(GL_ARRAY_BUFFER, _fovBoundsVBO);
	glBufferData(GL_ARRAY_BUFFER, _vBoundsSize * sizeof(GLfloat), NULL, GL_STATIC_DRAW); // orphaning the buffer, sending NULL data.
	glBufferSubData(GL_ARRAY_BUFFER, 0, _vBoundsSize * sizeof(GLfloat), _fovBounds.data());

	GLsizei st = sizeof(GLfloat) * _stride;

	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, st, (void*)0);
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, st, (void*)(4 * sizeof(GLfloat)));

	glBindVertexArray(0);

	// Orthogonal Plane
	glGenVertexArrays(1, &_fovPlaneVAO);
	glGenBuffers(1, &_fovPlaneVBO);

	glBindVertexArray(_fovPlaneVAO);
	glBindBuffer(GL_ARRAY_BUFFER, _fovPlaneVBO);
	glBufferData(GL_ARRAY_BUFFER, _vPlaneSize * sizeof(GLfloat), NULL, GL_STATIC_DRAW); // orphaning the buffer, sending NULL data.
	glBufferSubData(GL_ARRAY_BUFFER, 0, _vPlaneSize * sizeof(GLfloat), _fovPlane.data());

	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, st, (void*)0);
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, st, (void*)(4 * sizeof(GLfloat)));

	glBindVertexArray(0);
}

void RenderableFov::updateGPU() {
	glBindBuffer(GL_ARRAY_BUFFER, _fovBoundsVBO);
	glBufferSubData(GL_ARRAY_BUFFER, 0, _vBoundsSize * sizeof(GLfloat), _fovBounds.data());
	if (!_rebuild){
		// no new points
		glBindBuffer(GL_ARRAY_BUFFER, _fovPlaneVBO);
		glBufferSubData(GL_ARRAY_BUFFER, 0, _vPlaneSize * sizeof(GLfloat), _fovPlane.data());
	}else{
		// new points - memory change 
		glBindVertexArray(_fovPlaneVAO);
		glBindBuffer(GL_ARRAY_BUFFER, _fovPlaneVBO);
		glBufferData(GL_ARRAY_BUFFER, _vPlaneSize * sizeof(GLfloat), NULL, GL_STATIC_DRAW); // orphaning the buffer, sending NULL data.
		glBufferSubData(GL_ARRAY_BUFFER, 0, _vPlaneSize * sizeof(GLfloat), _fovPlane.data());

		GLsizei st = sizeof(GLfloat) * _stride;
		glEnableVertexAttribArray(0);
		glEnableVertexAttribArray(1);
		glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, st, (void*)0);
		glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, st, (void*)(4 * sizeof(GLfloat)));
	}

	glBindVertexArray(0);
}

// various helper methods
void RenderableFov::insertPoint(std::vector<float>& arr, glm::vec4 p, glm::vec4 c) {
	for (int i = 0; i < 4; i++){
		arr.push_back(p[i]);
	}
	for (int i = 0; i < 4; i++){
		arr.push_back(c[i]);
	}
	_nrInserted++;
}

glm::dvec3 RenderableFov::interpolate(glm::dvec3 p0, glm::dvec3 p1, float t) {
	assert(t >= 0 && t <= 1);
	float t2 = (1.f - t);
	return glm::dvec3(p0.x*t2 + p1.x*t, p0.y*t2 + p1.y*t, p0.z*t2 + p1.z*t);
}


// This method is the current bottleneck.
psc RenderableFov::checkForIntercept(glm::dvec3 ray) {
	bool intercepted = false;
	openspace::SpiceManager::ref().getSurfaceIntercept(_fovTarget, _spacecraft, _instrumentID,
		_frame, _method, _aberrationCorrection,
		_time, _targetEpoch, ray, ipoint, ivec, intercepted);
	
	ivec *= 0.9999;// because fov lands exactly on top of surface we need to move it out slightly
	_interceptVector = PowerScaledCoordinate::CreatePowerScaledCoordinate(ivec[0], ivec[1], ivec[2]);
	_interceptVector[3] += 3;

	return _interceptVector;
}
// Orthogonal projection next to planets surface
psc RenderableFov::orthogonalProjection(glm::dvec3 vecFov) {
	glm::dvec3 vecToTarget =
	SpiceManager::ref().targetPosition(_fovTarget, _spacecraft, _frame, _aberrationCorrection, _time, _lt);
	openspace::SpiceManager::ref().frameConversion(vecFov, _instrumentID, _frame, _time);
	glm::dvec3 p = openspace::SpiceManager::ref().orthogonalProjection(vecToTarget, vecFov);

	psc projection = PowerScaledCoordinate::CreatePowerScaledCoordinate(p[0], p[1], p[2]);
	projection[3] += 3;

	return projection;
}
// Bisection method, simple recurtion
glm::dvec3 RenderableFov::bisection(glm::dvec3 p1, glm::dvec3 p2, double tolerance) {
	//check if point is on surface
	glm::dvec3 half = interpolate(p1, p2, 0.5f);
	bool intercepted = false;
	openspace::SpiceManager::ref().getSurfaceIntercept(_fovTarget, _spacecraft, _instrumentID,
		_frame, _method, _aberrationCorrection,
		_time, _targetEpoch, half, ipoint, ivec, intercepted);
	if (glm::distance(_previousHalf, half) < tolerance){
		_previousHalf = glm::dvec3(0);
		return half;
	}
	_previousHalf = half;
	//recursive search
	if (!intercepted){
		return bisection(p1, half, tolerance);
	}
	else{
		return bisection(half, p2, tolerance);
	}
}


void RenderableFov::fovSurfaceIntercept(bool H[], std::vector<glm::dvec3> bounds) {
	_nrInserted = 0;
	_fovPlane.clear();// empty the array

	double tolerance = 0.000000001; // very low tolerance factor

	glm::dvec3 mid;
	glm::dvec3 interpolated;
	glm::dvec3 current;
	glm::dvec3 next;
	glm::vec4 tmp(1);
	if (bounds.size() > 1){
		for (int i = 0; i < bounds.size(); i++){
			int k = (i + 1 > bounds.size() - 1) ? 0 : i + 1;

			current = bounds[i];
			next = bounds[k];

			if (H[i] == false){ // If point is non-interceptive, project it. 
				insertPoint(_fovPlane, orthogonalProjection(current).vec4(), tmp);
				if (H[i + 1] == false && _withinFOV){
					// IFF incident point is also non-interceptive BUT something is within FOV
					// we need then to check if this segment makes contact with surface
					glm::dvec3 half = interpolate(current, next, 0.5f);
					bool intercepted;
					openspace::SpiceManager::ref().getSurfaceIntercept(_fovTarget, _spacecraft, _instrumentID,
																	   _frame, _method, _aberrationCorrection,
																	   _time, _targetEpoch, half, ipoint, ivec, intercepted);
					if (intercepted){
						// find the two outer most points of intersection 
						glm::dvec3 root1 = bisection(half, current, tolerance);
						glm::dvec3 root2 = bisection(half, next, tolerance);

						insertPoint(_fovPlane, orthogonalProjection(root1).vec4(), col_sq);
						for (int j = 1; j < _isteps; j++){
							float t = (static_cast<float>(j) / _isteps);
							interpolated = interpolate(root1, root2, t);
							_interceptVector = checkForIntercept(interpolated);
							insertPoint(_fovPlane, _interceptVector.vec4(), col_sq);
						}
						insertPoint(_fovPlane, orthogonalProjection(root2).vec4(), col_sq);
					}
				}
			}
			if (H[i] == true && H[i + 1] == false){ // current point is interceptive, next is not
				// find outer most point for interpolation
				mid = bisection(current, next, tolerance);
				for (int j = 1; j <= _isteps; j++){
					float t = (static_cast<float>(j) / _isteps);
					interpolated = interpolate(current, mid, t);
					_interceptVector = (j < _isteps) ? checkForIntercept(interpolated) : orthogonalProjection(interpolated);
					insertPoint(_fovPlane, _interceptVector.vec4(), col_sq);
				}
			}
			if (H[i] == false && H[i + 1] == true){ // current point is non-interceptive, next is
				mid = bisection(next, current, tolerance);
				for (int j = 1; j <= _isteps; j++){
					float t = (static_cast<float>(j) / _isteps);
					interpolated = interpolate(mid, next, t);
					_interceptVector = (j > 1) ? checkForIntercept(interpolated) : orthogonalProjection(interpolated);
					insertPoint(_fovPlane, _interceptVector.vec4(), col_sq);
				}
			}
			if (H[i] == true && H[i + 1] == true){ // both points intercept
				for (int j = 0; j <= _isteps; j++){
					float t = (static_cast<float>(j) / _isteps);
					interpolated = interpolate(current, next, t);
					_interceptVector = checkForIntercept(interpolated);
					insertPoint(_fovPlane, _interceptVector.vec4(), col_sq);
				}
			}
		}
	 }
	if (_nrInserted == 0){
		_rebuild = false;
	} 
	else {
		_rebuild = true;
		//update size etc;
		_vPlaneSize  = static_cast<unsigned int>(_fovPlane.size());
	}
}

// This method is purely cosmetics, can very well be removed 
// but be sure to set colors somewhere. 
void RenderableFov::computeColors() {
	double t2 = (openspace::ImageSequencer2::ref().getNextCaptureTime());
	double diff = (t2 - _time);
	float t = 0.0;
	float interpolationStart = 7.0; //seconds before
	if (diff <= interpolationStart)
		t = static_cast<float>(1.0 - (diff / interpolationStart));

	if (diff < 0.0) t = 0.f;

	// This is a bit hardcoded - either we go for color tables
	// or make these properties.
	col_gray    = glm::vec4(0.7);
	col_project = glm::vec4(0.0, 1.0, 0.00, 1); 
	col_start   = glm::vec4(1.00, 0.89, 0.00, 1);
	col_end     = glm::vec4(1.00, 0.29, 0.00, 1);
	col_blue    = glm::vec4(0, 0.5, 0.7, 1);
	col_sq      = glm::vec4(1.00, 0.29, 0.00, 1);
	
	col_end  = col_project*t + col_end*(1 - t);
	col_blue = col_project*t + col_blue*(1 - t);
	col_sq   = col_project*t + col_sq*(1 - t);
	
	float alpha;
	alpha = _drawSolid ? 0.5f : 0.8f;

	col_blue.w = alpha;
	col_project.w = alpha;
	col_end.w = alpha;
}

void RenderableFov::determineTarget(){
	_fovTarget = _potentialTargets[0]; //default;
	for (int i = 0; i < _potentialTargets.size(); i++){
		bool success = openspace::SpiceManager::ref().targetWithinFieldOfView(
			_instrumentID,
			_potentialTargets[i],
			_spacecraft,
			_method,
			_aberrationCorrection,
			_time,
			_withinFOV);
		if (success && _withinFOV){
			_fovTarget = _potentialTargets[i];
			break;
		}
	}
}

void RenderableFov::computeIntercepts(const RenderData& data){
	// for each FOV vector
	_fovBounds.clear();
	for (int i = 0; i <= _bounds.size(); i++){
		int r = (i == _bounds.size()) ? 0 : i;
		// compute surface intercept
		openspace::SpiceManager::ref().getSurfaceIntercept(_fovTarget, _spacecraft, _instrumentID,
			_frame, _method, _aberrationCorrection,
			_time, _targetEpoch, _bounds[r], ipoint, ivec, _interceptTag[r]);
		// if not found, use the orthogonal projected point
		if (!_interceptTag[r]) _projectionBounds[r] = orthogonalProjection(_bounds[r]);

		glm::vec4 fovOrigin = glm::vec4(0); //This will have to be fixed once spacecraft is 1:1!

		if (_interceptTag[r]){
			_interceptVector = PowerScaledCoordinate::CreatePowerScaledCoordinate(ivec[0], ivec[1], ivec[2]);
			_interceptVector[3] += 3;
			// INTERCEPTIONS
			insertPoint(_fovBounds, fovOrigin, col_start);
			insertPoint(_fovBounds, _interceptVector.vec4(), col_end);
		}
		else if (_withinFOV){
			// OBJECT IN FOV, NO INTERCEPT FOR THIS FOV-RAY
			insertPoint(_fovBounds, fovOrigin, glm::vec4(0, 0, 1, 1));
			insertPoint(_fovBounds, _projectionBounds[r].vec4(), col_blue);
		}
		else{
			glm::vec4 corner(_bounds[r][0], _bounds[r][1], _bounds[r][2], data.position[3] + 2);
			corner = _spacecraftRotation*corner;
			// NONE OF THE FOV-RAYS INTERCEPT AND NO OBJECT IN FOV
			insertPoint(_fovBounds, fovOrigin, col_gray);
			insertPoint(_fovBounds, corner, glm::vec4(0));
		}
	}
	_interceptTag[_bounds.size()] = _interceptTag[0];
	fovSurfaceIntercept(_interceptTag, _bounds);

	glm::vec3 aim = (_spacecraftRotation * glm::vec4(_boresight, 1)).xyz();
    glm::dvec3 position =
	SpiceManager::ref().targetPosition(_fovTarget,
		    							  _spacecraft,
		    							  _frame,
		    							  _aberrationCorrection,
		    							  _time,
		    							  _lt);
    psc p = PowerScaledCoordinate::CreatePowerScaledCoordinate(position.x, position.y, position.z);
	pss length = p.length();
	if (length[0] < DBL_EPSILON) {
		_drawFOV = false;
		return;
	}
	//if aimed 80 deg away from target, dont draw white square
	if (glm::dot(glm::normalize(aim), glm::normalize(p.vec3())) < 0.2){
		_drawFOV = false;
	}
}

void RenderableFov::render(const RenderData& data) {
	assert(_programObject);
	_programObject->activate();

	_drawFOV = false;
	// setup the data to the shader
	_programObject->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
	_programObject->setUniform("ModelTransform", glm::mat4(1));
	setPscUniforms(_programObject, &data.camera, data.position);
	
	if (openspace::ImageSequencer2::ref().isReady())
		_drawFOV = ImageSequencer2::ref().instrumentActive(_instrumentID);

	if (_drawFOV){
		// update only when time progresses.
		if (_oldTime != _time){
			determineTarget();
			computeColors();
			computeIntercepts(data);
			updateGPU();
		}
		_oldTime = _time;
		_mode = _drawSolid ? GL_TRIANGLE_STRIP : GL_LINES;

		glLineWidth(_lineWidth);
		glBindVertexArray(_fovBoundsVAO);
		glDrawArrays(_mode, 0, static_cast<int>(_vBoundsSize / _stride));
		glBindVertexArray(0);

		if (_drawFOV){
			glLineWidth(2.f);
			glBindVertexArray(_fovPlaneVAO);
			glDrawArrays(GL_LINE_LOOP, 0, static_cast<int>(_vPlaneSize / _stride));
			glBindVertexArray(0);
		}
        glLineWidth(1.f);
	}
	_programObject->deactivate();
}

void RenderableFov::update(const UpdateData& data) {
	_time  = data.time;
	openspace::SpiceManager::ref().getPositionTransformMatrix(_instrumentID, _frame, data.time, _stateMatrix);
	_spacecraftRotation = glm::mat4(1);
	for (int i = 0; i < 3; i++){
		for (int j = 0; j < 3; j++){
			_spacecraftRotation[i][j] = static_cast<float>(_stateMatrix[i][j]);
		}
	}
}

} // namespace openspace
