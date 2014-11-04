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
		, _vaoID1(0)
		, _vboID1(0)
		, _iboID1(0)
		, _vaoID2(0)
		, _vboID2(0)
		, _iboID2(0)
		, _mode(GL_LINES){

		assert(dictionary.getValue(keyBody               , _target));
		assert(dictionary.getValue(keyObserver           , _observer));
		assert(dictionary.getValue(keyFrame              , _frame));
}
void RenderableFov::allocateData(){
	int points = 8;
	_stride = 8;
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

	/*
	//    0                    1                          2                           3                           4              (point nr)
	// 0 1 2 3 | 4 5 6 7 | 8 9 10 11 | 12 13 14 15 | 16 17 18 19 | 20 21 22 23 | 24 25 26 27 | 28 29 30 31 | 32 33 34 35 | 36 37 38 39 |
	//                     8                         16                          24                          32                  (point index)
	*/
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


/*
void RenderableFov::findSegmentSplit(bool tag[]){
	psc start, end, newPoint;
	for (int i = 4; i > 0; i--){
		if (tag[i] != tag[i - 1]){
			int index = findIndx(i, i - 1);
			getIntervalEndpoints(start, end, index);
			newPoint = pscInterpolate(start, end, 0.5f); // point inbetween
			insertPoint(index, newPoint);
		}
	}
}
*/
/*
void RenderableFov::getIntervalEndpoints(psc& p1, psc& p2, int index){
	int secondIndex = index-_stride;
	for (int i = 0; i < 4; i++){
		p2[i] = _varray2[index];
		p1[i] = _varray2[secondIndex];
		index++;
		secondIndex++;
	}
}*/
/*
int RenderableFov::findIndx(unsigned int p1, unsigned int p2) const{
assert(p1 != p2 && _stride > 0);
int idx = (p1 > p2) ? p1*_stride : p2*_stride;
if (idx > _varray2.size()-_stride){
LERROR("Out of bounds for points " << p1 << " and " << p2
<< " will return index 0");
return 0;
}
return idx;
}
*/

void RenderableFov::insertPoint(int index, psc point){
	std::rotate(_varray2.begin(), _varray2.begin() + index, _varray2.end());
	std::reverse(_varray2.begin(), _varray2.end());
		_varray2.push_back(1);
		_varray2.push_back(0);
		_varray2.push_back(0);
		_varray2.push_back(1);
	
	for (int i = 0; i < 4; i++){
		_varray2.push_back(point[3-i]);  // add new point
	}
	std::rotate(_varray2.begin(), _varray2.begin() + index, _varray2.end());
	std::reverse(_varray2.begin(), _varray2.end());

	_vtotal2 += 1;
	_isize2  += 1;
	_vsize2 = _varray2.size();
	//update size etc;
	for (int i = 0; i < _isize2; i++){
		_iarray2[i] = i;
	}
	
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

void RenderableFov::insertPoint(psc& p, glm::vec4& c){
	for (int i = 0; i < 4; i++){
		_varray2.push_back(p[i]);
	}
	for (int i = 0; i < 4; i++){
		_varray2.push_back(c[i]);
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

	float s = (1.f - t)*p0[3] + t*p1[3];

	float x = ((1.f - t)*p0[0] + t*p1[0]);
	float y = ((1.f - t)*p0[1] + t*p1[1]);
	float z = ((1.f - t)*p0[2] + t*p1[2]);

	return PowerScaledCoordinate::PowerScaledCoordinate(x, y, z, s);
}
glm::dvec3 RenderableFov::interpolate(glm::dvec3 p0, glm::dvec3 p1, float t){
	assert(t >= 0 && t <= 1);

	float x = ((1.f - t)*p0[0] + t*p1[0]);
	float y = ((1.f - t)*p0[1] + t*p1[1]);
	float z = ((1.f - t)*p0[2] + t*p1[2]);

	return glm::dvec3(x, y, z);
}

psc RenderableFov::findSegmentIntercept(glm::dvec3 p1, glm::dvec3 p2, double tolerance){
	glm::dvec3 half = interpolate(p1, p2, 0.5f);


	//check if point is on surface
	glm::dvec3 h(half[0], half[1], half[2]);
	double targetEt;
	glm::dvec3 ip, iv;

	bool intercepted = openspace::SpiceManager::ref().getSurfaceIntercept("JUPITER", "NEW HORIZONS", "NH_LORRI",
		                                         "GALACTIC", "ELLIPSOID", "NONE", _time, targetEt, half, ip, iv);
	if (distanceBetweenPoints(_previousHalf, half) < tolerance){
		_previousHalf = glm::dvec3(0);
		return orthogonalProjection(half);
	}
	_previousHalf = half;
	//recursive search
	if (intercepted == false){
		return findSegmentIntercept(p1, half, tolerance);
	}else{
		return findSegmentIntercept(half, p2, tolerance);
	}
}
/*
	[3]---------[4]/[0]
	 |             |
	 |     FOV     |
	 |             |
	[2]-----------[1]

	CASE 1:
	H[i]                    H[i+1]
	[X]---------x...........[O]
	 |        /              :
	 |      /                :
	 |   /                   :
	 x /                     : 
    /:                       :
  /	 :                       :
	[O].....................[O]

*/

void RenderableFov::rebuildFOV(bool H[], std::vector<glm::dvec3> bounds){
	_nrInserted = 0;
	_varray2.clear();// empty the array
	int isteps = 4;
	for (int i = 0; i < 4; i++){
		psc current = orthogonalProjection(bounds[i]);
		insertPoint(current, glm::vec4(1));
		int k = (i + 1 > 3) ? 0 : i + 1;
		psc next = orthogonalProjection(bounds[k]);

		// CASE 1
		if (H[i] == true && H[i + 1] == false){ 
			psc mid = findSegmentIntercept(bounds[i], bounds[k], 0.00001); // very low tolerance factor, might be precision issue
			
			for (int j = 1; j < isteps; j++){
				psc interpolated = pscInterpolate(current, mid, (double)j / isteps);
				insertPoint(interpolated, glm::vec4(1));
			}
		}
		if (H[i] == false && H[i+1] == true){
			psc mid = findSegmentIntercept(bounds[k], bounds[i], 0.0000001);

			for (int j = 1; j < isteps; j++){
				psc interpolated = pscInterpolate(mid, next, (double)j / isteps);
				insertPoint(interpolated, glm::vec4(1));
			}
		}
		// just add remaining points for now... 
		if (H[i] == true && H[i + 1] == true){ // CASE 3
			for (int j = 1; j < isteps; j++){
				psc interpolated = pscInterpolate(current, next, (double)j / isteps);
				insertPoint(interpolated, glm::vec4(1));
			}
		}

	}
	//last point add
	psc last = orthogonalProjection(bounds[0]);
	insertPoint(last, glm::vec4(1));

	_vtotal2 = _nrInserted;
	_isize2  = _nrInserted;
	_vsize2  = _varray2.size();

	//update size etc;
	_iarray2 = new int[_isize2];
	for (int i = 0; i < _isize2; i++){
		_iarray2[i] = i;
	}

	//optimize later
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


void RenderableFov::updateData(){
	glBindBuffer(GL_ARRAY_BUFFER, _vboID1);
	glBufferSubData(GL_ARRAY_BUFFER, 0, _vsize * sizeof(GLfloat), &_varray1[0]);

	glBindBuffer(GL_ARRAY_BUFFER, _vboID2);
	glBufferSubData(GL_ARRAY_BUFFER, 0, _vsize2 * sizeof(GLfloat), &_varray2[0]);
}

psc RenderableFov::orthogonalProjection(glm::dvec3 vecFov){
	glm::dvec3 vecToTarget;
	double lt;
	SpiceManager::ref().getTargetPosition("JUPITER", "NEW HORIZONS", "GALACTIC", "NONE", _time, vecToTarget, lt);
	openspace::SpiceManager::ref().frameConversion(vecFov, "NH_LORRI", "GALACTIC", _time);

	glm::dvec3 p = openspace::SpiceManager::ref().orthogonalProjection(vecToTarget, vecFov);

	psc projection = PowerScaledCoordinate::CreatePowerScaledCoordinate(p[0], p[1], p[2]);
	projection[3] += 3;

	return projection;
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

		bool found = openspace::SpiceManager::ref().getFieldOfView("NH_LORRI", shape, instrument, boresight, bounds);
		if (!found) LERROR("Could not locate instrument"); // fixlater

		float size = 4 * sizeof(float);

		// just some colors to help with debugging
		glm::vec4 origin(0);
		glm::vec4 col_gray(0.3, 0.3, 0.3, 1);
		glm::vec4 col_start(1.00, 0.89, 0.00, 1);
		glm::vec4 col_end(1.00, 0.29, 0.00, 1);
		glm::vec4 col_proj(1, 1, 1, 1);

		int indx = 0;
		int indx2 = 0;

		psc projectionBounds[4];
		bool tags[5];
		
		/**
		* NOTE: THIS CLASS IS UNDER DEVELOPMENT AND THEREFORE NON-OPTIMIZED
		*/

		for (int i = 0; i < 4; i++){
			// might as well take glm. Would be nice if we had just one type to deal with here...
			glm::dvec3 ip, iv;
			double targetEpoch;
			// need to keep it explicit to keep my mind from exploding.
			found = openspace::SpiceManager::ref().getSurfaceIntercept("JUPITER", "NEW HORIZONS", "NH_LORRI",
				"GALACTIC", "ELLIPSOID", "NONE", _time, targetEpoch, bounds[i], ip, iv);

			psc interceptVector = PowerScaledCoordinate::CreatePowerScaledCoordinate(iv[0], iv[1], iv[2]);
			interceptVector[3] += 3;
			glm::vec4 corner(bounds[i][0], bounds[i][1], bounds[i][2], data.position[3]);

			corner = tmp*corner; // manual rotation is a must. 

			if (found){ 
				// INTERCEPTIONS
				tags[i] = true;

				memcpy(&_varray1[indx], glm::value_ptr(origin), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(col_start), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(interceptVector.vec4()), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(col_end), size);
				indx += 4;
			}else{
				// "INFINITE" FOV
				tags[i] = false;

				memcpy(&_varray1[indx], glm::value_ptr(origin), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(col_gray), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(corner), size);
				indx += 4;
				memcpy(&_varray1[indx], glm::value_ptr(glm::vec4(0)), size);
				indx += 4;
			}

			// COMPUTE ORTHOGONAL PROJECTION. -- have to make cosmetic changes to this later. 
				//projectionBounds[i] = orthogonalProjection(bounds[i]);

			/* // failed attempt at aberrated state.
			openspace::SpiceManager::ref().getPositionTransformMatrix("NH_LORRI", "IAU_JUPITER", _time, targetEpoch, _stateMatrix);
			glm::vec4 corner2(bounds[i][0], bounds[i][1], bounds[i][2], data.position[3]);

			corner2 = tmp*corner2;
			*/
		}

		tags[4] = tags[0]; // 0 & 5 same point

		//findSegmentSplit(tags);
		/*
		// draworder r->g->b->y
		glm::vec4 boundary_col[4] = { glm::vec4(1, 0, 0, 1), glm::vec4(0, 1, 0, 1), glm::vec4(0, 0, 1, 1), glm::vec4(1, 1, 0, 1) };

		for (int i = 0; i < 4; i++){
			memcpy(&_varray2[indx2], glm::value_ptr(projectionBounds[i].vec4()), size);
			indx2 += 4;
			memcpy(&_varray2[indx2], glm::value_ptr(boundary_col[i]), size);
			indx2 += 4;
		}
			
		//last point, finish lineloop
		psc lastpoint = orthogonalProjection(bounds[0]);
		memcpy(&_varray2[indx2], glm::value_ptr(lastpoint.vec4()), size);
		indx2 += 4;
		memcpy(&_varray2[indx2], glm::value_ptr(boundary_col[0]), size);
		indx2 += 4;
	*/

		rebuildFOV(tags, bounds);

		updateData();
	}
	_oldTime = _time;
	
	glBindVertexArray(_vaoID1);
	glDrawArrays(_mode, 0, _vtotal);
	glBindVertexArray(0);

	//render points
	glPointSize(10.f);
	glBindVertexArray(_vaoID1);
	glDrawArrays(GL_POINTS, 0, _vtotal);
	glBindVertexArray(0);

	//second vbo
	glBindVertexArray(_vaoID2);
	glDrawArrays(GL_LINE_STRIP, 0, _vtotal2);
	glBindVertexArray(0);
	
	glPointSize(7.f);
	glBindVertexArray(_vaoID2);
	glDrawArrays(GL_POINTS, 0, _vtotal2);
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