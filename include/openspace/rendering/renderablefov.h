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

#ifndef __RenderableFov_H__
#define __RenderableFov_H__

// open space includes
#include <openspace/rendering/renderable.h>

#include <openspace/properties/stringproperty.h>

// ghoul includes
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
//#include <openspace/util/runtimedata.h>

namespace openspace {
class RenderableFov : public Renderable{
public:
	RenderableFov(const ghoul::Dictionary& dictionary);
	~RenderableFov();

	bool initialize() override;
	bool deinitialize() override;

	void render(const RenderData& data) override;
	void update(const UpdateData& data) override;
 private:
	properties::StringProperty _colorTexturePath; 
	ghoul::opengl::ProgramObject* _programObject;
	ghoul::opengl::Texture* _texture;
	
	void loadTexture();
	void allocateData();
	psc orthogonalProjection(glm::dvec3 camvec);

	void printFovArray();

	double distanceBetweenPoints(psc p1, psc p2);
	double distanceBetweenPoints(glm::dvec3 p1, glm::dvec3 p2);

	psc checkForIntercept(glm::dvec3 ray);


	glm::dvec3 bisection(glm::dvec3 p1, glm::dvec3 p2, double tolerance);
	glm::dvec3 _previousHalf;

	void insertPoint(std::vector<float>& arr, psc& p, glm::vec4& c);
	void fovProjection(bool H[], std::vector<glm::dvec3> bounds);
	psc pscInterpolate(psc p0, psc p1, float t);
	glm::dvec3 interpolate(glm::dvec3 p0, glm::dvec3 p1, float t);
	psc sphericalInterpolate(glm::dvec3 p0, glm::dvec3 p1, float t);

	int _nrInserted = 0;
	bool _rebuild = false;
	int _isteps;
	psc _projectionBounds[4];

	// spice
	std::string _spacecraft;
	std::string _observer;
	std::string _frame;
	std::string _instrumentID;
	std::string _method;
	std::string _aberrationCorrection;

	std::string _fovTarget;

	// color
	glm::vec3 _c; 
	double _r, _g, _b;

	GLuint _vaoID1 ;
	GLuint _vboID1 ;
	GLuint _iboID1;
	GLenum _mode;
	unsigned int _isize;
	unsigned int _vsize;
	unsigned int _vtotal;
	unsigned int _stride;
	std::vector<float> _varray1;
	int* _iarray1;

	// second vbo 
	GLuint _vaoID2;
	GLuint _vboID2;
	GLuint _iboID2;
	unsigned int _isize2;
	unsigned int _vsize2;
	unsigned int _vtotal2;

	std::vector<float> _varray2;
	int* _iarray2;

	void updateData();
	void sendToGPU();

	glm::dmat3 _stateMatrix;

	//used for update of trail
	psc _pscpos, _pscvel;
	double _increment;
	double _time = 0;
	double _oldTime = 0;
	int _delta  = 0;


};
}
#endif

/* // idk how we will compute the aberrated state.
double RenderableFov::computeTargetLocalTime(PowerScaledScalar d){
double c = 299792456.075; // m/s
double dt = ( (d[0]*pow(10, d[1])) / c );
double t_local = _time - dt*86400;

std::string localTime;
std::string currentTime;

openspace::SpiceManager::ref().getDateFromET(t_local, localTime);
openspace::SpiceManager::ref().getDateFromET(_time  , currentTime);

std::cout << "time at jupiter : " << localTime << "\time at NH" << currentTime << std::endl;
return t_local;
}*/
