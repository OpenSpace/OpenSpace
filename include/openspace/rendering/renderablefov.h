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
#include <openspace/query/query.h>

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

	bool isReady() const override;

	void render(const RenderData& data) override;
	void update(const UpdateData& data) override;
 private:
	properties::StringProperty _colorTexturePath; 
	ghoul::opengl::ProgramObject* _programObject;
	ghoul::opengl::Texture* _texture;
	openspace::SceneGraphNode* _targetNode;

	void loadTexture();
	void allocateData();
	void insertPoint(std::vector<float>& arr, psc& p, glm::vec4& c);
	void fovProjection(bool H[], std::vector<glm::dvec3> bounds);

	psc orthogonalProjection(glm::dvec3 camvec);
	psc checkForIntercept(glm::dvec3 ray);
	psc pscInterpolate(psc p0, psc p1, float t);
	psc sphericalInterpolate(glm::dvec3 p0, glm::dvec3 p1, float t);
	
	glm::dvec3 interpolate(glm::dvec3 p0, glm::dvec3 p1, float t);
	glm::dvec3 bisection(glm::dvec3 p1, glm::dvec3 p2, double tolerance);

	double distanceBetweenPoints(psc p1, psc p2);
	// instance variables
	int _nrInserted = 0;
	int _isteps;
	bool _rebuild = false;
	bool _interceptTag[5];
	bool _withinFOV;
	psc _projectionBounds[4];
	psc _interceptVector;

	// spice
	std::string _spacecraft;
	std::string _observer;
	std::string _frame;
	std::string _instrumentID;
	std::string _method;
	std::string _aberrationCorrection;
	std::string _fovTarget;

	glm::dvec3 ipoint, ivec;
	glm::dvec3 _previousHalf;
	glm::vec3 _c; 
	double _r, _g, _b;

	// GPU stuff
	GLuint _vaoID[2] ;
	GLuint _vboID[2] ;
	GLuint _iboID[2];
	GLenum _mode;
	unsigned int _isize[2];
	unsigned int _vsize[2];
	unsigned int _vtotal[2];
	unsigned int _stride[2];
	std::vector<float> _varray1;
	std::vector<float> _varray2;
	int* _iarray1[2];

	void updateData();
	void sendToGPU();

	glm::dmat3 _stateMatrix;

	// time
	double _time = 0;
	double _oldTime = 0;
	int _delta  = 0;
};
}
#endif
// Scrap stuff i need to keep for now (michal)


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
