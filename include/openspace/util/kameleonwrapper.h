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

#ifndef KAMELEONWRAPPER_H_
#define KAMELEONWRAPPER_H_

#include <glm/gtx/std_based_type.hpp>

namespace ccmc {
    class Model;
    class Interpolator;
}

namespace openspace {

struct LinePoint {
	glm::vec3 position;
	glm::vec4 color;

	LinePoint(glm::vec3 pos, glm::vec4 col) {
		position = pos;
		color = col;
	}
};

class KameleonWrapper {
public:

	enum class Model {
		ENLIL,		// Heliosphere
		BATSRUS		// Magnetosphere
	};

	enum class TraceDirection {
		FORWARD = 1,
		BACK 	= -1
	};

	enum class FieldlineEnd {
		NORTH,
		SOUTH,
		OUT
	};

	KameleonWrapper(const std::string& filename, Model model);
	~KameleonWrapper();
	float* getUniformSampledValues(const std::string& var, glm::size3_t outDimensions);
	float* getUniformSampledVectorValues(const std::string& xVar, const std::string& yVar,
			const std::string& zVar, glm::size3_t outDimensions);

	std::vector<std::vector<LinePoint> > getClassifiedFieldLines(const std::string& xVar,
			const std::string& yVar, const std::string& zVar,
			std::vector<glm::vec3> seedPoints, float stepSize);

	std::vector<std::vector<LinePoint> > getFieldLines(const std::string& xVar,
				const std::string& yVar, const std::string& zVar,
				std::vector<glm::vec3> seedPoints, float stepSize, glm::vec4 color);

	std::vector<std::vector<LinePoint> > getLorentzTrajectories(std::vector<glm::vec3> seedPoints,
			glm::vec4 color, float stepsize);

private:
	std::vector<glm::vec3> traceCartesianFieldline(const std::string& xVar,
			const std::string& yVar, const std::string& zVar, glm::vec3 seedPoint,
			float stepSize, TraceDirection direction, FieldlineEnd& end);

	std::vector<glm::vec3> traceLorentzTrajectory(glm::vec3 seedPoint,
			float stepsize, float eCharge);

	void getGridVariables(std::string& x, std::string& y, std::string& z);
	void progressBar(int current, int end);
	glm::vec4 classifyFieldline(FieldlineEnd fEnd, FieldlineEnd bEnd);

	ccmc::Model* _model;
    Model _type;
	ccmc::Interpolator* _interpolator;

	// Model parameters
	float _xMin, _xMax, _yMin, _yMax, _zMin, _zMax;
	std::string _xCoordVar, _yCoordVar, _zCoordVar;

	 // For progressbar
	int _lastiProgress;
};

} // namespace openspace

#endif // KAMELEONWRAPPER_H_
