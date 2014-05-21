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

#include <glm/glm.hpp>
#include <glm/gtx/std_based_type.hpp>

namespace ccmc {
    class Model;
    class Interpolator;
}

namespace openspace {

class KameleonWrapper {
public:

	enum class Model {
		ENLIL,		// Heliosphere
		BATSRUS		// Magnetosphere
	};

	KameleonWrapper(const std::string& filename, Model model);
	~KameleonWrapper();
	float* getUniformSampledValues(const std::string& var, glm::size3_t outDimensions);
	float* getUniformSampledVectorValues(const std::string& xVar, const std::string& yVar, const std::string& zVar, glm::size3_t outDimensions);

private:
	void getGridVariables(std::string& x, std::string& y, std::string& z);
	ccmc::Model* _model;
    Model _type;
	ccmc::Interpolator* _interpolator;
};

} // namespace openspace

#endif // KAMELEONWRAPPER_H_
