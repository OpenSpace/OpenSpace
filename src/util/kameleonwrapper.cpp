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

#include <ghoul/logging/logmanager.h>
#include <openspace/util/kameleonwrapper.h>

#include <ccmc/Model.h>
#include <ccmc/Interpolator.h>
#include <ccmc/BATSRUS.h>
#include <ccmc/ENLIL.h>

namespace openspace {

std::string _loggerCat = "KameleonWrapper";

KameleonWrapper::KameleonWrapper(const std::string& filename, Model model) {
	switch (model) {
        case Model::BATSRUS:
            _model = new ccmc::BATSRUS();
            if(!_model) LERROR("BATSRUS:Failed to create model instance");
            if (_model->open(filename) != ccmc::FileReader::OK)
				LERROR("BATSRUS:Failed to open "+filename);
            _interpolator = _model->createNewInterpolator();
            if (!_interpolator) LERROR("BATSRUS:Failed to create interpolator");
            break;
        case Model::ENLIL:
            _model = new ccmc::ENLIL();
            if(!_model) LERROR("Failed to create model instance");
            if (_model->open(filename) != ccmc::FileReader::OK)
				LERROR("Failed to open "+filename);
            _interpolator = _model->createNewInterpolator();
            if (!_interpolator) LERROR("Failed to create interpolator");
            break;
        default:
            LERROR("Only the BATSRUS model is supported for now. Sorry.");
	}
}

KameleonWrapper::~KameleonWrapper() {
	delete _model;
	delete _interpolator;
}

float* KameleonWrapper::getUniformSampledValues(const std::string& var, glm::size3_t outDimensions) {
	assert(_model && _interpolator);
	assert(outDimensions.x > 0 && outDimensions.y > 0 && outDimensions.z > 0);
    LDEBUG("getUniformSampledValues");

	int size = outDimensions.x*outDimensions.y*outDimensions.z;
	float* data = new float[size];

	// TODO Check which coordinate system the model use. Currently assumes {x,y,z}
	float xMin = _model->getVariableAttribute("x", "actual_min").getAttributeFloat();
	float xMax = _model->getVariableAttribute("x", "actual_max").getAttributeFloat();
	float yMin = _model->getVariableAttribute("y", "actual_min").getAttributeFloat();
	float yMax = _model->getVariableAttribute("y", "actual_max").getAttributeFloat();
	float zMin = _model->getVariableAttribute("z", "actual_min").getAttributeFloat();
	float zMax = _model->getVariableAttribute("z", "actual_max").getAttributeFloat();
	float varMin = _model->getVariableAttribute(var, "actual_min").getAttributeFloat();
	float varMax = _model->getVariableAttribute(var, "actual_max").getAttributeFloat();

	float stepX = (xMax-xMin)/((float)outDimensions.x-1.0);
	float stepY = (yMax-yMin)/((float)outDimensions.y-1.0);
	float stepZ = (zMax-zMin)/((float)outDimensions.z-1.0);

	// Temporary hack to keep data proportions correct, will give a lot of empty
	// voxels in y and z. TODO Add spacing/voxel dimensions
	float step = std::max(stepX, stepY);
	step = std::max(step, stepZ);

	for (int x = 0; x < outDimensions.x; ++x) {
		unsigned int progress = (unsigned int)(((float)x/(float)outDimensions.x)*100.f);
		if (progress % 10 == 0) {
			std::cout << "Getting data from kameleon: "<< progress << "%     \r" << std::flush;
		}
		for (int y = 0; y < outDimensions.y; ++y) {
			for (int z = 0; z < outDimensions.z; ++z) {

				float xPos = xMin + step*x;
				float yPos = yMin + step*y;
				float zPos = zMin + step*z;
				int index = x + y*outDimensions.x + z*outDimensions.x*outDimensions.y;
				// get interpolated data value for (xPos, yPos, zPos) and scale to [0,1]
				data[index] = (_interpolator->interpolate(var, xPos, yPos, zPos)-varMin)/(varMax-varMin);
			}
		}
	}

	return data;
}

} // namespace openspace

