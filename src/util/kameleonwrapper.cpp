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
#define _USE_MATH_DEFINES
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <iomanip>

namespace openspace {

std::string _loggerCat = "KameleonWrapper";

KameleonWrapper::KameleonWrapper(const std::string& filename, Model model): _type(model) {
	switch (_type) {
        case Model::BATSRUS:
            _model = new ccmc::BATSRUS();
            if(!_model) LERROR("BATSRUS:Failed to create BATSRUS model instance");
            if (_model->open(filename) != ccmc::FileReader::OK)
				LERROR("BATSRUS:Failed to open "+filename);
            _interpolator = _model->createNewInterpolator();
            if (!_interpolator) LERROR("BATSRUS:Failed to create BATSRUS interpolator");
            break;
        case Model::ENLIL:
            _model = new ccmc::ENLIL();
            if(!_model) LERROR("Failed to create ENLIL model instance");
            if (_model->open(filename) != ccmc::FileReader::OK)
				LERROR("Failed to open "+filename);
            _interpolator = _model->createNewInterpolator();
            if (!_interpolator) LERROR("Failed to create ENLIL interpolator");
            break;
        default:
            LERROR("No valid model type provided!");
	}
	_lastiProgress = -1; // for progressbar
}

KameleonWrapper::~KameleonWrapper() {
	delete _model;
	delete _interpolator;
}

float* KameleonWrapper::getUniformSampledValues(const std::string& var, glm::size3_t outDimensions) {
	assert(_model && _interpolator);
	assert(outDimensions.x > 0 && outDimensions.y > 0 && outDimensions.z > 0);
    assert(_type == Model::ENLIL || _type == Model::BATSRUS);
    LINFO("Loading variable " << var << " from CDF data with a uniform sampling");

	int size = outDimensions.x*outDimensions.y*outDimensions.z;
	float* data = new float[size];

	std::string v_x, v_y, v_z;
	getGridVariables(v_x, v_y, v_z);
    LDEBUG("Using coordinate system variables: " << v_x << ", " << v_y << ", " << v_z);
    
	float xMin =    _model->getVariableAttribute(v_x, "actual_min").getAttributeFloat();
	float xMax =    _model->getVariableAttribute(v_x, "actual_max").getAttributeFloat();
	float yMin =    _model->getVariableAttribute(v_y, "actual_min").getAttributeFloat();
	float yMax =    _model->getVariableAttribute(v_y, "actual_max").getAttributeFloat();
	float zMin =    _model->getVariableAttribute(v_z, "actual_min").getAttributeFloat();
	float zMax =    _model->getVariableAttribute(v_z, "actual_max").getAttributeFloat();
	float varMin =  _model->getVariableAttribute(var, "actual_min").getAttributeFloat();
	float varMax =  _model->getVariableAttribute(var, "actual_max").getAttributeFloat();

	float stepX = (xMax-xMin)/(static_cast<float>(outDimensions.x));
	float stepY = (yMax-yMin)/(static_cast<float>(outDimensions.y));
	float stepZ = (zMax-zMin)/(static_cast<float>(outDimensions.z));
    
    LDEBUG(v_x << "Min: " << xMin);
    LDEBUG(v_x << "Max: " << xMax);
    LDEBUG(v_y << "Min: " << yMin);
    LDEBUG(v_y << "Max: " << yMax);
    LDEBUG(v_z << "Min: " << zMin);
    LDEBUG(v_z << "Max: " << zMax);
    LDEBUG(var << "Min: " << varMin);
    LDEBUG(var << "Max: " << varMax);
    
    for (int x = 0; x < outDimensions.x; ++x) {
    	progressBar(x, outDimensions.x);
        
		for (int y = 0; y < outDimensions.y; ++y) {
			for (int z = 0; z < outDimensions.z; ++z) {
                
                int index = x + y*outDimensions.x + z*outDimensions.x*outDimensions.y;
                
                if(_type == Model::BATSRUS) {
                    float xPos = xMin + stepX*x;
                    float yPos = yMin + stepY*y;
                    float zPos = zMin + stepZ*z;
                    
                    // get interpolated data value for (xPos, yPos, zPos)
                    float value = _interpolator->interpolate(var, xPos, yPos, zPos);
                    
                    // scale to [0,1]
                    data[index] = (value-varMin)/(varMax-varMin);
                } else if (_type == Model::ENLIL) {
                    
                    // Put r in the [0..sqrt(3)] range
                    float rNorm = sqrt(3.0)*(float)x/(float)(outDimensions.x-1);
                    
                    // Put theta in the [0..PI] range
                    float thetaNorm = M_PI*(float)y/(float)(outDimensions.y-1);
                    
                    // Put phi in the [0..2PI] range
                    float phiNorm = 2.0*M_PI*(float)z/(float)(outDimensions.z-1);
                    
                    // Go to physical coordinates before sampling
                    float rPh = xMin + rNorm*(xMax-xMin);
                    float thetaPh = thetaNorm;
                    // phi range needs to be mapped to the slightly different model
                    // range to avoid gaps in the data Subtract a small term to
                    // avoid rounding errors when comparing to phiMax.
                    float phiPh = zMin + phiNorm/(2.0*M_PI)*(zMax-zMin-0.000001);
                    
                    float varValue = 0.f;
                    // See if sample point is inside domain
                    if (rPh < xMin || rPh > xMax || thetaPh < yMin ||
                        thetaPh > yMax || phiPh < zMin || phiPh > zMax) {
                        if (phiPh > zMax) {
                            std::cout << "Warning: There might be a gap in the data\n";
                        }
                        // Leave values at zero if outside domain
                    } else { // if inside
                        
                        // ENLIL CDF specific hacks!
                        // Convert from meters to AU for interpolator
                        rPh /= ccmc::constants::AU_in_meters;
                        // Convert from colatitude [0, pi] rad to latitude [-90, 90] degrees
                        thetaPh = -thetaPh*180.f/M_PI+90.f;
                        // Convert from [0, 2pi] rad to [0, 360] degrees
                        phiPh = phiPh*180.f/M_PI;
                        // Sample
                        varValue = _interpolator->interpolate(var, rPh, thetaPh, phiPh);
                    }

                    data[index] = (varValue-varMin)/(varMax-varMin);
                }
			}
		}
	}
    std::cout << std::endl;
    LINFO("Done!");

	return data;
}

float* KameleonWrapper::getUniformSampledVectorValues(const std::string& xVar, const std::string& yVar, const std::string& zVar, glm::size3_t outDimensions) {
	assert(_model && _interpolator);
	assert(outDimensions.x > 0 && outDimensions.y > 0 && outDimensions.z > 0);
	assert(_type == Model::ENLIL || _type == Model::BATSRUS);
	LINFO("Loading variables " << xVar << " " << yVar << " " << zVar << " from CDF data with a uniform sampling");

	int channels = 4;
	int size = channels*outDimensions.x*outDimensions.y*outDimensions.z;
	float* data = new float[size];

//	memset(data, 0.0, sizeof(data));



	std::string v_x, v_y, v_z;
	getGridVariables(v_x, v_y, v_z);
	LDEBUG("Using coordinate system variables: " << v_x << ", " << v_y << ", " << v_z);

	float xMin =    _model->getVariableAttribute(v_x, "actual_min").getAttributeFloat();
	float xMax =    _model->getVariableAttribute(v_x, "actual_max").getAttributeFloat();
	float yMin =    _model->getVariableAttribute(v_y, "actual_min").getAttributeFloat();
	float yMax =    _model->getVariableAttribute(v_y, "actual_max").getAttributeFloat();
	float zMin =    _model->getVariableAttribute(v_z, "actual_min").getAttributeFloat();
	float zMax =    _model->getVariableAttribute(v_z, "actual_max").getAttributeFloat();
	float varXMin =  _model->getVariableAttribute(xVar, "actual_min").getAttributeFloat();
	float varXMax =  _model->getVariableAttribute(xVar, "actual_max").getAttributeFloat();
	float varYMin =  _model->getVariableAttribute(yVar, "actual_min").getAttributeFloat();
	float varYMax =  _model->getVariableAttribute(yVar, "actual_max").getAttributeFloat();
	float varZMin =  _model->getVariableAttribute(zVar, "actual_min").getAttributeFloat();
	float varZMax =  _model->getVariableAttribute(zVar, "actual_max").getAttributeFloat();

	float stepX = (xMax-xMin)/(static_cast<float>(outDimensions.x));
	float stepY = (yMax-yMin)/(static_cast<float>(outDimensions.y));
	float stepZ = (zMax-zMin)/(static_cast<float>(outDimensions.z));

	LDEBUG(v_x << "Min: " << xMin);
	LDEBUG(v_x << "Max: " << xMax);
	LDEBUG(v_y << "Min: " << yMin);
	LDEBUG(v_y << "Max: " << yMax);
	LDEBUG(v_z << "Min: " << zMin);
	LDEBUG(v_z << "Max: " << zMax);
	LDEBUG(xVar << "Min: " << varXMin);
	LDEBUG(xVar << "Max: " << varXMax);
	LDEBUG(yVar << "Min: " << varYMin);
	LDEBUG(yVar << "Max: " << varYMax);
	LDEBUG(zVar << "Min: " << varZMin);
	LDEBUG(zVar << "Max: " << varZMax);

	for (int x = 0; x < outDimensions.x; ++x) {
		progressBar(x, outDimensions.x);

		for (int y = 0; y < outDimensions.y; ++y) {
			for (int z = 0; z < outDimensions.z; ++z) {

				int index = x*channels + y*channels*outDimensions.x + z*channels*outDimensions.x*outDimensions.y;

				if(_type == Model::BATSRUS) {
					float xPos = xMin + stepX*x;
					float yPos = yMin + stepY*y;
					float zPos = zMin + stepZ*z;

					// get interpolated data value for (xPos, yPos, zPos)
					float xValue = _interpolator->interpolate(xVar, xPos, yPos, zPos);
					float yValue = _interpolator->interpolate(yVar, xPos, yPos, zPos);
					float zValue = _interpolator->interpolate(zVar, xPos, yPos, zPos);

					// scale to [0,1]
					data[index] 	= (xValue-varXMin)/(varXMax-varXMin); // R
					data[index + 1] = (yValue-varYMin)/(varYMax-varYMin); // G
					data[index + 2] = (zValue-varZMin)/(varZMax-varZMin); // B
					data[index + 3] = 1.0; // GL_RGB refuses to work. Workaround by doing a GL_RGBA with hardcoded alpha
				} else {
					LERROR("Only BATSRUS supported for getUniformSampledVectorValues (for now)");
				}
			}
		}
	}
	std::cout << std::endl;
	LINFO("Done!");

	return data;
}

void KameleonWrapper::getGridVariables(std::string& x, std::string& y, std::string& z) {
	// get the grid system string
	std::string gridSystem = _model->getGlobalAttribute("grid_system_1").getAttributeString();

	// remove leading and trailing brackets
	gridSystem = gridSystem.substr(1,gridSystem.length()-2);

	// remove all whitespaces
	gridSystem.erase(remove_if(gridSystem.begin(), gridSystem.end(), isspace), gridSystem.end());

	// replace all comma signs with whitespaces
	std::replace( gridSystem.begin(), gridSystem.end(), ',', ' ');

	// tokenize
	std::istringstream iss(gridSystem);
	std::vector<std::string> tokens{std::istream_iterator<std::string>{iss},std::istream_iterator<std::string>{}};

	// validate
	if (tokens.size() != 3) LERROR("Something went wrong");

	x = tokens.at(0);
	y = tokens.at(1);
	z = tokens.at(2);
}

float* KameleonWrapper::getFieldLines(const std::string& xVar,
		const std::string& yVar, const std::string& zVar,
		glm::size3_t outDimensions, std::vector<glm::vec3> seedPoints) {
	assert(_model && _interpolator);
	assert(outDimensions.x > 0 && outDimensions.y > 0 && outDimensions.z > 0);
	assert(_type == Model::ENLIL || _type == Model::BATSRUS);
	LINFO("Creating " << seedPoints.size() << " fieldlines from variables " << xVar << " " << yVar << " " << zVar);

	std::string v_x, v_y, v_z;
	getGridVariables(v_x, v_y, v_z);
	LDEBUG("Using coordinate system variables: " << v_x << ", " << v_y << ", " << v_z);

	float xMin = _model->getVariableAttribute(v_x, "actual_min").getAttributeFloat();
	float xMax = _model->getVariableAttribute(v_x, "actual_max").getAttributeFloat();
	float yMin = _model->getVariableAttribute(v_y, "actual_min").getAttributeFloat();
	float yMax = _model->getVariableAttribute(v_y, "actual_max").getAttributeFloat();
	float zMin = _model->getVariableAttribute(v_z, "actual_min").getAttributeFloat();
	float zMax = _model->getVariableAttribute(v_z, "actual_max").getAttributeFloat();

	float stepSize = 0.001;
	float stepX = stepSize*(xMax-xMin)/(static_cast<float>(outDimensions.x));
	float stepY = stepSize*(yMax-yMin)/(static_cast<float>(outDimensions.y));
	float stepZ = stepSize*(zMax-zMin)/(static_cast<float>(outDimensions.z));

	int size = outDimensions.x*outDimensions.y*outDimensions.z;
	float* data = new float[size];
	glm::vec3 dir, pos;

	int highNumber = 100000;

	for (int i = 0; i < seedPoints.size(); ++i) {
		progressBar(i, seedPoints.size());
		pos = seedPoints.at(i);
		int avoidInfLoopPlz = 0;
		while (pos.x < xMax && pos.x > xMin &&
			   pos.y < yMax && pos.y > yMin &&
			   pos.z < zMax && pos.z > zMin) {

			// Save position
			int vPosX = std::floor(outDimensions.x*(pos.x-xMin)/(xMax-xMin));
			int vPosY = std::floor(outDimensions.y*(pos.y-yMin)/(yMax-yMin));
			int vPosZ = std::floor(outDimensions.z*(pos.z-zMin)/(zMax-zMin));
			int index = vPosX + vPosY*outDimensions.x + vPosZ*outDimensions.x*outDimensions.y;
			data[index] = 1.0;

			// Calculate the next position
			dir.x = _interpolator->interpolate(xVar, pos.x, pos.y, pos.z);
			dir.y = _interpolator->interpolate(yVar, pos.x, pos.y, pos.z);
			dir.z = _interpolator->interpolate(zVar, pos.x, pos.y, pos.z);
			pos = glm::vec3(stepX*dir.x+pos.x, stepY*dir.y+pos.y, stepZ*dir.z+pos.z);
			++avoidInfLoopPlz;

			if (avoidInfLoopPlz > highNumber) {
				LDEBUG("Inf loop averted");
				break;
			}
		}
	}
	return data;
}

void KameleonWrapper::progressBar(int current, int end) {
	float progress = static_cast<float>(current) / static_cast<float>(end-1);
	int iprogress = static_cast<int>(progress*100.0f);
	int barWidth = 70;
	if (iprogress != _lastiProgress) {
		int pos = barWidth * progress;
		int eqWidth = pos+1;
		int spWidth = barWidth - pos + 2;
		std::cout   << "[" << std::setfill('=') << std::setw(eqWidth)
		<< ">" << std::setfill(' ') << std::setw(spWidth)
		<< "] " << iprogress << " %  \r" << std::flush;
	}
	_lastiProgress = iprogress;
}

} // namespace openspace

