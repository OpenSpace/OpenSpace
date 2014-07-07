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
#include <stdlib.h>

#include <glm/gtx/rotate_vector.hpp>

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

	getGridVariables(_xCoordVar, _yCoordVar, _zCoordVar);
	_xMin = _model->getVariableAttribute(_xCoordVar, "actual_min").getAttributeFloat();
	_xMax = _model->getVariableAttribute(_xCoordVar, "actual_max").getAttributeFloat();
	_yMin = _model->getVariableAttribute(_yCoordVar, "actual_min").getAttributeFloat();
	_yMax = _model->getVariableAttribute(_yCoordVar, "actual_max").getAttributeFloat();
	_zMin = _model->getVariableAttribute(_zCoordVar, "actual_min").getAttributeFloat();
	_zMax = _model->getVariableAttribute(_zCoordVar, "actual_max").getAttributeFloat();

	_lastiProgress = -1; // For progressbar
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
	double* doubleData = new double[size];

	double varMin =  _model->getVariableAttribute(var, "actual_min").getAttributeFloat();
	double varMax =  _model->getVariableAttribute(var, "actual_max").getAttributeFloat();
	
	double stepX = (_xMax-_xMin)/(static_cast<double>(outDimensions.x));
	double stepY = (_yMax-_yMin)/(static_cast<double>(outDimensions.y));
	double stepZ = (_zMax-_zMin)/(static_cast<double>(outDimensions.z));
    
    LDEBUG(var << "Min: " << varMin);
    LDEBUG(var << "Max: " << varMax);

    // HISTOGRAM
    const int bins = 200;
    const float truncLim = 0.9;
    std::vector<int> histogram (bins,0);
    auto mapToHistogram = [varMin, varMax, bins](double val) {
    	double zeroToOne = (val-varMin)/(varMax-varMin);
    	zeroToOne *= static_cast<double>(bins);
    	return static_cast<int>(zeroToOne);
    };
    
    for (int x = 0; x < outDimensions.x; ++x) {
    	progressBar(x, outDimensions.x);
        
		for (int y = 0; y < outDimensions.y; ++y) {
			for (int z = 0; z < outDimensions.z; ++z) {
                
                int index = x + y*outDimensions.x + z*outDimensions.x*outDimensions.y;
                
                if(_type == Model::BATSRUS) {
                    double xPos = _xMin + stepX*x;
                    double yPos = _yMin + stepY*y;
                    double zPos = _zMin + stepZ*z;
                    
                    // get interpolated data value for (xPos, yPos, zPos)
                    // swap yPos and zPos because model has Z as up
                    double value = _interpolator->interpolate(var, xPos, zPos, yPos);
                    
                    // scale to [0,1]
                    //doubleData[index] = (value-varMin)/(varMax-varMin);
                    doubleData[index] = value;
					histogram[mapToHistogram(value)]++;
                } else if (_type == Model::ENLIL) {
                    
                    // Put r in the [0..sqrt(3)] range
                    double rNorm = sqrt(3.0)*(double)x/(double)(outDimensions.x-1);
                    
                    // Put theta in the [0..PI] range
                    double thetaNorm = M_PI*(double)y/(double)(outDimensions.y-1);
                    
                    // Put phi in the [0..2PI] range
                    double phiNorm = 2.0*M_PI*(double)z/(double)(outDimensions.z-1);
                    
                    // Go to physical coordinates before sampling
                    double rPh = _xMin + rNorm*(_xMax-_xMin);
                    double thetaPh = thetaNorm;
                    // phi range needs to be mapped to the slightly different model
                    // range to avoid gaps in the data Subtract a small term to
                    // avoid rounding errors when comparing to phiMax.
                    double phiPh = _zMin + phiNorm/(2.0*M_PI)*(_zMax-_zMin-0.000001);
                    
                    double varValue = 0.f;
                    // See if sample point is inside domain
                    if (rPh < _xMin || rPh > _xMax || thetaPh < _yMin ||
                        thetaPh > _yMax || phiPh < _zMin || phiPh > _zMax) {
                        if (phiPh > _zMax) {
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

                    doubleData[index] = (varValue-varMin)/(varMax-varMin);
                }
			}
		}
	}
    std::cout << std::endl;
    LINFO("Done!");

    int sum = 0;
    int stop;
    const int sumuntil = size * truncLim;
    for(int i = 0; i < bins-1; ++i) {
    	sum += histogram[i];
    	if(sum + histogram[i+1] > sumuntil) {
    		stop = i;
    		LDEBUG("====================");
    		break;
    	}
    	LDEBUG(histogram[i]);
    }

    double dist = varMax - varMin;
    dist = (dist / static_cast<double>(bins)) * static_cast<double>(stop);

	varMax = varMin + dist;
    for(int i = 0; i < size; ++i) {
    	double normalizedVal = (doubleData[i]-varMin)/(varMax-varMin);

    	data[i] = static_cast<float>(glm::clamp(normalizedVal, 0.0, 1.0));
    }
    delete[] doubleData;

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

	float varXMin =  _model->getVariableAttribute(xVar, "actual_min").getAttributeFloat();
	float varXMax =  _model->getVariableAttribute(xVar, "actual_max").getAttributeFloat();
	float varYMin =  _model->getVariableAttribute(yVar, "actual_min").getAttributeFloat();
	float varYMax =  _model->getVariableAttribute(yVar, "actual_max").getAttributeFloat();
	float varZMin =  _model->getVariableAttribute(zVar, "actual_min").getAttributeFloat();
	float varZMax =  _model->getVariableAttribute(zVar, "actual_max").getAttributeFloat();

	float stepX = (_xMax-_xMin)/(static_cast<float>(outDimensions.x));
	float stepY = (_yMax-_yMin)/(static_cast<float>(outDimensions.y));
	float stepZ = (_zMax-_zMin)/(static_cast<float>(outDimensions.z));

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
					float xPos = _xMin + stepX*x;
					float yPos = _yMin + stepY*y;
					float zPos = _zMin + stepZ*z;

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
					return data;
				}
			}
		}
	}
	std::cout << std::endl;
	LINFO("Done!");

	return data;
}

std::vector<std::vector<LinePoint> > KameleonWrapper::getClassifiedFieldLines(
		const std::string& xVar, const std::string& yVar,
		const std::string& zVar, std::vector<glm::vec3> seedPoints,
		float stepSize ) {
	assert(_model && _interpolator);
	assert(_type == Model::ENLIL || _type == Model::BATSRUS);
	LINFO("Creating " << seedPoints.size() << " fieldlines from variables " << xVar << " " << yVar << " " << zVar);

	std::vector<glm::vec3> fLine, bLine;
	std::vector<std::vector<LinePoint> > fieldLines;
	glm::vec4 color;
	FieldlineEnd forwardEnd, backEnd;

	if (_type == Model::BATSRUS) {
		for (glm::vec3 seedPoint : seedPoints) {
			fLine = traceCartesianFieldline(xVar, yVar, zVar, seedPoint, stepSize, TraceDirection::FORWARD, forwardEnd);
			bLine = traceCartesianFieldline(xVar, yVar, zVar, seedPoint, stepSize, TraceDirection::BACK, backEnd);

			bLine.insert(bLine.begin(), fLine.rbegin(), fLine.rend());

			// classify
			color = classifyFieldline(forwardEnd, backEnd);

			// write colors
			std::vector<LinePoint> line;
			for (glm::vec3 position : bLine) {
				line.push_back(LinePoint(position, color));
			}

			fieldLines.push_back(line);
		}
	} else {
		LERROR("Fieldlines are only supported for BATSRUS model");
	}

	return fieldLines;
}

std::vector<std::vector<LinePoint> > KameleonWrapper::getFieldLines(
		const std::string& xVar, const std::string& yVar,
		const std::string& zVar, std::vector<glm::vec3> seedPoints,
		float stepSize, glm::vec4 color ) {
	assert(_model && _interpolator);
	assert(_type == Model::ENLIL || _type == Model::BATSRUS);
	LINFO("Creating " << seedPoints.size() << " fieldlines from variables " << xVar << " " << yVar << " " << zVar);

	std::vector<glm::vec3> fLine, bLine;
	std::vector<std::vector<LinePoint> > fieldLines;
	FieldlineEnd forwardEnd, backEnd;

	if (_type == Model::BATSRUS) {
		for (glm::vec3 seedPoint : seedPoints) {
			fLine = traceCartesianFieldline(xVar, yVar, zVar, seedPoint, stepSize, TraceDirection::FORWARD, forwardEnd);
			bLine = traceCartesianFieldline(xVar, yVar, zVar, seedPoint, stepSize, TraceDirection::BACK, backEnd);

			bLine.insert(bLine.begin(), fLine.rbegin(), fLine.rend());

			// write colors
			std::vector<LinePoint> line;
			for (glm::vec3 position : bLine) {
				line.push_back(LinePoint(position, color));
			}

			fieldLines.push_back(line);
		}
	} else {
		LERROR("Fieldlines are only supported for BATSRUS model");
	}

	return fieldLines;
}

std::vector<std::vector<LinePoint> > KameleonWrapper::getLorentzTrajectories(
		std::vector<glm::vec3> seedPoints, glm::vec4 color, float stepsize) {
	LINFO("Creating " << seedPoints.size() << " Lorentz force trajectories");

	std::vector<std::vector<LinePoint> > trajectories;
	std::vector<glm::vec3> plusTraj, minusTraj;

	for (auto seedPoint : seedPoints) {
		plusTraj = traceLorentzTrajectory(seedPoint, stepsize, 1.0);
		minusTraj = traceLorentzTrajectory(seedPoint, stepsize, -1.0);

		minusTraj.insert(minusTraj.begin(), plusTraj.rbegin(), plusTraj.rend());

		// write colors
		std::vector<LinePoint> trajectory;
		for (glm::vec3 position : minusTraj) {
			trajectory.push_back(LinePoint(position, color));
		}
		trajectories.push_back(trajectory);
	}

	return trajectories;
}

glm::vec3 KameleonWrapper::getModelBarycenterOffset() {
	glm::vec3 offset;
	offset.x = _xMin+(std::abs(_xMin)+std::abs(_xMax))/2.0f;
	offset.y = _yMin+(std::abs(_yMin)+std::abs(_yMax))/2.0f;
	offset.z = _zMin+(std::abs(_zMin)+std::abs(_zMax))/2.0f;
	return offset;
}

std::vector<glm::vec3> KameleonWrapper::traceCartesianFieldline(
		const std::string& xVar, const std::string& yVar,
		const std::string& zVar, glm::vec3 seedPoint,
		float stepSize, TraceDirection direction, FieldlineEnd& end) {

	glm::vec3 color, pos, k1, k2, k3, k4;
	std::vector<glm::vec3> line;
	float stepX, stepY, stepZ;
	int numSteps = 0, maxSteps = 5000;
	pos = seedPoint;

	_model->loadVariable(xVar);
	_model->loadVariable(yVar);
	_model->loadVariable(zVar);

	long int xID = _model->getVariableID(xVar);
	long int yID = _model->getVariableID(yVar);
	long int zID = _model->getVariableID(zVar);

	// While we are inside the models boundries and not inside earth
	while ((pos.x < _xMax && pos.x > _xMin && pos.y < _yMax && pos.y > _yMin &&
			pos.z < _zMax && pos.z > _zMin) && !(pos.x*pos.x + pos.y*pos.y + pos.z*pos.z < 1.0)) {

		// Save position. Model has +Z as up
		line.push_back(glm::vec3(pos.x, pos.z, pos.y));

		// Calculate new position with Runge-Kutta 4th order
		k1.x = _interpolator->interpolate(xID, pos.x, pos.y, pos.z, stepX, stepY, stepZ);
		k1.y = _interpolator->interpolate(yID, pos.x, pos.y, pos.z);
		k1.z = _interpolator->interpolate(zID, pos.x, pos.y, pos.z);
		k1 = (float)direction*glm::normalize(k1);
		stepX=stepX*stepSize, stepY=stepY*stepSize, stepZ=stepZ*stepSize;
		k2.x = _interpolator->interpolate(xID, pos.x+(stepX/2.0)*k1.x, pos.y+(stepY/2.0)*k1.y, pos.z+(stepZ/2.0)*k1.z);
		k2.y = _interpolator->interpolate(yID, pos.x+(stepX/2.0)*k1.x, pos.y+(stepY/2.0)*k1.y, pos.z+(stepZ/2.0)*k1.z);
		k2.z = _interpolator->interpolate(zID, pos.x+(stepX/2.0)*k1.x, pos.y+(stepY/2.0)*k1.y, pos.z+(stepZ/2.0)*k1.z);
		k2 = (float)direction*glm::normalize(k2);
		k3.x = _interpolator->interpolate(xID, pos.x+(stepX/2.0)*k2.x, pos.y+(stepY/2.0)*k2.y, pos.z+(stepZ/2.0)*k2.z);
		k3.y = _interpolator->interpolate(yID, pos.x+(stepX/2.0)*k2.x, pos.y+(stepY/2.0)*k2.y, pos.z+(stepZ/2.0)*k2.z);
		k3.z = _interpolator->interpolate(zID, pos.x+(stepX/2.0)*k2.x, pos.y+(stepY/2.0)*k2.y, pos.z+(stepZ/2.0)*k2.z);
		k3 = (float)direction*glm::normalize(k3);
		k4.x = _interpolator->interpolate(xID, pos.x+stepX*k3.x, pos.y+stepY*k3.y, pos.z+stepZ*k3.z);
		k4.y = _interpolator->interpolate(yID, pos.x+stepX*k3.x, pos.y+stepY*k3.y, pos.z+stepZ*k3.z);
		k4.z = _interpolator->interpolate(zID, pos.x+stepX*k3.x, pos.y+stepY*k3.y, pos.z+stepZ*k3.z);
		k4 = (float)direction*glm::normalize(k4);
		pos.x = pos.x + (stepX/6.0)*(k1.x + 2.0*k2.x + 2.0*k3.x + k4.x);
		pos.y = pos.y + (stepY/6.0)*(k1.y + 2.0*k2.y + 2.0*k3.y + k4.y);
		pos.z = pos.z + (stepZ/6.0)*(k1.z + 2.0*k2.z + 2.0*k3.z + k4.z);

		++numSteps;
		if (numSteps > maxSteps) {
			LDEBUG("Max number of steps taken (" << maxSteps <<")");
			break;
		}
	}
	// Save last position. Model has +Z as up
	line.push_back(glm::vec3(pos.x, pos.z, pos.y));

	if (pos.z > 0.0 && (pos.x*pos.x + pos.y*pos.y + pos.z*pos.z < 1.0))
		end = FieldlineEnd::NORTH;
	else if (pos.z < 0.0 && (pos.x*pos.x + pos.y*pos.y + pos.z*pos.z < 1.0))
		end = FieldlineEnd::SOUTH;
	else
		end = FieldlineEnd::OUT;

	return line;
}

std::vector<glm::vec3> KameleonWrapper::traceLorentzTrajectory(glm::vec3 seedPoint,
		float stepsize, float eCharge) {
	glm::vec3 B, E, v0, k1, k2, k3, k4, sPos, tmpV;
	float stepX = stepsize, stepY = stepsize, stepZ = stepsize;

	long int bxID = _model->getVariableID("bx");
	long int byID = _model->getVariableID("by");
	long int bzID = _model->getVariableID("bz");
	long int jxID = _model->getVariableID("jx");
	long int jyID = _model->getVariableID("jy");
	long int jzID = _model->getVariableID("jz");

	std::vector<glm::vec3> trajectory;
	glm::vec3 pos = seedPoint;
	int numSteps = 0, maxSteps = 5000;
	v0.x = _interpolator->interpolate("ux", pos.x, pos.y, pos.z);
	v0.y = _interpolator->interpolate("uy", pos.x, pos.y, pos.z);
	v0.z = _interpolator->interpolate("uz", pos.x, pos.y, pos.z);
	v0 = glm::normalize(v0);

	// While we are inside the models boundries and not inside earth
	while ((pos.x < _xMax && pos.x > _xMin && pos.y < _yMax && pos.y > _yMin &&
			pos.z < _zMax && pos.z > _zMin) && !(pos.x*pos.x + pos.y*pos.y + pos.z*pos.z < 1.0)) {

		// Save position. Model has +Z as up
		trajectory.push_back(glm::vec3(pos.x, pos.z, pos.y));

		// Calculate new position with Lorentz force quation and Runge-Kutta 4th order
		B.x = _interpolator->interpolate(bxID, pos.x, pos.y, pos.z);
		B.y = _interpolator->interpolate(byID, pos.x, pos.y, pos.z);
		B.z = _interpolator->interpolate(bzID, pos.x, pos.y, pos.z);
		E.x = _interpolator->interpolate(jxID, pos.x, pos.y, pos.z);
		E.y = _interpolator->interpolate(jyID, pos.x, pos.y, pos.z);
		E.z = _interpolator->interpolate(jzID, pos.x, pos.y, pos.z);
		k1 = eCharge*(E + glm::cross(v0, B));
		k1 = glm::normalize(k1);

		sPos = glm::vec3(	pos.x+(stepX/2.0)*v0.x+(stepX*stepX/8.0)*k1.x,
							pos.y+(stepY/2.0)*v0.y+(stepY*stepY/8.0)*k1.y,
							pos.z+(stepZ/2.0)*v0.z+(stepZ*stepZ/8.0)*k1.z);
		B.x = _interpolator->interpolate(bxID, sPos.x, sPos.y, sPos.z);
		B.y = _interpolator->interpolate(byID, sPos.x, sPos.y, sPos.z);
		B.z = _interpolator->interpolate(bzID, sPos.x, sPos.y, sPos.z);
		E.x = _interpolator->interpolate(jxID, sPos.x, sPos.y, sPos.z);
		E.y = _interpolator->interpolate(jyID, sPos.x, sPos.y, sPos.z);
		E.z = _interpolator->interpolate(jzID, sPos.x, sPos.y, sPos.z);
		tmpV = v0+(stepX/2.0f)*k1;
		k2 = eCharge*(E + glm::cross(tmpV, B));
		k2 = glm::normalize(k2);

		B.x = _interpolator->interpolate(bxID, sPos.x, sPos.y, sPos.z);
		B.y = _interpolator->interpolate(byID, sPos.x, sPos.y, sPos.z);
		B.z = _interpolator->interpolate(bzID, sPos.x, sPos.y, sPos.z);
		E.x = _interpolator->interpolate(jxID, sPos.x, sPos.y, sPos.z);
		E.y = _interpolator->interpolate(jyID, sPos.x, sPos.y, sPos.z);
		E.z = _interpolator->interpolate(jzID, sPos.x, sPos.y, sPos.z);
		tmpV = v0+(stepX/2.0f)*k2;
		k3 = eCharge*(E + glm::cross(tmpV, B));
		k3 = glm::normalize(k3);

		sPos = glm::vec3(	pos.x+stepX*v0.x+(stepX*stepX/2.0)*k1.x,
							pos.y+stepY*v0.y+(stepY*stepY/2.0)*k1.y,
							pos.z+stepZ*v0.z+(stepZ*stepZ/2.0)*k1.z);
		B.x = _interpolator->interpolate(bxID, sPos.x, sPos.y, sPos.z);
		B.y = _interpolator->interpolate(byID, sPos.x, sPos.y, sPos.z);
		B.z = _interpolator->interpolate(bzID, sPos.x, sPos.y, sPos.z);
		E.x = _interpolator->interpolate(jxID, sPos.x, sPos.y, sPos.z);
		E.y = _interpolator->interpolate(jyID, sPos.x, sPos.y, sPos.z);
		E.z = _interpolator->interpolate(jzID, sPos.x, sPos.y, sPos.z);
		tmpV = v0+stepX*k3;
		k4 = eCharge*(E + glm::cross(tmpV, B));
		k4 = glm::normalize(k4);

		pos.x = pos.x + stepX*v0.x + (stepX*stepX/6.0)*(k1.x + k2.x + k3.x);
		pos.y = pos.y + stepY*v0.y + (stepY*stepY/6.0)*(k1.y + k2.y + k3.y);
		pos.z = pos.z + stepZ*v0.z + (stepZ*stepZ/6.0)*(k1.z + k2.z + k3.z);

		v0.x = v0.x + (stepX/6.0)*(k1.x + 2.0*k2.x + 2.0*k3.x + k4.z);
		v0.y = v0.y + (stepY/6.0)*(k1.y + 2.0*k2.y + 2.0*k3.y + k4.y);
		v0.z = v0.z + (stepZ/6.0)*(k1.z + 2.0*k2.z + 2.0*k3.z + k4.z);

		++numSteps;
		if (numSteps > maxSteps) {
			LDEBUG("Max number of steps taken (" << maxSteps <<")");
			break;
		}
	}
	// Save last position. Model has +Z as up
	trajectory.push_back(glm::vec3(pos.x, pos.z, pos.y));
	return trajectory;
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

glm::vec4 KameleonWrapper::classifyFieldline(FieldlineEnd fEnd, FieldlineEnd bEnd) {
	glm::vec4 color;
	if (		(fEnd == FieldlineEnd::NORTH || fEnd == FieldlineEnd::SOUTH)
			&& 	(bEnd == FieldlineEnd::NORTH || bEnd == FieldlineEnd::SOUTH)) {
		// closed
		color = glm::vec4(1.0, 0.0, 0.0, 1.0);
	} else if ((fEnd == FieldlineEnd::OUT && bEnd == FieldlineEnd::NORTH)
			|| (bEnd == FieldlineEnd::OUT && fEnd == FieldlineEnd::NORTH)) {
		// north
		color = glm::vec4(1.0, 1.0, 0.0, 1.0);
	} else if ((fEnd == FieldlineEnd::OUT && bEnd == FieldlineEnd::SOUTH)
			|| (bEnd == FieldlineEnd::OUT && fEnd == FieldlineEnd::SOUTH)) {
		// south
		color = glm::vec4(0.0, 1.0, 0.0, 1.0);
	} else if (fEnd == FieldlineEnd::OUT && bEnd == FieldlineEnd::OUT) {
		// solar wind
		color = glm::vec4(0.0, 0.0, 1.0, 1.0);
	}
	return color;
}

} // namespace openspace
