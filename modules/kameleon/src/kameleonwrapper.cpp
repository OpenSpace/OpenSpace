/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/kameleon/include/kameleonwrapper.h>
//#include <openspace/util/progressbar.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>

#include <ccmc/Kameleon.h>
#include <ccmc/Model.h>
#include <ccmc/Interpolator.h>
#include <ccmc/BATSRUS.h>
#include <ccmc/ENLIL.h>
#include <ccmc/CCMCTime.h>

#define _USE_MATH_DEFINES
#include <math.h>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iomanip>

#include <glm/gtx/rotate_vector.hpp>

namespace openspace {

std::string _loggerCat = "KameleonWrapper";
const float RE_TO_METER = 6371000;

KameleonWrapper::KameleonWrapper()
    : _kameleon(nullptr)
    , _model(nullptr)
    , _type(Model::Unknown)
    , _interpolator(nullptr)
    , _xMin(0.f)
    , _xMax(0.f)
    , _yMin(0.f)
    , _yMax(0.f)
    , _zMin(0.f)
    , _zMax(0.f)
    , _xValidMin(0.f)
    , _xValidMax(0.f)
    , _yValidMin(0.f)
    , _yValidMax(0.f)
    , _zValidMin(0.f)
    , _zValidMax(0.f)
    , _xCoordVar("")
    , _yCoordVar("")
    , _zCoordVar("")
    , _gridType(GridType::Unknown)
{}

KameleonWrapper::KameleonWrapper(const std::string& filename)
    : _kameleon(nullptr)
    , _model(nullptr)
    , _type(Model::Unknown)
    , _interpolator(nullptr)
    , _xMin(0.f)
    , _xMax(0.f)
    , _yMin(0.f)
    , _yMax(0.f)
    , _zMin(0.f)
    , _zMax(0.f)
    , _xValidMin(0.f)
    , _xValidMax(0.f)
    , _yValidMin(0.f)
    , _yValidMax(0.f)
    , _zValidMin(0.f)
    , _zValidMax(0.f)
    , _xCoordVar("")
    , _yCoordVar("")
    , _zCoordVar("")
    , _gridType(GridType::Unknown)
{
    open(filename);
}

KameleonWrapper::~KameleonWrapper() {
    close();
}

bool KameleonWrapper::open(const std::string& filename) {
    close();

    if (!FileSys.fileExists(filename))
        return false;

    _kameleon = new ccmc::Kameleon;
    long status = _kameleon->open(filename);
    if (status == ccmc::FileReader::OK) {
        _model = _kameleon->model;
        _interpolator = _model->createNewInterpolator();

        getGridVariables(_xCoordVar, _yCoordVar, _zCoordVar);

        _gridType = getGridType(_xCoordVar, _yCoordVar, _zCoordVar);
        _type = getModelType();

        LDEBUG("x:" << _xCoordVar);
        LDEBUG("y:" << _yCoordVar);
        LDEBUG("z:" << _zCoordVar);

        _xMin = _model->getVariableAttribute(_xCoordVar, "actual_min").getAttributeFloat();
        _xMax = _model->getVariableAttribute(_xCoordVar, "actual_max").getAttributeFloat();
        _yMin = _model->getVariableAttribute(_yCoordVar, "actual_min").getAttributeFloat();
        _yMax = _model->getVariableAttribute(_yCoordVar, "actual_max").getAttributeFloat();
        _zMin = _model->getVariableAttribute(_zCoordVar, "actual_min").getAttributeFloat();
        _zMax = _model->getVariableAttribute(_zCoordVar, "actual_max").getAttributeFloat();

        /*    _xMin = -200.0f;
        _xMax = 200.0f;
        _yMin = -200.0f;
        _yMax = 200.0f;
        _zMin = -200.0f;
        _zMax = 200.0f;*/


        _xValidMin = _model->getVariableAttribute(_xCoordVar, "valid_min").getAttributeFloat();
        _xValidMax = _model->getVariableAttribute(_xCoordVar, "valid_max").getAttributeFloat();
        _yValidMin = _model->getVariableAttribute(_yCoordVar, "valid_min").getAttributeFloat();
        _yValidMax = _model->getVariableAttribute(_yCoordVar, "valid_max").getAttributeFloat();
        _zValidMin = _model->getVariableAttribute(_zCoordVar, "valid_min").getAttributeFloat();
        _zValidMax = _model->getVariableAttribute(_zCoordVar, "valid_max").getAttributeFloat();
        LDEBUG("_xMin: " << _xMin);
        LDEBUG("_xMax: " << _xMax);
        LDEBUG("_yMin: " << _yMin);
        LDEBUG("_yMax: " << _yMax);
        LDEBUG("_zMin: " << _zMin);
        LDEBUG("_zMax: " << _zMax);
        LDEBUG("_xValidMin: " << _xValidMin);
        LDEBUG("_xValidMax: " << _xValidMax);
        LDEBUG("_yValidMin: " << _yValidMin);
        LDEBUG("_yValidMax: " << _yValidMax);
        LDEBUG("_zValidMin: " << _zValidMin);
        LDEBUG("_zValidMax: " << _zValidMax);

        return true;
    }
    return false;
}

void KameleonWrapper::close() {
    if (_kameleon)
        _kameleon->close();
    if (_interpolator)
        delete _interpolator;
    if (_kameleon)
        delete _kameleon;

    _kameleon = nullptr;
    _interpolator = nullptr;
    _model = nullptr;
    _type = Model::Unknown;
    _gridType = GridType::Unknown;
}

float* KameleonWrapper::getUniformSampledValues(
    const std::string& var, 
    const glm::size3_t& outDimensions) 
{
    assert(_model && _interpolator);
    assert(outDimensions.x > 0 && outDimensions.y > 0 && outDimensions.z > 0);
    LINFO("Loading variable " << var << " from CDF data with a uniform sampling");

    unsigned int size = static_cast<unsigned int>(outDimensions.x*outDimensions.y*outDimensions.z);
    float* data = new float[size];
    double* doubleData = new double[size];

    double varMin = _model->getVariableAttribute(var, "actual_min").getAttributeFloat();
    double varMax = _model->getVariableAttribute(var, "actual_max").getAttributeFloat();
    
    double stepX = (_xMax-_xMin)/(static_cast<double>(outDimensions.x));
    double stepY = (_yMax-_yMin)/(static_cast<double>(outDimensions.y));
    double stepZ = (_zMax-_zMin)/(static_cast<double>(outDimensions.z));
    
    LDEBUG(var << "Min: " << varMin);
    LDEBUG(var << "Max: " << varMax);

    // HISTOGRAM
    const int bins = 200;
    const float truncLim = 0.9f;
    std::vector<int> histogram (bins,0);
    auto mapToHistogram = [varMin, varMax, bins](double val) {
        double zeroToOne = (val-varMin)/(varMax-varMin);
        zeroToOne *= static_cast<double>(bins);
        int izerotoone = static_cast<int>(zeroToOne);
        
        return glm::clamp(izerotoone, 0, bins-1);
    };
    
    //ProgressBar pb(static_cast<int>(outDimensions.x));
    for (int x = 0; x < outDimensions.x; ++x) {
        //pb.print(x);
        
        for (int y = 0; y < outDimensions.y; ++y) {
            for (int z = 0; z < outDimensions.z; ++z) {
                
                unsigned int index = static_cast<unsigned int>(x + y*outDimensions.x + z*outDimensions.x*outDimensions.y);
                
                if (_gridType == GridType::Spherical) {
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
                    
                    double value = 0.0;
                    // See if sample point is inside domain
                    if (rPh < _xMin || rPh > _xMax || thetaPh < _yMin ||
                        thetaPh > _yMax || phiPh < _zMin || phiPh > _zMax) {
                        if (phiPh > _zMax) {
                            LWARNING("Warning: There might be a gap in the data");
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
                        value = _interpolator->interpolate(
                            var, 
                            static_cast<float>(rPh), 
                            static_cast<float>(thetaPh), 
                            static_cast<float>(phiPh));
                        // value = _interpolator->interpolate(var, rPh, phiPh, thetaPh);
                    }
                    
                    doubleData[index] = value;
                    histogram[mapToHistogram(value)]++;
                    
                } else {
                    // Assume cartesian for fallback purpose
                    double xPos = _xMin + stepX*x;
                    double yPos = _yMin + stepY*y;
                    double zPos = _zMin + stepZ*z;
                    /*
                    if (xPos < _xValidMin || xPos > _xValidMax ||
                        yPos < _yValidMin || yPos > _yValidMax ||
                        zPos < _zValidMin || zPos > _zValidMax) {
                        doubleData[index] = varMin;
                        continue;
                    }
                    */
                    
                    // get interpolated data value for (xPos, yPos, zPos)
                    // swap yPos and zPos because model has Z as up
                    double value = _interpolator->interpolate(
                        var,
                        static_cast<float>(xPos),
                        static_cast<float>(zPos),
                        static_cast<float>(yPos));
                    doubleData[index] = value;
                    histogram[mapToHistogram(value)]++;
                }
            }
        }
    }
    //std::cout << std::endl;
    //LINFO("Done!");

     //for (int i = 0; i <  outDimensions.x *  outDimensions.y *  outDimensions.z; ++i)
     //{
     //    std::cout << std::setfill(' ') << std::setw(15) << doubleData[i] << ", ";
     //    if(i % 10 == 0)
     //        std::cout << std::endl;
     //}
     //    
    // for(int i = 0; i < bins-1; ++i) {
    //     // sum += histogram[i];
    //     // if(sum + histogram[i+1] > sumuntil) {
    //     //     stop = i;
    //     //     LDEBUG("====================");
    //     //     break;
    //     // }
    //     LDEBUG("histogram[" << i << "]: " << histogram[i]);
    // }

    int sum = 0;
    int stop = 0;
    const int sumuntil = static_cast<int>(static_cast<float>(size) * truncLim);
    for(int i = 0; i < bins; ++i) {
        sum += histogram[i];
        if(sum > sumuntil) {
            stop = i;
            // LDEBUG("====================");
            break;
        }
        LDEBUG("histogram[" << i << "]: " << histogram[i]);
    }

    double dist = varMax - varMin;
    dist = (dist / static_cast<double>(bins)) * static_cast<double>(stop);

    varMax = varMin + dist;
    //LDEBUG(var << "Min: " << varMin);
    //LDEBUG(var << "Max: " << varMax);
    for(size_t i = 0; i < size; ++i) {
        double normalizedVal = (doubleData[i]-varMin)/(varMax-varMin);

        data[i] = static_cast<float>(glm::clamp(normalizedVal, 0.0, 1.0));
        if(data[i] < 0.0) {
            LERROR("Datapoint " << i << " less than 0");
        }
        if(data[i] > 1.0) {
            LERROR("Datapoint " << i << " more than 1");
        }
    }

    // for(int i = 0; i < size; ++i) {
    //     double normalizedVal = (doubleData[i]-varMin)/(varMax-varMin);
    //     // data[i] = static_cast<float>(glm::clamp(normalizedVal, 0.0, 1.0));
    //     data[i] = static_cast<float>(normalizedVal);
    // }

    delete[] doubleData;
    return data;
}

float* KameleonWrapper::getUniformSliceValues(    
    const std::string& var, 
    const glm::size3_t& outDimensions,
    const float& slice)
{
    assert(_model && _interpolator);
    assert(outDimensions.x > 0 && outDimensions.y > 0 && outDimensions.z > 0);
    LINFO("Loading variable " << var << " from CDF data with a uniform sampling");

    unsigned int size = static_cast<unsigned int>(outDimensions.x*outDimensions.y*outDimensions.z);
    float* data = new float[size];
    double* doubleData = new double[size];

    _model->loadVariable(var);

    double varMin = _model->getVariableAttribute(var, "actual_min").getAttributeFloat();
    double varMax = _model->getVariableAttribute(var, "actual_max").getAttributeFloat();
    
    double stepX = (_xMax-_xMin)/(static_cast<double>(outDimensions.x));
    double stepY = (_yMax-_yMin)/(static_cast<double>(outDimensions.y));
    double stepZ = (_zMax-_zMin)/(static_cast<double>(outDimensions.z));

    bool xSlice = (outDimensions.x <= 1);
    bool ySlice = (outDimensions.y <= 1);
    bool zSlice = (outDimensions.z <= 1);

    double xDim = (!xSlice)? outDimensions.x-1 : 1.0;
    double yDim = (!ySlice)? outDimensions.y-1 : 1.0;
    double zDim = (!zSlice)? outDimensions.z-1 : 1.0;

    LDEBUG(var << "Min: " << varMin);
    LDEBUG(var << "Max: " << varMax);

    double maxValue = 0.0;
    double minValue = std::numeric_limits<double>::max();
    
    float missingValue = _model->getMissingValue();

    for (int x = 0; x < outDimensions.x; ++x) {
        for (int y = 0; y < outDimensions.y; ++y) {
            for(int z = 0; z < outDimensions.z; ++z){

                float xi = (!xSlice)? x : slice;
                float yi = (!ySlice)? y : slice;
                float zi = (!zSlice)? z : slice;

                double value = 0;
                unsigned int index = static_cast<unsigned int>(x + y*outDimensions.x + z*outDimensions.x*outDimensions.y);
                if(_gridType == GridType::Spherical) {
                        // int z = zSlice; 
                        // Put r in the [0..sqrt(3)] range
                        double rNorm = sqrt(3.0)*(double)xi/(double)(xDim);
                        
                        // Put theta in the [0..PI] range
                        double thetaNorm = M_PI*(double)yi/(double)(yDim);
                        
                        // Put phi in the [0..2PI] range
                        double phiNorm = 2.0*M_PI*(double)zi/(double)(zDim);
                        
                        // Go to physical coordinates before sampling
                        double rPh = _xMin + rNorm*(_xMax-_xMin);
                        double thetaPh = thetaNorm;
                        // phi range needs to be mapped to the slightly different model
                        // range to avoid gaps in the data Subtract a small term to
                        // avoid rounding errors when comparing to phiMax.
                        double phiPh = _zMin + phiNorm/(2.0*M_PI)*(_zMax-_zMin-0.000001);
                        
                        // See if sample point is inside domain
                        if (rPh < _xMin || rPh > _xMax || thetaPh < _yMin ||
                            thetaPh > _yMax || phiPh < _zMin || phiPh > _zMax) {
                            if (phiPh > _zMax) {
                                LWARNING("Warning: There might be a gap in the data");
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
                            value = _interpolator->interpolate(
                                var, 
                                static_cast<float>(rPh), 
                                static_cast<float>(phiPh), 
                                static_cast<float>(thetaPh));
                            // value = _interpolator->interpolate(var, rPh, phiPh, thetaPh);
                        }
         
                }else{
                    double xPos = _xMin + stepX*xi;
                    double yPos = _yMin + stepY*yi;
                    double zPos = _zMin + stepZ*zi;

                    // std::cout << zPos << ", " << zpos << std::endl;
                    // Should y and z be flipped?
                    value = _interpolator->interpolate(
                        var,
                        static_cast<float>(xPos),
                        static_cast<float>(zPos),
                        static_cast<float>(yPos));

                }

                if(value != missingValue){
                    doubleData[index] = value;
                    data[index] = value;
                    // if(value > maxValue){
                    //     maxValue = value;
                    // }
                    // if(value < minValue){
                    //     minValue = value;
                    // }
                }else{
                    // std::cout << "value missing" << std::endl;
                    doubleData[index] = 0;
                }
            }
        }
    }
    // for(size_t i = 0; i < size; ++i) {
        // double normalizedVal = (doubleData[i]-minValue)/(maxValue-minValue);
        // data[i] = glm::clamp(normalizedVal, 0.0, 1.0);
        // data[i] = 1;
        // std::cout << minValue << ", " << maxValue << ", " << doubleData[i] << ", " << normalizedVal << ", " << data[i] << std::endl;
    // }

    delete[] doubleData;
    return data;
}

float* KameleonWrapper::getUniformSampledVectorValues(
    const std::string& xVar,
    const std::string& yVar, 
    const std::string& zVar, 
    const glm::size3_t& outDimensions) 
{
    assert(_model && _interpolator);
    assert(outDimensions.x > 0 && outDimensions.y > 0 && outDimensions.z > 0);
    LINFO("Loading variables " << xVar << " " << yVar << " " << zVar << " from CDF data with a uniform sampling");

    int channels = 4;
    unsigned int size = static_cast<unsigned int>(channels*outDimensions.x*outDimensions.y*outDimensions.z);
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

    //LDEBUG(xVar << "Min: " << varXMin);
    //LDEBUG(xVar << "Max: " << varXMax);
    //LDEBUG(yVar << "Min: " << varYMin);
    //LDEBUG(yVar << "Max: " << varYMax);
    //LDEBUG(zVar << "Min: " << varZMin);
    //LDEBUG(zVar << "Max: " << varZMax);

    //ProgressBar pb(static_cast<int>(outDimensions.x));
    for (int x = 0; x < outDimensions.x; ++x) {
        //pb.print(x);

        for (int y = 0; y < outDimensions.y; ++y) {
            for (int z = 0; z < outDimensions.z; ++z) {

                unsigned int index = static_cast<unsigned int>(x*channels + y*channels*outDimensions.x + z*channels*outDimensions.x*outDimensions.y);

                if(_gridType == GridType::Cartesian) {
                    float xPos = _xMin + stepX*x;
                    float yPos = _yMin + stepY*y;
                    float zPos = _zMin + stepZ*z;

                    // get interpolated data value for (xPos, yPos, zPos)
                    float xValue = _interpolator->interpolate(xVar, xPos, yPos, zPos);
                    float yValue = _interpolator->interpolate(yVar, xPos, yPos, zPos);
                    float zValue = _interpolator->interpolate(zVar, xPos, yPos, zPos);

                    // scale to [0,1]
                    data[index]     = (xValue-varXMin)/(varXMax-varXMin); // R
                    data[index + 1] = (yValue-varYMin)/(varYMax-varYMin); // G
                    data[index + 2] = (zValue-varZMin)/(varZMax-varZMin); // B
                    data[index + 3] = 1.0; // GL_RGB refuses to work. Workaround by doing a GL_RGBA with hardcoded alpha
                } else {
                    LERROR("Only cartesian grid supported for getUniformSampledVectorValues (for now)");
                    return data;
                }
            }
        }
    }

    return data;
}

KameleonWrapper::Fieldlines KameleonWrapper::getClassifiedFieldLines(
    const std::string& xVar, 
    const std::string& yVar,
    const std::string& zVar,
    const std::vector<glm::vec3>& seedPoints,
    float stepSize ) 
{
    assert(_model && _interpolator);
    LINFO("Creating " << seedPoints.size() << " fieldlines from variables " << xVar << " " << yVar << " " << zVar);

    std::vector<glm::vec3> fLine, bLine;
    std::vector<std::vector<LinePoint> > fieldLines;
    glm::vec4 color;
    FieldlineEnd forwardEnd, backEnd;

    if (_type == Model::BATSRUS) {
        for (glm::vec3 seedPoint : seedPoints) {
            fLine = traceCartesianFieldline(xVar, yVar, zVar, seedPoint, stepSize, TraceDirection::FORWARD, forwardEnd);
            bLine = traceCartesianFieldline(xVar, yVar, zVar, seedPoint, stepSize, TraceDirection::BACK, backEnd);

            bLine.erase(bLine.begin());
            bLine.insert(bLine.begin(), fLine.rbegin(), fLine.rend());

            // classify
            color = classifyFieldline(forwardEnd, backEnd);

            // write colors and convert positions to meter
            std::vector<LinePoint> line;
            for (glm::vec3 position : bLine) {
                line.push_back(LinePoint(RE_TO_METER*position, color));
            }

            fieldLines.push_back(line);
        }
    } else {
        LERROR("Fieldlines are only supported for BATSRUS model");
    }

    return fieldLines;
}

KameleonWrapper::Fieldlines KameleonWrapper::getFieldLines(
    const std::string& xVar, 
    const std::string& yVar,
    const std::string& zVar, 
    const std::vector<glm::vec3>& seedPoints,
    float stepSize, 
    const glm::vec4& color ) 
{
    assert(_model && _interpolator);
    LINFO("Creating " << seedPoints.size() << " fieldlines from variables " << xVar << " " << yVar << " " << zVar);

    std::vector<glm::vec3> fLine, bLine;
    Fieldlines fieldLines;
    FieldlineEnd forwardEnd, backEnd;

    if (_type == Model::BATSRUS) {
        for (glm::vec3 seedPoint : seedPoints) {
            fLine = traceCartesianFieldline(xVar, yVar, zVar, seedPoint, stepSize, TraceDirection::FORWARD, forwardEnd);
            bLine = traceCartesianFieldline(xVar, yVar, zVar, seedPoint, stepSize, TraceDirection::BACK, backEnd);

            bLine.erase(bLine.begin());
            bLine.insert(bLine.begin(), fLine.rbegin(), fLine.rend());            

            // write colors and convert positions to meter
            std::vector<LinePoint> line;
            for (glm::vec3 position : bLine) {
                line.push_back(LinePoint(RE_TO_METER*position, color));
            }

            fieldLines.push_back(line);
        }
    } else {
        LERROR("Fieldlines are only supported for BATSRUS model");
    }

    return fieldLines;
}

KameleonWrapper::Fieldlines KameleonWrapper::getLorentzTrajectories(
    const std::vector<glm::vec3>& seedPoints, 
    const glm::vec4& color, 
    float stepsize) 
{
    LINFO("Creating " << seedPoints.size() << " Lorentz force trajectories");

    Fieldlines trajectories;
    std::vector<glm::vec3> plusTraj, minusTraj;

    for (auto seedPoint : seedPoints) {
        plusTraj = traceLorentzTrajectory(seedPoint, stepsize, 1.0);
        minusTraj = traceLorentzTrajectory(seedPoint, stepsize, -1.0);

        //minusTraj.erase(minusTraj.begin());
        size_t plusNum = plusTraj.size();
        minusTraj.insert(minusTraj.begin(), plusTraj.rbegin(), plusTraj.rend());

        // write colors and convert positions to meter
        std::vector<LinePoint> trajectory;
        for (glm::vec3 position : minusTraj) {            
            if (trajectory.size() < plusNum) // set positive trajectory to pink
                trajectory.push_back(LinePoint(RE_TO_METER*position, glm::vec4(1, 0, 1, 1)));
            else // set negative trajectory to cyan
                trajectory.push_back(LinePoint(RE_TO_METER*position, glm::vec4(0, 1, 1, 1)));
        }
        trajectories.push_back(trajectory);
    }

    return trajectories;
}

glm::vec3 KameleonWrapper::getModelBarycenterOffset() {
    // ENLIL is centered, no need for offset
    if (_type == Model::ENLIL)
        return glm::vec3(0,0,0);

    glm::vec3 offset;
    offset.x = _xMin+(std::abs(_xMin)+std::abs(_xMax))/2.0f;
    offset.y = _yMin+(std::abs(_yMin)+std::abs(_yMax))/2.0f;
    offset.z = _zMin+(std::abs(_zMin)+std::abs(_zMax))/2.0f;
    return offset;
}

glm::vec4 KameleonWrapper::getModelBarycenterOffsetScaled(){
    std::tuple < std::string, std::string, std::string > gridUnits = getGridUnits();
    glm::vec4 offset = glm::vec4(getModelBarycenterOffset(), 1.0);
    if(std::get<0>(gridUnits) == "R" && std::get<1>(gridUnits) == "R" && std::get<2>(gridUnits) == "R"){
        offset.x *= 6.371f;
        offset.y *= 6.371f;
        offset.z *= 6.371f;
        offset.w = 6;
    }
    // else if(std::get<0>(t) == "m" && std::get<1>(t) == "radian" && std::get<2>(t) == "radian"){}
    return offset;
}

glm::vec3 KameleonWrapper::getModelScale() {
    if (_type == Model::ENLIL)
        return glm::vec3(1.0f, 1.0f, 1.0f);

    glm::vec3 scale;
    scale.x = _xMax - _xMin;
    scale.y = _yMax - _yMin;
    scale.z = _zMax - _zMin;
    return scale;
}

glm::vec4 KameleonWrapper::getModelScaleScaled(){
    std::tuple < std::string, std::string, std::string > gridUnits = getGridUnits();
    glm::vec4 scale = glm::vec4(getModelScale(), 1.0);
    if (std::get<0>(gridUnits) == "R" && std::get<1>(gridUnits) == "R" && std::get<2>(gridUnits) == "R") {
        // Earth radius
        scale.x *= 6.371f;
        scale.y *= 6.371f;
        scale.z *= 6.371f;
        scale.w = 6;
    }
    else if (std::get<0>(gridUnits) == "m" && std::get<1>(gridUnits) == "radian" && std::get<2>(gridUnits) == "radian") {
        // For spherical coordinate systems the radius is in meter
        scale.w = -log10(1.0f/_xMax);
    }

    return scale;
}

glm::vec3 KameleonWrapper::getGridMax() {
    return glm::vec3(_xMax, _yMax, _zMax);
}

glm::vec3 KameleonWrapper::getGridMin() {
    return glm::vec3(_xMin, _yMin, _zMin);
}

std::string KameleonWrapper::getVariableUnit(const std::string& variable) {
    return _model->getVariableAttribute(variable, "units").getAttributeString();
}

std::tuple < std::string, std::string, std::string >
KameleonWrapper::getGridUnits() {
    return std::make_tuple(
        getVariableUnit(_xCoordVar),
        getVariableUnit(_yCoordVar),
        getVariableUnit(_zCoordVar)
    );
}

KameleonWrapper::Model  KameleonWrapper::model() {
    return _type;
}

KameleonWrapper::GridType  KameleonWrapper::gridType() {
    return _gridType;
}

KameleonWrapper::TraceLine KameleonWrapper::traceCartesianFieldline(
    const std::string& xVar,
    const std::string& yVar,
    const std::string& zVar, 
    const glm::vec3& seedPoint,
    float stepSize, 
    TraceDirection direction, 
    FieldlineEnd& end) 
{

    glm::vec3 color, pos, k1, k2, k3, k4;
    TraceLine line;
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
        k2.x = _interpolator->interpolate(xID, pos.x+(stepX/2.0f)*k1.x, pos.y+(stepY/2.0f)*k1.y, pos.z+(stepZ/2.0f)*k1.z);
        k2.y = _interpolator->interpolate(yID, pos.x+(stepX/2.0f)*k1.x, pos.y+(stepY/2.0f)*k1.y, pos.z+(stepZ/2.0f)*k1.z);
        k2.z = _interpolator->interpolate(zID, pos.x+(stepX/2.0f)*k1.x, pos.y+(stepY/2.0f)*k1.y, pos.z+(stepZ/2.0f)*k1.z);
        k2 = (float)direction*glm::normalize(k2);
        k3.x = _interpolator->interpolate(xID, pos.x+(stepX/2.0f)*k2.x, pos.y+(stepY/2.0f)*k2.y, pos.z+(stepZ/2.0f)*k2.z);
        k3.y = _interpolator->interpolate(yID, pos.x+(stepX/2.0f)*k2.x, pos.y+(stepY/2.0f)*k2.y, pos.z+(stepZ/2.0f)*k2.z);
        k3.z = _interpolator->interpolate(zID, pos.x+(stepX/2.0f)*k2.x, pos.y+(stepY/2.0f)*k2.y, pos.z+(stepZ/2.0f)*k2.z);
        k3 = (float)direction*glm::normalize(k3);
        k4.x = _interpolator->interpolate(xID, pos.x+stepX*k3.x, pos.y+stepY*k3.y, pos.z+stepZ*k3.z);
        k4.y = _interpolator->interpolate(yID, pos.x+stepX*k3.x, pos.y+stepY*k3.y, pos.z+stepZ*k3.z);
        k4.z = _interpolator->interpolate(zID, pos.x+stepX*k3.x, pos.y+stepY*k3.y, pos.z+stepZ*k3.z);
        k4 = (float)direction*glm::normalize(k4);
        pos.x = pos.x + (stepX/6.0f)*(k1.x + 2.0f*k2.x + 2.0f*k3.x + k4.x);
        pos.y = pos.y + (stepY/6.0f)*(k1.y + 2.0f*k2.y + 2.0f*k3.y + k4.y);
        pos.z = pos.z + (stepZ/6.0f)*(k1.z + 2.0f*k2.z + 2.0f*k3.z + k4.z);

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
        end = FieldlineEnd::FAROUT;

    return line;
}

KameleonWrapper::TraceLine KameleonWrapper::traceLorentzTrajectory(
    const glm::vec3& seedPoint,
    float stepsize, 
    float eCharge) 
{
    glm::vec3 B, E, v0, k1, k2, k3, k4, sPos, tmpV;
    float stepX = stepsize, stepY = stepsize, stepZ = stepsize;

    long int bxID = _model->getVariableID("bx");
    long int byID = _model->getVariableID("by");
    long int bzID = _model->getVariableID("bz");
    long int jxID = _model->getVariableID("jx");
    long int jyID = _model->getVariableID("jy");
    long int jzID = _model->getVariableID("jz");

    TraceLine trajectory;
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

        sPos = glm::vec3(    pos.x+(stepX/2.0)*v0.x+(stepX*stepX/8.0)*k1.x,
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

        sPos = glm::vec3(    pos.x+stepX*v0.x+(stepX*stepX/2.0)*k1.x,
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

        pos.x = pos.x + stepX*v0.x + (stepX*stepX/6.0f)*(k1.x + k2.x + k3.x);
        pos.y = pos.y + stepY*v0.y + (stepY*stepY/6.0f)*(k1.y + k2.y + k3.y);
        pos.z = pos.z + stepZ*v0.z + (stepZ*stepZ/6.0f)*(k1.z + k2.z + k3.z);

        v0.x = v0.x + (stepX/6.0f)*(k1.x + 2.0f*k2.x + 2.0f*k3.x + k4.z);
        v0.y = v0.y + (stepY/6.0f)*(k1.y + 2.0f*k2.y + 2.0f*k3.y + k4.y);
        v0.z = v0.z + (stepZ/6.0f)*(k1.z + 2.0f*k2.z + 2.0f*k3.z + k4.z);

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

    std::transform(x.begin(), x.end(), x.begin(), ::tolower);
    std::transform(y.begin(), y.end(), y.begin(), ::tolower);
    std::transform(z.begin(), z.end(), z.begin(), ::tolower);
}

KameleonWrapper::GridType KameleonWrapper::getGridType(
    const std::string& x, 
    const std::string& y, 
    const std::string& z) 
{
    if(x == "x" && y == "y" && z == "z")
        return GridType::Cartesian;
    if(x == "r" && y == "theta" && z == "phi")
        return GridType::Spherical;
    
    return GridType::Unknown;
}

KameleonWrapper::Model KameleonWrapper::getModelType() {
    if(_kameleon->doesAttributeExist("model_name")) {
        std::string modelName = (_kameleon->getGlobalAttribute("model_name")).getAttributeString();
        if (modelName == "open_ggcm" || modelName == "ucla_ggcm")
        {
            return Model::OpenGGCM;
        } else if (modelName == "batsrus")
        {
            return Model::BATSRUS;
        } else if (modelName == "enlil")
        {
            return Model::ENLIL;
        } else if (modelName == "mas")
        {
            return Model::MAS;
        } else if (modelName == "ADAPT3D")
        {
            return Model::Adapt3D;
        } else if (modelName == "swmf")
        {
            return Model::SWMF;
        } else if (modelName == "LFM")
        {
            return Model::LFM;
        }
    }
    return Model::Unknown;
}

glm::vec4 KameleonWrapper::classifyFieldline(FieldlineEnd fEnd, FieldlineEnd bEnd) {
    glm::vec4 color;
    if (        (fEnd == FieldlineEnd::NORTH || fEnd == FieldlineEnd::SOUTH)
            &&     (bEnd == FieldlineEnd::NORTH || bEnd == FieldlineEnd::SOUTH)) {
        // closed
        color = glm::vec4(1.0, 0.0, 0.0, 1.0);
    } else if ((fEnd == FieldlineEnd::FAROUT && bEnd == FieldlineEnd::NORTH)
            || (bEnd == FieldlineEnd::FAROUT && fEnd == FieldlineEnd::NORTH)) {
        // north
        color = glm::vec4(1.0, 1.0, 0.0, 1.0);
    } else if ((fEnd == FieldlineEnd::FAROUT && bEnd == FieldlineEnd::SOUTH)
            || (bEnd == FieldlineEnd::FAROUT && fEnd == FieldlineEnd::SOUTH)) {
        // south
        color = glm::vec4(0.0, 1.0, 0.0, 1.0);
    } else if (fEnd == FieldlineEnd::FAROUT && bEnd == FieldlineEnd::FAROUT) {
        // solar wind
        color = glm::vec4(0.0, 0.0, 1.0, 1.0);
    }
    return color;
}

std::string KameleonWrapper::getParent() {
    switch (_type) {
    case KameleonWrapper::Model::BATSRUS:
    case KameleonWrapper::Model::OpenGGCM:
    case KameleonWrapper::Model::LFM:
        return "Earth";
    case  KameleonWrapper::Model::ENLIL:
    case KameleonWrapper::Model::MAS:
    case KameleonWrapper::Model::Adapt3D:
    case KameleonWrapper::Model::SWMF:
        return "Sun";
    default:
        return "";
    }
}

std::string KameleonWrapper::getFrame() {
    switch (_type) {
    case KameleonWrapper::Model::BATSRUS:
    case KameleonWrapper::Model::OpenGGCM:
    case KameleonWrapper::Model::LFM:
        return "GSM";
    case  KameleonWrapper::Model::ENLIL:
    case KameleonWrapper::Model::MAS:
    case KameleonWrapper::Model::Adapt3D:
    case KameleonWrapper::Model::SWMF:
        return "HEEQ";
    default:
        return "";
    }
}

std::vector<std::string> KameleonWrapper::getVariables(){
    std::vector<std::string> variableNames;

    int numVariables = _model->getNumberOfVariables();

    for(int i=0; i<numVariables; i++){
        variableNames.push_back(_model->getVariableName(i));;
    }
    return variableNames;
}

std::vector<std::string> KameleonWrapper::getLoadedVariables(){
    return _kameleon->getLoadedVariables();
}

} // namespace openspace
