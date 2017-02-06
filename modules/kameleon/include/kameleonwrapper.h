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

#ifndef __OPENSPACE_MODULE_ISWA___KAMELEONWRAPPER___H__
#define __OPENSPACE_MODULE_ISWA___KAMELEONWRAPPER___H__

#include <glm/gtx/std_based_type.hpp>

#include <tuple>
#include <string>
#include <vector>

namespace ccmc {
    class Kameleon;
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
        OpenGGCM,
        BATSRUS,        // Magnetosphere
        ENLIL,            // Heliosphere
        MAS,
        Adapt3D,
        SWMF,
        LFM,
        Unknown
    };

    enum class TraceDirection {
        FORWARD = 1,
        BACK     = -1
    };

    enum class FieldlineEnd {
        NORTH,
        SOUTH,
        FAROUT
    };
    
    enum class GridType {
        Cartesian,
        Spherical,
        Unknown
    };

    typedef std::vector<std::vector<LinePoint> > Fieldlines;

    KameleonWrapper();
    KameleonWrapper(const std::string& filename);
    ~KameleonWrapper();

    bool open(const std::string& filename);
    void close();

    float* getUniformSampledValues(
        const std::string& var, 
        const glm::size3_t& outDimensions);

    float* getUniformSliceValues(    
    const std::string& var, 
    const glm::size3_t& outDimensions,
    const float& zSlice);

    float* getUniformSampledVectorValues(
        const std::string& xVar, 
        const std::string& yVar,
        const std::string& zVar, 
        const glm::size3_t& outDimensions);

    Fieldlines getClassifiedFieldLines(
        const std::string& xVar,
        const std::string& yVar, 
        const std::string& zVar,
        const std::vector<glm::vec3>& seedPoints, 
        float stepSize);

    Fieldlines getFieldLines(
        const std::string& xVar,
        const std::string& yVar, 
        const std::string& zVar,
        const std::vector<glm::vec3>& seedPoints, 
        float stepSize, 
        const glm::vec4& color);

    Fieldlines getLorentzTrajectories(
        const std::vector<glm::vec3>& seedPoints,
        const glm::vec4& color, 
        float stepsize);

    glm::vec3 getModelBarycenterOffset();
    glm::vec4 getModelBarycenterOffsetScaled();
    glm::vec3 getModelScale();
    glm::vec4 getModelScaleScaled();
    glm::vec3 getGridMax();
    glm::vec3 getGridMin();
    std::string getVariableUnit(const std::string& variable);

    std::tuple < std::string, std::string, std::string > getGridUnits();

    Model model();
    GridType gridType();
    std::string getParent();
    std::string getFrame();
    std::vector<std::string> getVariables();
    std::vector<std::string> getLoadedVariables();

private:
    typedef std::vector<glm::vec3> TraceLine;
    TraceLine traceCartesianFieldline(
        const std::string& xVar,
        const std::string& yVar,
        const std::string& zVar, 
        const glm::vec3& seedPoint,
        float stepSize, 
        TraceDirection direction, 
        FieldlineEnd& end);

    TraceLine traceLorentzTrajectory(
        const glm::vec3& seedPoint,
        float stepsize, 
        float eCharge);
    
    void getGridVariables(std::string& x, std::string& y, std::string& z);
    GridType getGridType(
        const std::string& x, 
        const std::string& y, 
        const std::string& z);
    Model getModelType();
    glm::vec4 classifyFieldline(FieldlineEnd fEnd, FieldlineEnd bEnd);

    ccmc::Kameleon* _kameleon;
    ccmc::Model* _model;
    Model _type;
    ccmc::Interpolator* _interpolator;

    // Model parameters
    float _xMin, _xMax, _yMin, _yMax, _zMin, _zMax;
    float _xValidMin, _xValidMax, _yValidMin, _yValidMax, _zValidMin, _zValidMax;
    std::string _xCoordVar, _yCoordVar, _zCoordVar;
    GridType _gridType;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_ISWA___KAMELEONWRAPPER___H__
