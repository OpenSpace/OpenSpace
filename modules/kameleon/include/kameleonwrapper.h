/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_MODULE_KAMELEON___KAMELEONWRAPPER___H__
#define __OPENSPACE_MODULE_KAMELEON___KAMELEONWRAPPER___H__

#include <ghoul/glm.h>
#include <glm/gtx/std_based_type.hpp>
#include <array>
#include <filesystem>
#include <string>
#include <vector>

namespace ccmc {
    class Kameleon;
    class Model;
    class Interpolator;
} // namespace ccmc

namespace openspace {

struct LinePoint {
    glm::vec3 position = glm::vec3(0.f);
    glm::vec4 color = glm::vec4(0.f);
};

std::array<std::string, 3> gridVariables(ccmc::Model* model);

class KameleonWrapper {
public:
    enum class Model {
        OpenGGCM,
        BATSRUS, // Magnetosphere
        ENLIL, // Heliosphere
        MAS,
        Adapt3D,
        SWMF,
        LFM,
        Unknown
    };

    enum class TraceDirection {
        FORWARD = 1,
        BACK    = -1
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

    using Fieldlines = std::vector<std::vector<LinePoint>>;

    explicit KameleonWrapper(const std::filesystem::path& filename);
    ~KameleonWrapper();

    bool open(const std::filesystem::path& filename);
    void close();

    float* uniformSampledValues(const std::string& var,
        const glm::size3_t& outDimensions) const;

    float* uniformSliceValues(const std::string& var, const glm::size3_t& outDimensions,
        float zSlice) const;

    float* uniformSampledVectorValues(const std::string& xVar, const std::string& yVar,
        const std::string& zVar, const glm::size3_t& outDimensions) const;

    Fieldlines classifiedFieldLines(const std::string& xVar, const std::string& yVar,
        const std::string& zVar, const std::vector<glm::vec3>& seedPoints,
        float stepSize) const;

    Fieldlines fieldLines(const std::string& xVar, const std::string& yVar,
        const std::string& zVar, const std::vector<glm::vec3>& seedPoints, float stepSize,
        const glm::vec4& color) const;

    Fieldlines lorentzTrajectories(const std::vector<glm::vec3>& seedPoints,
        const glm::vec4& color, float stepsize) const;

    glm::vec3 modelBarycenterOffset() const;
    glm::vec4 modelBarycenterOffsetScaled() const;
    glm::vec3 modelScale() const;
    glm::vec4 modelScaleScaled() const;
    const glm::vec3& gridMax() const;
    const glm::vec3& gridMin() const;
    std::string variableUnit(const std::string& variable) const;

    std::array<std::string, 3> gridUnits() const;

    std::array<std::string, 3> gridVariables() const;

    Model model() const;
    GridType gridType() const;
    std::string parent() const;
    std::string frame() const;
    std::vector<std::string> variables() const;
    std::vector<std::string> loadedVariables() const;

private:
    using TraceLine = std::vector<glm::vec3>;

    TraceLine traceCartesianFieldline(const std::string& xVar, const std::string& yVar,
        const std::string& zVar, const glm::vec3& seedPoint, float stepSize,
        TraceDirection direction, FieldlineEnd& end) const;

    TraceLine traceLorentzTrajectory(const glm::vec3& seedPoint, float stepsize,
        float eCharge) const;

    GridType gridType(const std::string& x, const std::string& y,
        const std::string& z) const;
    Model modelType() const;
    glm::vec4 classifyFieldline(FieldlineEnd fEnd, FieldlineEnd bEnd) const;

    ccmc::Kameleon* _kameleon = nullptr;
    ccmc::Model* _model = nullptr;
    Model _type = Model::Unknown;
    ccmc::Interpolator* _interpolator = nullptr;

    // Model parameters
    glm::vec3 _min = glm::vec3(0.f);
    glm::vec3 _max = glm::vec3(0.f);
    glm::vec3 _validMin = glm::vec3(0.f);
    glm::vec3 _validMax = glm::vec3(0.f);
    std::string _xCoordVar;
    std::string _yCoordVar;
    std::string _zCoordVar;
    GridType _gridType = GridType::Unknown;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_KAMELEON___KAMELEONWRAPPER___H__
