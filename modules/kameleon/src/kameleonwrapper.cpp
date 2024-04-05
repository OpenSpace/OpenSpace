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

#include <modules/kameleon/include/kameleonwrapper.h>

#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/stringhelper.h>
#include <filesystem>

#ifdef WIN32
#pragma warning (push)
#pragma warning (disable : 4619) // #pragma warning: there is no warning number '4675'
#endif // WIN32

#include <ccmc/Kameleon.h>
#include <ccmc/Constants.h>
#include <ccmc/FileReader.h>
#include <ccmc/Model.h>
#include <ccmc/Interpolator.h>
#include <ccmc/BATSRUS.h>
#include <ccmc/ENLIL.h>
#include <ccmc/CCMCTime.h>

#ifdef WIN32
#pragma warning (pop)
#endif // WIN32

namespace {
    constexpr std::string_view _loggerCat = "KameleonWrapper";
    constexpr float RE_TO_METER = 6371000;
} // namespace

namespace openspace {

std::array<std::string, 3> gridVariables(ccmc::Model* model) {
    // get the grid system string
    std::string grid = model->getGlobalAttribute("grid_system_1").getAttributeString();

    // remove leading and trailing brackets
    grid = grid.substr(1, grid.length() - 2);

    // remove all whitespaces
    grid.erase(remove_if(grid.begin(), grid.end(), isspace), grid.end());

    // tokenize
    std::vector<std::string> tokens = ghoul::tokenizeString(grid, ',');

    // validate
    if (tokens.size() != 3) {
        throw ghoul::RuntimeError(
            "Expected three dimensional grid system. Got " +
            std::to_string(tokens.size()) + "dimensions"
        );
    }

    std::string x = std::move(tokens.at(0));
    std::string y = std::move(tokens.at(1));
    std::string z = std::move(tokens.at(2));

    x = ghoul::toLowerCase(x);
    y = ghoul::toLowerCase(y);
    z = ghoul::toLowerCase(z);

    return { x, y, z };
}

KameleonWrapper::KameleonWrapper(const std::string& filename) {
    open(filename);
}

KameleonWrapper::~KameleonWrapper() {
    close();
}

bool KameleonWrapper::open(const std::string& filename) {
    close();

    if (!std::filesystem::is_regular_file(filename)) {
        return false;
    }

    _kameleon = new ccmc::Kameleon;
    long status = _kameleon->open(filename);
    if (status == ccmc::FileReader::OK) {
        _model = _kameleon->model;
        _interpolator = _model->createNewInterpolator();

        std::array<std::string, 3> v = gridVariables();
        _xCoordVar = v[0];
        _yCoordVar = v[1];
        _zCoordVar = v[2];
        _type = modelType();

        LDEBUG(std::format("x: {}", _xCoordVar));
        LDEBUG(std::format("y: {}", _yCoordVar));
        LDEBUG(std::format("z: {}", _zCoordVar));

        _min = glm::vec3(
            _model->getVariableAttribute(_xCoordVar, "actual_min").getAttributeFloat(),
            _model->getVariableAttribute(_yCoordVar, "actual_min").getAttributeFloat(),
            _model->getVariableAttribute(_zCoordVar, "actual_min").getAttributeFloat()
        );

        _max = glm::vec3(
            _model->getVariableAttribute(_xCoordVar, "actual_max").getAttributeFloat(),
            _model->getVariableAttribute(_yCoordVar, "actual_max").getAttributeFloat(),
            _model->getVariableAttribute(_zCoordVar, "actual_max").getAttributeFloat()
        );

        _validMin = glm::vec3(
            _model->getVariableAttribute(_xCoordVar, "valid_min").getAttributeFloat(),
            _model->getVariableAttribute(_yCoordVar, "valid_min").getAttributeFloat(),
            _model->getVariableAttribute(_zCoordVar, "valid_min").getAttributeFloat()
        );

        _validMax = glm::vec3(
            _model->getVariableAttribute(_xCoordVar, "valid_max").getAttributeFloat(),
            _model->getVariableAttribute(_yCoordVar, "valid_max").getAttributeFloat(),
            _model->getVariableAttribute(_zCoordVar, "valid_max").getAttributeFloat()
        );

        return true;
    }
    return false;
}

void KameleonWrapper::close() {
    if (_kameleon) {
        _kameleon->close();
    }

    delete _interpolator;
    _interpolator = nullptr;

    delete _kameleon;
    _kameleon = nullptr;

    _model = nullptr;
    _type = Model::Unknown;
    _gridType = GridType::Unknown;
}

// This method returns new'd memory,  turn into std::vector<float> instead?
float* KameleonWrapper::uniformSampledValues(const std::string& var,
                                             const glm::size3_t& outDimensions) const
{
    ghoul_assert(_model && _interpolator, "Model and interpolator must exist");

    LINFO(std::format(
        "Loading variable '{}' from CDF data with a uniform sampling", var
    ));

    const size_t size = outDimensions.x * outDimensions.y * outDimensions.z;
    float* data = new float[size];
    std::vector<double> doubleData(size);


    const double varMin =
        _model->getVariableAttribute(var, "actual_min").getAttributeFloat();
    LDEBUG(std::format("{} Min: {}", var, varMin));
    const double varMax =
        _model->getVariableAttribute(var, "actual_max").getAttributeFloat();
    LDEBUG(std::format("{} Max: {}", var, varMax));

    // HISTOGRAM
    constexpr int NBins = 200;
    std::vector<int> histogram(NBins, 0);
    // Explicitly mentioning the capture list provides either an error on MSVC (if NBins)
    // is not specified or a warning on Clang if it is specified. Sigh...
    auto mapToHistogram = [=](double val) {
        double zeroToOne = (val - varMin) / (varMax - varMin);
        zeroToOne *= static_cast<double>(NBins);
        const int izerotoone = static_cast<int>(zeroToOne);

        return glm::clamp(izerotoone, 0, NBins - 1);
    };

    // ProgressBar pb(static_cast<int>(outDimensions.x));
    for (size_t x = 0; x < outDimensions.x; ++x) {
        for (size_t y = 0; y < outDimensions.y; ++y) {
            for (size_t z = 0; z < outDimensions.z; ++z) {
                const size_t index = x + y * outDimensions.x +
                                     z * outDimensions.x * outDimensions.y;

                if (_gridType == GridType::Spherical) {
                    // Put r in the [0..sqrt(3)] range
                    const double rNorm = glm::root_three<double>() * x /
                                         outDimensions.x - 1;

                    // Put theta in the [0..PI] range
                    const double thetaNorm = glm::pi<double>() * y / outDimensions.y - 1;

                    // Put phi in the [0..2PI] range
                    const double phiNorm = glm::two_pi<double>() * z /
                                           outDimensions.z - 1;

                    // Go to physical coordinates before sampling
                    const double rPh = _min.x + rNorm * (_max.x - _min.x);
                    const double thetaPh = thetaNorm;
                    // phi range needs to be mapped to the slightly different model
                    // range to avoid gaps in the data Subtract a small term to
                    // avoid rounding errors when comparing to phiMax.
                    const double phiPh = _min.z + phiNorm /
                                    glm::two_pi<double>() * (_max.z - _min.z - 0.000001);

                    double value = 0.0;
                    // See if sample point is inside domain
                    if (rPh < _min.x || rPh > _max.x || thetaPh < _min.y ||
                        thetaPh > _max.y || phiPh < _min.z || phiPh > _max.z)
                    {
                        if (phiPh > _max.z) {
                            LWARNING("Warning: There might be a gap in the data");
                        }
                        // Leave values at zero if outside domain
                    }
                    else { // if inside
                        // ENLIL CDF specific hacks!
                        // Convert from meters to AU for interpolator
                        const double localRPh = rPh / ccmc::constants::AU_in_meters;
                        // Convert from colatitude [0, pi] rad to latitude [-90, 90] deg
                        const double localThetaPh = -thetaPh * 180.f /
                                                    glm::pi<double>() + 90.f;
                        // Convert from [0, 2pi] rad to [0, 360] degrees
                        const double localPhiPh = phiPh * 180.f / glm::pi<double>();
                        // Sample
                        value = _interpolator->interpolate(
                            var,
                            static_cast<float>(localRPh),
                            static_cast<float>(localThetaPh),
                            static_cast<float>(localPhiPh)
                        );
                    }

                    doubleData[index] = value;
                    histogram[mapToHistogram(value)]++;
                }
                else {
                    // Assume cartesian for fallback purpose
                    const double stepX = (_max.x - _min.x) /
                                         (static_cast<double>(outDimensions.x));
                    const double stepY = (_max.y - _min.y) /
                                         (static_cast<double>(outDimensions.y));
                    const double stepZ = (_max.z - _min.z) /
                                         (static_cast<double>(outDimensions.z));

                    const double xPos = _min.x + stepX * x;
                    const double yPos = _min.y + stepY * y;
                    const double zPos = _min.z + stepZ * z;

                    // get interpolated data value for (xPos, yPos, zPos)
                    // swap yPos and zPos because model has Z as up
                    double value = _interpolator->interpolate(
                        var,
                        static_cast<float>(xPos),
                        static_cast<float>(zPos),
                        static_cast<float>(yPos)
                    );
                    doubleData[index] = value;
                    histogram[mapToHistogram(value)]++;
                }
            }
        }
    }

    int sum = 0;
    int stop = 0;
    constexpr float TruncationLimit = 0.9f;
    const int upperLimit = static_cast<int>(size * TruncationLimit);
    for (int i = 0; i < NBins; i++) {
        sum += histogram[i];
        if (sum > upperLimit) {
            stop = i;
            break;
        }
    }

    const double dist = ((varMax - varMin) / NBins) * stop;

    const double varMaxNew = varMin + dist;
    for(size_t i = 0; i < size; i++) {
        const double normalizedVal = (doubleData[i] - varMin) / (varMaxNew - varMin);

        data[i] = static_cast<float>(glm::clamp(normalizedVal, 0.0, 1.0));
        if (data[i] < 0.f) {
            LERROR(std::format("Datapoint {} less than 0", i));
        }
        if (data[i] > 1.f) {
            LERROR(std::format("Datapoint {} more than 1", i));
        }
    }

    return data;
}

// This method returns new'd memory,  turn into std::vector<float> instead?
float* KameleonWrapper::uniformSliceValues(const std::string& var,
                                           const glm::size3_t& outDimensions,
                                           float slice) const
{
    ghoul_assert(_model && _interpolator, "Model and interpolator must exist");
    LINFO(std::format(
        "Loading variable '{}' from CDF data with a uniform sampling",
        var
    ));

    const size_t size = outDimensions.x * outDimensions.y * outDimensions.z;
    float* data = new float[size];
    std::vector<double> doubleData(size);

    _model->loadVariable(var);

    const double varMin =
        _model->getVariableAttribute(var, "actual_min").getAttributeFloat();
    const double varMax =
        _model->getVariableAttribute(var, "actual_max").getAttributeFloat();

    const double stepX = (_max.x - _min.x) / outDimensions.x;
    const double stepY = (_max.y - _min.y) / outDimensions.y;
    const double stepZ = (_max.z - _min.z) / outDimensions.z;

    const bool hasXSlice = (outDimensions.x <= 1);
    const bool hasYSlice = (outDimensions.y <= 1);
    const bool hasZSlice = (outDimensions.z <= 1);

    const double xDim = hasXSlice ? 1.0 : outDimensions.x - 1;
    const double yDim = hasYSlice ? 1.0 : outDimensions.y - 1;
    const double zDim = hasZSlice ? 1.0 : outDimensions.z - 1;

    LDEBUG(std::format("{} min: {}", var, varMin));
    LDEBUG(std::format("{} max: {}", var, varMax));

    //double maxValue = 0.0;
    //double minValue = std::numeric_limits<double>::max();

    float missingValue = _model->getMissingValue();

    for (size_t x = 0; x < outDimensions.x; ++x) {
        for (size_t y = 0; y < outDimensions.y; ++y) {
            for(size_t z = 0; z < outDimensions.z; ++z){

                const float xi = (hasXSlice) ? slice : x;
                const float yi = (hasYSlice) ? slice : y;
                const float zi = (hasZSlice) ? slice : z;

                double value = 0;
                const size_t index = x + y * outDimensions.x +
                                     z * outDimensions.x * outDimensions.y;
                if (_gridType == GridType::Spherical) {
                        // int z = zSlice;
                        // Put r in the [0..sqrt(3)] range
                        const double rNorm = glm::root_three<double>() * xi / xDim;

                        // Put theta in the [0..PI] range
                        const double thetaNorm = glm::pi<double>() * yi / yDim;

                        // Put phi in the [0..2PI] range
                        const double phiNorm = glm::two_pi<double>() * zi / zDim;

                        // Go to physical coordinates before sampling
                        const double rPh = _min.x + rNorm * (_max.x - _min.x);
                        const double thetaPh = thetaNorm;
                        // phi range needs to be mapped to the slightly different model
                        // range to avoid gaps in the data Subtract a small term to
                        // avoid rounding errors when comparing to phiMax.
                        const double phiPh = _min.z + phiNorm / glm::two_pi<double>() *
                                             (_max.z - _min.z - 0.000001);

                        // See if sample point is inside domain
                        if (rPh < _min.x || rPh > _max.x || thetaPh < _min.y ||
                            thetaPh > _max.y || phiPh < _min.z || phiPh > _max.z)
                        {
                            if (phiPh > _max.z) {
                                LWARNING("Warning: There might be a gap in the data");
                            }
                            // Leave values at zero if outside domain
                        }
                        else {
                            // if inside
                            // ENLIL CDF specific hacks!
                            // Convert from meters to AU for interpolator
                            const double localRPh = rPh / ccmc::constants::AU_in_meters;
                            // Convert from colatitude [0, pi] rad to [-90, 90] deg
                            const double localThetaPh = -thetaPh * 180.f /
                                                        glm::pi<double>() + 90.f;
                            // Convert from [0, 2pi] rad to [0, 360] degrees
                            const double localPhiPh = phiPh * 180.f / glm::pi<double>();
                            // Sample
                            value = _interpolator->interpolate(
                                var,
                                static_cast<float>(localRPh),
                                static_cast<float>(localPhiPh),
                                static_cast<float>(localThetaPh)
                            );
                        }

                }
                else {
                    const double xPos = _min.x + stepX * xi;
                    const double yPos = _min.y + stepY * yi;
                    const double zPos = _min.z + stepZ * zi;

                    // std::cout << zPos << ", " << zpos << std::endl;
                    // Should y and z be flipped?
                    value = _interpolator->interpolate(
                        var,
                        static_cast<float>(xPos),
                        static_cast<float>(zPos),
                        static_cast<float>(yPos));
                }

                if (value != missingValue) {
                    doubleData[index] = value;
                    data[index] = static_cast<float>(value);
                }
                else {
                    doubleData[index] = 0;
                }
            }
        }
    }

    return data;
}

float* KameleonWrapper::uniformSampledVectorValues(const std::string& xVar,
                                                   const std::string& yVar,
                                                   const std::string& zVar,
                                                  const glm::size3_t& outDimensions) const
{
    ghoul_assert(_model && _interpolator, "Model and interpolator must exist");

    LINFO(std::format(
        "Loading variables {} {} {} from CDF data with a uniform sampling",
        xVar,
        yVar,
        zVar
    ));

    constexpr int NumChannels = 4;
    const size_t size = NumChannels * outDimensions.x * outDimensions.y * outDimensions.z;
    float* data = new float[size];

    float varXMin = _model->getVariableAttribute(xVar, "actual_min").getAttributeFloat();
    float varXMax = _model->getVariableAttribute(xVar, "actual_max").getAttributeFloat();
    float varYMin = _model->getVariableAttribute(yVar, "actual_min").getAttributeFloat();
    float varYMax = _model->getVariableAttribute(yVar, "actual_max").getAttributeFloat();
    float varZMin = _model->getVariableAttribute(zVar, "actual_min").getAttributeFloat();
    float varZMax = _model->getVariableAttribute(zVar, "actual_max").getAttributeFloat();

    const float stepX = (_max.x - _min.x) / (static_cast<float>(outDimensions.x));
    const float stepY = (_max.y - _min.y) / (static_cast<float>(outDimensions.y));
    const float stepZ = (_max.z - _min.z) / (static_cast<float>(outDimensions.z));

    //LDEBUG(xVar << "Min: " << varXMin);
    //LDEBUG(xVar << "Max: " << varXMax);
    //LDEBUG(yVar << "Min: " << varYMin);
    //LDEBUG(yVar << "Max: " << varYMax);
    //LDEBUG(zVar << "Min: " << varZMin);
    //LDEBUG(zVar << "Max: " << varZMax);

    //ProgressBar pb(static_cast<int>(outDimensions.x));
    for (size_t x = 0; x < outDimensions.x; ++x) {
        //pb.print(x);
        for (size_t y = 0; y < outDimensions.y; ++y) {
            for (size_t z = 0; z < outDimensions.z; ++z) {
                const size_t index = x * NumChannels + y * NumChannels * outDimensions.x +
                                     z * NumChannels * outDimensions.x * outDimensions.y;

                if (_gridType == GridType::Cartesian) {
                    const float xPos = _min.x + stepX * x;
                    const float yPos = _min.y + stepY * y;
                    const float zPos = _min.z + stepZ * z;

                    // get interpolated data value for (xPos, yPos, zPos)
                    const float xVal = _interpolator->interpolate(xVar, xPos, yPos, zPos);
                    const float yVal = _interpolator->interpolate(yVar, xPos, yPos, zPos);
                    const float zVal = _interpolator->interpolate(zVar, xPos, yPos, zPos);

                    // scale to [0,1]
                    data[index]     = (xVal - varXMin) / (varXMax - varXMin); // R
                    data[index + 1] = (yVal - varYMin) / (varYMax - varYMin); // G
                    data[index + 2] = (zVal - varZMin) / (varZMax - varZMin); // B
                    // GL_RGB refuses to work. Workaround doing a GL_RGBA  hardcoded alpha
                    data[index + 3] = 1.f;
                }
                else {
                    LERROR(
                        "Only cartesian grid supported for "
                        "uniformSampledVectorValues (for now)"
                    );
                    return data;
                }
            }
        }
    }

    return data;
}

KameleonWrapper::Fieldlines KameleonWrapper::classifiedFieldLines(const std::string& xVar,
                                                                  const std::string& yVar,
                                                                  const std::string& zVar,
                                                 const std::vector<glm::vec3>& seedPoints,
                                                                     float stepSize) const
{
    ghoul_assert(_model && _interpolator, "Model and interpolator must exist");
    LINFO(std::format(
        "Creating {} fieldlines from variables {} {} {}",
        seedPoints.size(), xVar, yVar, zVar
    ));

    std::vector<std::vector<LinePoint> > fieldLines;

    if (_type == Model::BATSRUS) {
        fieldLines.reserve(seedPoints.size());
        for (const glm::vec3& seedPoint : seedPoints) {
            FieldlineEnd forwardEnd;
            std::vector<glm::vec3> fLine = traceCartesianFieldline(
                xVar,
                yVar,
                zVar,
                seedPoint,
                stepSize,
                TraceDirection::FORWARD,
                forwardEnd
            );
            FieldlineEnd backEnd;
            std::vector<glm::vec3> bLine = traceCartesianFieldline(
                xVar,
                yVar,
                zVar,
                seedPoint,
                stepSize,
                TraceDirection::BACK,
                backEnd
            );

            bLine.erase(bLine.begin());
            bLine.insert(bLine.begin(), fLine.rbegin(), fLine.rend());

            // classify
            glm::vec4 color = classifyFieldline(forwardEnd, backEnd);

            // write colors and convert positions to meter
            std::vector<LinePoint> line;
            for (glm::vec3& position : bLine) {
                line.push_back({ RE_TO_METER * std::move(position), color });
            }

            fieldLines.push_back(line);
        }
    }
    else {
        LERROR("Fieldlines are only supported for BATSRUS model");
    }

    return fieldLines;
}

KameleonWrapper::Fieldlines KameleonWrapper::fieldLines(const std::string& xVar,
                                                        const std::string& yVar,
                                                        const std::string& zVar,
                                                 const std::vector<glm::vec3>& seedPoints,
                                                                           float stepSize,
                                                             const glm::vec4& color) const
{
    ghoul_assert(_model && _interpolator, "Model and interpolator must exist");

    LINFO(std::format(
        "Creating {} fieldlines from variables {} {} {}",
        seedPoints.size(), xVar, yVar, zVar
    ));

    Fieldlines fieldLines;

    if (_type == Model::BATSRUS) {
        fieldLines.reserve(seedPoints.size());
        for (const glm::vec3& seedPoint : seedPoints) {
            FieldlineEnd forwardEnd;
            std::vector<glm::vec3> fLine = traceCartesianFieldline(
                xVar,
                yVar,
                zVar,
                seedPoint,
                stepSize,
                TraceDirection::FORWARD,
                forwardEnd
            );
            FieldlineEnd backEnd;
            std::vector<glm::vec3> bLine = traceCartesianFieldline(
                xVar,
                yVar,
                zVar,
                seedPoint,
                stepSize,
                TraceDirection::BACK,
                backEnd
            );

            bLine.erase(bLine.begin());
            bLine.insert(bLine.begin(), fLine.rbegin(), fLine.rend());

            // write colors and convert positions to meter
            std::vector<LinePoint> line;
            for (glm::vec3& position : bLine) {
                line.push_back({ RE_TO_METER * std::move(position), color });
            }

            fieldLines.push_back(line);
        }
    }
    else {
        LERROR("Fieldlines are only supported for BATSRUS model");
    }

    return fieldLines;
}

KameleonWrapper::Fieldlines KameleonWrapper::lorentzTrajectories(
                                                 const std::vector<glm::vec3>& seedPoints,
                                                               const glm::vec4& /*color*/,
                                                                         float step) const
{
    LINFO(std::format("Creating {} Lorentz force trajectories", seedPoints.size()));

    Fieldlines trajectories;

    for (const glm::vec3& seedPoint : seedPoints) {
        std::vector<glm::vec3> posTraj = traceLorentzTrajectory(seedPoint, step, 1.f);
        std::vector<glm::vec3> negTraj = traceLorentzTrajectory(seedPoint, step, -1.f);

        negTraj.insert(negTraj.begin(), posTraj.rbegin(), posTraj.rend());

        // write colors and convert positions to meter
        std::vector<LinePoint> trajectory;
        for (glm::vec3& position : negTraj) {
            if (trajectory.size() < posTraj.size()) {
                // set positive trajectory to pink
                trajectory.push_back({
                    RE_TO_METER * std::move(position),
                    glm::vec4(1.f, 0.f, 1.f, 1.f)
                });
            }
            else {
                // set negative trajectory to cyan
                trajectory.push_back({
                    RE_TO_METER * std::move(position),
                    glm::vec4(0.f, 1.f, 1.f, 1.f)
                });
            }
        }
        trajectories.push_back(trajectory);
    }

    return trajectories;
}

glm::vec3 KameleonWrapper::modelBarycenterOffset() const {
    // ENLIL is centered, no need for offset
    if (_type == Model::ENLIL) {
        return glm::vec3(0.f);
    }

    glm::vec3 offset = glm::vec3(
        _min.x + (std::abs(_min.x) + std::abs(_max.x)) / 2.f,
        _min.y + (std::abs(_min.y) + std::abs(_max.y)) / 2.f,
        _min.z + (std::abs(_min.z) + std::abs(_max.z)) / 2.f
    );
    return offset;
}

glm::vec4 KameleonWrapper::modelBarycenterOffsetScaled() const {
    const std::array<std::string, 3>& units = gridUnits();
    glm::vec4 offset = glm::vec4(modelBarycenterOffset(), 1.f);
    if (units[0] == "R" && units[1] == "R" && units[2] == "R") {
        offset.x *= 6.371f;
        offset.y *= 6.371f;
        offset.z *= 6.371f;
        offset.w = 6;
    }
    return offset;
}

glm::vec3 KameleonWrapper::modelScale() const {
    if (_type == Model::ENLIL) {
        return glm::vec3(1.f);
    }

    glm::vec3 scale = glm::vec3(
        _max.x - _min.x,
        _max.y - _min.y,
        _max.z - _min.z
    );
    return scale;
}

glm::vec4 KameleonWrapper::modelScaleScaled() const {
    const std::array<std::string, 3>& units = gridUnits();
    glm::vec4 scale = glm::vec4(modelScale(), 1.0);
    if (units[0] == "R" && units[1] == "R" && units[2] == "R") {
        // Earth radius
        scale.x *= 6.371f;
        scale.y *= 6.371f;
        scale.z *= 6.371f;
        scale.w = 6;
    }
    else if (units[0] == "m" && units[1] == "radian" && units[2] == "radian") {
        // For spherical coordinate systems the radius is in meter
        scale.w = -log10(1.f / _max.x);
    }

    return scale;
}

const glm::vec3& KameleonWrapper::gridMax() const {
    return _max;
}

const glm::vec3& KameleonWrapper::gridMin() const {
    return _min;
}

std::string KameleonWrapper::variableUnit(const std::string& variable) const {
    return _model->getVariableAttribute(variable, "units").getAttributeString();
}

std::array<std::string, 3> KameleonWrapper::gridUnits() const {
    return {
        variableUnit(_xCoordVar),
        variableUnit(_yCoordVar),
        variableUnit(_zCoordVar)
    };
}

KameleonWrapper::Model KameleonWrapper::model() const {
    return _type;
}

KameleonWrapper::GridType KameleonWrapper::gridType() const {
    return _gridType;
}

KameleonWrapper::TraceLine KameleonWrapper::traceCartesianFieldline(
                                                                  const std::string& xVar,
                                                                  const std::string& yVar,
                                                                  const std::string& zVar,
                                                               const glm::vec3& seedPoint,
                                                                           float stepSize,
                                                                 TraceDirection direction,
                                                                  FieldlineEnd& end) const
{
    constexpr int MaxSteps = 5000;

    _model->loadVariable(xVar);
    const long int xID = _model->getVariableID(xVar);

    _model->loadVariable(yVar);
    const long int yID = _model->getVariableID(yVar);

    _model->loadVariable(zVar);
    const long int zID = _model->getVariableID(zVar);

    glm::vec3 pos = seedPoint;
    int numSteps = 0;
    TraceLine line;

    // While we are inside the models boundaries and not inside earth
    while ((pos.x < _max.x && pos.x > _min.x && pos.y < _max.y && pos.y > _min.y &&
            pos.z < _max.z && pos.z > _min.z) &&
            !(pos.x*pos.x + pos.y*pos.y + pos.z*pos.z < 1.0))
    {

        // Save position. Model has +Z as up
        line.push_back(glm::vec3(pos.x, pos.z, pos.y));

        // Calculate new position with Runge-Kutta 4th order
        float stepX;
        float stepY;
        float stepZ;
        glm::vec3 k1 = glm::normalize(glm::vec3(
            _interpolator->interpolate(xID, pos.x, pos.y, pos.z, stepX, stepY, stepZ),
            _interpolator->interpolate(yID, pos.x, pos.y, pos.z),
            _interpolator->interpolate(zID, pos.x, pos.y, pos.z)
        ));
        k1 = (direction == TraceDirection::FORWARD) ? k1 : -1.f * k1;

        glm::vec3 step = glm::vec3(stepX, stepY, stepZ) * stepSize;

        glm::vec3 k1Pos = pos + step / 2.f * k1;
        glm::vec3 k2 = glm::normalize(glm::vec3(
            _interpolator->interpolate(xID, k1Pos.x, k1Pos.y, k1Pos.z),
            _interpolator->interpolate(yID, k1Pos.x, k1Pos.y, k1Pos.z),
            _interpolator->interpolate(zID, k1Pos.x, k1Pos.y, k1Pos.z)
        ));
        k2 = (direction == TraceDirection::FORWARD) ? k2 : -1.f * k2;

        glm::vec3 k2Pos = pos + step / 2.f * k2;
        glm::vec3 k3 = glm::normalize(glm::vec3(
            _interpolator->interpolate(xID, k2Pos.x, k2Pos.y, k2Pos.z),
            _interpolator->interpolate(yID, k2Pos.x, k2Pos.y, k2Pos.z),
            _interpolator->interpolate(zID, k2Pos.x, k2Pos.y, k2Pos.z)
        ));
        k3 = (direction == TraceDirection::FORWARD) ? k3 : -1.f * k3;

        glm::vec3 k3Pos = pos + step / 2.f * k3;
        glm::vec3 k4 = glm::normalize(glm::vec3(
            _interpolator->interpolate(xID, k3Pos.x, k3Pos.y, k3Pos.z),
            _interpolator->interpolate(yID, k3Pos.x, k3Pos.y, k3Pos.z),
            _interpolator->interpolate(zID, k3Pos.x, k3Pos.y, k3Pos.z)
        ));
        k4 = (direction == TraceDirection::FORWARD) ? k4 : -1.f * k4;

        pos = pos + (step / 6.f) * (k1 + 2.f * k2 + 2.f * k3 + k4);

        ++numSteps;
        if (numSteps > MaxSteps) {
            LDEBUG(std::format("Max number of steps taken ({})", MaxSteps));
            break;
        }
    }
    // Save last position. Model has +Z as up
    line.push_back(pos);

    if (pos.z > 0.f && (pos.x * pos.x + pos.y * pos.y + pos.z * pos.z < 1.f)) {
        end = FieldlineEnd::NORTH;
    }
    else if (pos.z < 0.f && (pos.x * pos.x + pos.y * pos.y + pos.z * pos.z < 1.f)) {
        end = FieldlineEnd::SOUTH;
    }
    else {
        end = FieldlineEnd::FAROUT;
    }

    return line;
}

KameleonWrapper::TraceLine KameleonWrapper::traceLorentzTrajectory(
                                                               const glm::vec3& seedPoint,
                                                                           float stepsize,
                                                                      float eCharge) const
{
    constexpr int MaxSteps = 5000;

    glm::vec3 step = glm::vec3(stepsize);

    const long int bxID = _model->getVariableID("bx");
    const long int byID = _model->getVariableID("by");
    const long int bzID = _model->getVariableID("bz");
    const long int jxID = _model->getVariableID("jx");
    const long int jyID = _model->getVariableID("jy");
    const long int jzID = _model->getVariableID("jz");

    TraceLine trajectory;
    glm::vec3 pos = seedPoint;
    glm::vec3 v0 = glm::normalize(glm::vec3(
        _interpolator->interpolate("ux", pos.x, pos.y, pos.z),
        _interpolator->interpolate("uy", pos.x, pos.y, pos.z),
        _interpolator->interpolate("uz", pos.x, pos.y, pos.z)
    ));

    int numSteps = 0;
    // While we are inside the models boundries and not inside earth
    while ((pos.x < _max.x && pos.x > _min.x && pos.y < _max.y && pos.y > _min.y &&
            pos.z < _max.z && pos.z > _min.z) &&
            !(pos.x*pos.x + pos.y*pos.y + pos.z*pos.z < 1.0))
    {
        // Save position. Model has +Z as up
        trajectory.push_back(pos);

        // Calculate new position with Lorentz force quation and Runge-Kutta 4th order
        glm::vec3 B = glm::vec3(
            _interpolator->interpolate(bxID, pos.x, pos.y, pos.z),
            _interpolator->interpolate(byID, pos.x, pos.y, pos.z),
            _interpolator->interpolate(bzID, pos.x, pos.y, pos.z)
        );

        glm::vec3 E = glm::vec3(
            _interpolator->interpolate(jxID, pos.x, pos.y, pos.z),
            _interpolator->interpolate(jyID, pos.x, pos.y, pos.z),
            _interpolator->interpolate(jzID, pos.x, pos.y, pos.z)
        );
        const glm::vec3 k1 = glm::normalize(eCharge * (E + glm::cross(v0, B)));
        const glm::vec3 k1Pos = pos + step / 2.f * v0 + step * step / 8.f * k1;

        B = glm::vec3(
            _interpolator->interpolate(bxID, k1Pos.x, k1Pos.y, k1Pos.z),
            _interpolator->interpolate(byID, k1Pos.x, k1Pos.y, k1Pos.z),
            _interpolator->interpolate(bzID, k1Pos.x, k1Pos.y, k1Pos.z)
        );
        E = glm::vec3(
            _interpolator->interpolate(jxID, k1Pos.x, k1Pos.y, k1Pos.z),
            _interpolator->interpolate(jyID, k1Pos.x, k1Pos.y, k1Pos.z),
            _interpolator->interpolate(jzID, k1Pos.x, k1Pos.y, k1Pos.z)
        );
        const glm::vec3 v1 = v0 + step / 2.f * k1;
        const glm::vec3 k2 = glm::normalize(eCharge * (E + glm::cross(v1, B)));

        B = glm::vec3(
            _interpolator->interpolate(bxID, k1Pos.x, k1Pos.y, k1Pos.z),
            _interpolator->interpolate(byID, k1Pos.x, k1Pos.y, k1Pos.z),
            _interpolator->interpolate(bzID, k1Pos.x, k1Pos.y, k1Pos.z)
        );
        E = glm::vec3(
            _interpolator->interpolate(jxID, k1Pos.x, k1Pos.y, k1Pos.z),
            _interpolator->interpolate(jyID, k1Pos.x, k1Pos.y, k1Pos.z),
            _interpolator->interpolate(jzID, k1Pos.x, k1Pos.y, k1Pos.z)
        );
        const glm::vec3 v2 = v0 + step / 2.f * k2;
        const glm::vec3 k3 = glm::normalize(eCharge * (E + glm::cross(v2, B)));
        const glm::vec3 k3Pos = pos + step * v0 + step * step / 2.f * k1;

        B = glm::vec3(
            _interpolator->interpolate(bxID, k3Pos.x, k3Pos.y, k3Pos.z),
            _interpolator->interpolate(byID, k3Pos.x, k3Pos.y, k3Pos.z),
            _interpolator->interpolate(bzID, k3Pos.x, k3Pos.y, k3Pos.z)
        );
        E = glm::vec3(
            _interpolator->interpolate(jxID, k3Pos.x, k3Pos.y, k3Pos.z),
            _interpolator->interpolate(jyID, k3Pos.x, k3Pos.y, k3Pos.z),
            _interpolator->interpolate(jzID, k3Pos.x, k3Pos.y, k3Pos.z)
        );
        const glm::vec3 v3 = v0 + step * k3;
        const glm::vec3 k4 = glm::normalize(eCharge * (E + glm::cross(v3, B)));

        pos = pos + step * v0 + step * step / 6.f * (k1 + k2 + k3);

        v0 = v0 + step / 6.f * (k1 + 2.f * k2 + 2.f * k3 + k4);

        ++numSteps;
        if (numSteps > MaxSteps) {
            LDEBUG(std::format("Max number of steps taken ({})", MaxSteps));
            break;
        }
    }
    // Save last position. Model has +Z as up
    trajectory.push_back(pos);
    return trajectory;
}

std::array<std::string, 3> KameleonWrapper::gridVariables() const {
    return openspace::gridVariables(_model);
}

KameleonWrapper::GridType KameleonWrapper::gridType(const std::string& x,
                                                    const std::string& y,
                                                    const std::string& z) const
{
    if (x == "x" && y == "y" && z == "z") {
        return GridType::Cartesian;
    }
    if (x == "r" && y == "theta" && z == "phi") {
        return GridType::Spherical;
    }

    return GridType::Unknown;
}

KameleonWrapper::Model KameleonWrapper::modelType() const {
    if (_kameleon->doesAttributeExist("model_name")) {
        const std::string& modelName =
            _kameleon->getGlobalAttribute("model_name").getAttributeString();
        if (modelName == "open_ggcm" || modelName == "ucla_ggcm") {
            return Model::OpenGGCM;
        }
        else if (modelName == "batsrus") {
            return Model::BATSRUS;
        }
        else if (modelName == "enlil") {
            return Model::ENLIL;
        }
        else if (modelName == "mas") {
            return Model::MAS;
        }
        else if (modelName == "ADAPT3D") {
            return Model::Adapt3D;
        }
        else if (modelName == "swmf") {
            return Model::SWMF;
        }
        else if (modelName == "LFM") {
            return Model::LFM;
        }
    }
    return Model::Unknown;
}

glm::vec4 KameleonWrapper::classifyFieldline(FieldlineEnd fEnd, FieldlineEnd bEnd) const {
    if ((fEnd == FieldlineEnd::NORTH || fEnd == FieldlineEnd::SOUTH) &&
        (bEnd == FieldlineEnd::NORTH || bEnd == FieldlineEnd::SOUTH))
    {
        // closed
        return glm::vec4(1.f, 0.f, 0.f, 1.f);
    }
    else if ((fEnd == FieldlineEnd::FAROUT && bEnd == FieldlineEnd::NORTH) ||
             (bEnd == FieldlineEnd::FAROUT && fEnd == FieldlineEnd::NORTH))
    {
        // north
        return glm::vec4(1.f, 1.f, 0.f, 1.f);
    }
    else if ((fEnd == FieldlineEnd::FAROUT && bEnd == FieldlineEnd::SOUTH) ||
             (bEnd == FieldlineEnd::FAROUT && fEnd == FieldlineEnd::SOUTH))
    {
        // south
        return glm::vec4(0.f, 1.f, 0.f, 1.f);
    }
    else if (fEnd == FieldlineEnd::FAROUT && bEnd == FieldlineEnd::FAROUT) {
        // solar wind
        return glm::vec4(0.f, 0.f, 1.f, 1.f);
    }

    return glm::vec4(0.f);
}

std::string KameleonWrapper::parent() const {
    switch (_type) {
        case KameleonWrapper::Model::BATSRUS:
        case KameleonWrapper::Model::OpenGGCM:
        case KameleonWrapper::Model::LFM:
            return "Earth";
        case KameleonWrapper::Model::ENLIL:
        case KameleonWrapper::Model::MAS:
        case KameleonWrapper::Model::Adapt3D:
        case KameleonWrapper::Model::SWMF:
            return "Sun";
        default:
            return "";
    }
}

std::string KameleonWrapper::frame() const {
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

std::vector<std::string> KameleonWrapper::variables() const {
    std::vector<std::string> variableNames;

    int numVariables = _model->getNumberOfVariables();

    for (int i = 0; i < numVariables; i++) {
        variableNames.push_back(_model->getVariableName(i));;
    }
    return variableNames;
}

std::vector<std::string> KameleonWrapper::loadedVariables() const {
    return _kameleon->getLoadedVariables();
}

} // namespace openspace
