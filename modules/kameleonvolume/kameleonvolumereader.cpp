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

#include <modules/kameleonvolume/kameleonvolumereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>

#ifdef WIN32
#pragma warning (push)
#pragma warning (disable : 4619) // #pragma warning: there is no warning number '4619'
#pragma warning (disable : 4675) // #pragma warning: there is no warning number '4675'
#pragma warning (disable : 4800) // #pragma warning: there is no warning number '4800'
#endif // WIN32

#include <ccmc/Model.h>
#include <ccmc/BATSRUS.h>
#include <ccmc/ENLIL.h>
#include <ccmc/CCMCTime.h>
#include <ccmc/Attribute.h>

#ifdef WIN32
#pragma warning (pop)
#endif // WIN32

namespace {
    const char* _loggerCat = "KameleonVolumeReader";
} // namespace

namespace openspace {
namespace kameleonvolume {

KameleonVolumeReader::KameleonVolumeReader(const std::string& path)
    : _path(path)
{
    if (!FileSys.fileExists(path)) {
        LERROR(_path << " does not exist");
        throw ghoul::FileNotFoundError(_path);
    }

    long status = _kameleon.open(_path);
    if (status != ccmc::FileReader::OK) {
        LERROR("Failed to open file " << _path << " with Kameleon");
        throw ghoul::RuntimeError("Failed to open file: " + _path + " with Kameleon");
        return;
    }

    _model = _kameleon.model;

    // Possibly use a kameleon interpolator instead of a model interpolator?
    _interpolator = std::unique_ptr<ccmc::Interpolator>(_model->createNewInterpolator());
}

std::unique_ptr<volume::RawVolume<float>> KameleonVolumeReader::readFloatVolume(
    const glm::uvec3 & dimensions,
    const std::string & variable,
    const glm::vec3 & lowerDomainBound,
    const glm::vec3 & upperDomainBound) const
{
    float min, max;
    return readFloatVolume(dimensions, variable, lowerDomainBound, upperDomainBound, min, max);
}

std::unique_ptr<volume::RawVolume<float>> KameleonVolumeReader::readFloatVolume(
    const glm::uvec3 & dimensions,
    const std::string & variable,
    const glm::vec3 & lowerBound,
    const glm::vec3 & upperBound,
    float& minValue,
    float& maxValue) const
{
    minValue = FLT_MAX;
    maxValue = FLT_MIN;

    auto volume = std::make_unique<volume::RawVolume<float>>(dimensions);
    const glm::vec3 dims = volume->dimensions();
    const glm::vec3 diff = upperBound - lowerBound;

    auto interpolate =
        [this](const std::string& variable, glm::vec3 volumeCoords) {
            return _interpolator->interpolate(
                variable,
                volumeCoords[0],
                volumeCoords[1],
                volumeCoords[2]);
        };

    auto sample = [this, &variable, &interpolate](glm::vec3 volumeCoords) {
        return interpolate(variable, volumeCoords);
    };

    float* data = volume->data();
    for (size_t index = 0; index < volume->nCells(); index++) {
        glm::vec3 coords = volume->indexToCoords(index);
        glm::vec3 coordsZeroToOne = coords / dims;
        glm::vec3 volumeCoords = lowerBound + diff * coordsZeroToOne;

        data[index] = sample(volumeCoords);

        if (data[index] < minValue) {
            minValue = data[index];
        }
        if (data[index] > maxValue) {
            maxValue = data[index];
        }
    }

    return volume;
}

std::vector<std::string> KameleonVolumeReader::gridVariableNames() const {
    // get the grid system string
    std::string gridSystem = _model->getGlobalAttribute("grid_system_1").getAttributeString();

    // remove leading and trailing brackets
    gridSystem = gridSystem.substr(1, gridSystem.length() - 2);

    // remove all whitespaces
    gridSystem.erase(remove_if(gridSystem.begin(), gridSystem.end(), isspace), gridSystem.end());

    // replace all comma signs with whitespaces
    std::replace(gridSystem.begin(), gridSystem.end(), ',', ' ');

    // tokenize
    std::istringstream iss(gridSystem);
    std::vector<std::string> tokens{ std::istream_iterator<std::string>{iss},std::istream_iterator<std::string>{} };

    // validate
    if (tokens.size() != 3) {
        throw ghoul::RuntimeError(
            "Expected three dimensional grid system. Got " +
            std::to_string(tokens.size()) +
            "dimensions");
    }

    std::string x = tokens.at(0);
    std::string y = tokens.at(1);
    std::string z = tokens.at(2);

    std::transform(
        x.begin(),
        x.end(),
        x.begin(),
        [](char v) { return static_cast<char>(tolower(v)); }
    );
    std::transform(
        y.begin(),
        y.end(), y.begin(),
        [](char v) { return static_cast<char>(tolower(v)); }
    );
    std::transform(
        z.begin(),
        z.end(), z.begin(),
        [](char v) { return static_cast<char>(tolower(v)); }
    );

    return std::vector<std::string>{x, y, z};
}

std::vector<std::string> KameleonVolumeReader::variableNames() const {
    std::vector<std::string> variableNames;
    const int nVariables = _model->getNumberOfVariables();
    for (int i = 0; i < nVariables; ++i) {
        variableNames.push_back(_model->getVariableName(i));
    }
    return variableNames;
}
    
std::vector<std::string> KameleonVolumeReader::variableAttributeNames() const {
    return _model->getVariableAttributeNames();
}

std::vector<std::string> KameleonVolumeReader::globalAttributeNames() const {
    std::vector<std::string> attributeNames;
    const int nAttributes = _model->getNumberOfGlobalAttributes();
    for (int i = 0; i < nAttributes; ++i) {
        attributeNames.push_back(_model->getGlobalAttributeName(i));
    }
    return attributeNames;
}

void KameleonVolumeReader::addAttributeToDictionary(ghoul::Dictionary& dictionary, const std::string& key, ccmc::Attribute& attr) {
    ccmc::Attribute::AttributeType type = attr.getAttributeType();
    switch (type) {
    case ccmc::Attribute::AttributeType::FLOAT:
        dictionary.setValue<float>(key, attr.getAttributeFloat());
        return;
    case ccmc::Attribute::AttributeType::INT:
        dictionary.setValue<int>(key, attr.getAttributeInt());
        return;
    case ccmc::Attribute::AttributeType::STRING:
        dictionary.setValue<std::string>(key, attr.getAttributeString());
        return;
    }
}

ghoul::Dictionary KameleonVolumeReader::readMetaData() const {
    ghoul::Dictionary globalAttributesDictionary;
    for (const std::string& attributeName : globalAttributeNames()) {
        ccmc::Attribute attribute = _model->getGlobalAttribute(attributeName);
        addAttributeToDictionary(globalAttributesDictionary, attributeName, attribute);
    }

    ghoul::Dictionary variableDictionary;
    std::vector<std::string> varAttrNames = variableAttributeNames();
    for (const std::string& variableName : variableNames()) {
        ghoul::Dictionary variableAttributesDictionary;
        for (const std::string& attributeName : varAttrNames) {
            ccmc::Attribute attribute = _model->getVariableAttribute(variableName, attributeName);
            addAttributeToDictionary(variableAttributesDictionary, attributeName, attribute);
        }
        variableDictionary.setValue(variableName, variableAttributesDictionary);
    }

    return {
        {"globalAttributes", std::move(globalAttributesDictionary) },
        {"variableAttributes", std::move(variableDictionary) }
    };
}

std::string KameleonVolumeReader::simulationStart() const {
    std::string startTime;
    if (_model->doesAttributeExist("start_time")){
        startTime =
            _model->getGlobalAttribute("start_time").getAttributeString();
    } else if (_model->doesAttributeExist("tim_rundate_cal")) {
        startTime =
            _model->getGlobalAttribute("tim_rundate_cal").getAttributeString();
        size_t numChars = startTime.length();
        if (numChars < 19) {
            // Fall through to add the required characters
            switch (numChars) {
                case 10 : // YYYY-MM-DD        => YYYY-MM-DDTHH
                    startTime += "T00";
                    [[fallthrough]];
                case 13 : // YYYY-MM-DDTHH     => YYYY-MM-DDTHH:
                    startTime += ":";
                    [[fallthrough]];
                case 14 : // YYYY-MM-DDTHH:    => YYYY-MM-DDTHH:MM
                    startTime += "00";
                    [[fallthrough]];
                case 16 : // YYYY-MM-DDTHH:MM  => YYYY-MM-DDTHH:MM:
                    startTime += ":";
                    [[fallthrough]];
                case 17 : // YYYY-MM-DDTHH:MM: => YYYY-MM-DDTHH:MM:SS
                    startTime += "00";
                    break;
                default :
                    break;
            }
        }
    } else if (_model->doesAttributeExist("tim_obsdate_cal")) {
        startTime =
            _model->getGlobalAttribute("tim_obsdate_cal").getAttributeString();
    } else if (_model->doesAttributeExist("tim_crstart_cal")) {
        startTime =
            _model->getGlobalAttribute("tim_crstart_cal").getAttributeString();
    }

    if (startTime.length() == 19) {
        startTime += ".000Z";
    }

    return startTime;
}

float KameleonVolumeReader::elapsedTime() const {
    if (_model->doesAttributeExist("elapsed_time_in_seconds")) {
        return _model->getGlobalAttribute("elapsed_time_in_seconds").getAttributeFloat();
    } else if (_model->doesAttributeExist("time_physical_time")) {
        return _model->getGlobalAttribute("time_physical_time").getAttributeFloat();
    }
    return 0;
}

std::string KameleonVolumeReader::simulationEnd() const {
    return _model->getGlobalAttribute("end_time").getAttributeString();
}

std::string KameleonVolumeReader::time() const {
    double start =
        ccmc::Time(simulationStart()).getEpoch();
    // Get elapsed time in seconds and convert to milliseconds.
    double elapsed = elapsedTime() * 1000;
    return ccmc::Time(start + elapsed).toString();
}

double KameleonVolumeReader::minValue(const std::string & variable) const {
    return _model->getVariableAttribute(variable, "actual_min").getAttributeFloat();
}

double KameleonVolumeReader::maxValue(const std::string & variable) const {
    return _model->getVariableAttribute(variable, "actual_max").getAttributeFloat();
}

} // namepace kameleonvolume
} // namespace openspace
