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

#include <modules/kameleonvolume/kameleonvolumereader.h>

#include <modules/kameleon/include/kameleonwrapper.h>
#include <modules/volume/rawvolume.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <filesystem>

#ifdef WIN32
#pragma warning (push)
#pragma warning (disable : 4619) // #pragma warning: there is no warning number '4619'
#pragma warning (disable : 4675) // #pragma warning: there is no warning number '4675'
#pragma warning (disable : 4800) // #pragma warning: there is no warning number '4800'
#endif // WIN32

#include <ccmc/Kameleon.h>
#include <ccmc/Model.h>
#include <ccmc/FileReader.h>
#include <ccmc/BATSRUS.h>
#include <ccmc/ENLIL.h>
#include <ccmc/CCMCTime.h>
#include <ccmc/Attribute.h>
#include <ccmc/Interpolator.h>

#ifdef WIN32
#pragma warning (pop)
#endif // WIN32

namespace {
    constexpr std::string_view _loggerCat = "KameleonVolumeReader";

    template <typename T>
    T globalAttribute(ccmc::Model&, const std::string&) {
        static_assert(sizeof(T) == 0);
    }

    template <>
    std::string globalAttribute(ccmc::Model& model, const std::string& attribute) {
        return model.getGlobalAttribute(attribute).getAttributeString();
    }

    template <>
    float globalAttribute(ccmc::Model& model, const std::string& attribute) {
        return model.getGlobalAttribute(attribute).getAttributeFloat();
    }


} // namespace

namespace openspace::kameleonvolume {

KameleonVolumeReader::KameleonVolumeReader(std::filesystem::path path)
    : _path(std::move(path))
{
    if (!std::filesystem::is_regular_file(_path)) {
        throw ghoul::FileNotFoundError(_path);
    }

    const long status = _kameleon->open(_path.string());
    if (status != ccmc::FileReader::OK) {
        throw ghoul::RuntimeError(std::format(
            "Failed to open file '{}' with Kameleon", _path
        ));
    }

    // Possibly use a kameleon interpolator instead of a model interpolator?
    _interpolator = std::unique_ptr<ccmc::Interpolator>(
        _kameleon->model->createNewInterpolator()
    );
}

KameleonVolumeReader::~KameleonVolumeReader() {}

std::unique_ptr<volume::RawVolume<float>> KameleonVolumeReader::readFloatVolume(
                                                            const glm::uvec3 & dimensions,
                                                              const std::string& variable,
                                                        const glm::vec3& lowerDomainBound,
                                                  const glm::vec3& upperDomainBound) const
{
    float min, max;
    return readFloatVolume(
        dimensions,
        variable,
        lowerDomainBound,
        upperDomainBound,
        min,
        max
    );
}

std::unique_ptr<volume::RawVolume<float>> KameleonVolumeReader::readFloatVolume(
                                                            const glm::uvec3 & dimensions,
                                                              const std::string& variable,
                                                              const glm::vec3& lowerBound,
                                                              const glm::vec3& upperBound,
                                                                          float& minValue,
                                                                    float& maxValue) const
{
    minValue = std::numeric_limits<float>::max();
    maxValue = -std::numeric_limits<float>::max();

    auto volume = std::make_unique<volume::RawVolume<float>>(dimensions);

    const glm::vec3 dims = volume->dimensions();
    const glm::vec3 diff = upperBound - lowerBound;

    auto interpolate = [this](const std::string& var, const glm::vec3& coords) {
        return _interpolator->interpolate(var, coords[0], coords[1], coords[2]);
    };

    float* data = volume->data();
    for (size_t index = 0; index < volume->nCells(); index++) {
        const glm::vec3 coords = volume->indexToCoords(index);
        const glm::vec3 coordsZeroToOne = coords / dims;
        const glm::vec3 volumeCoords = lowerBound + diff * coordsZeroToOne;

        data[index] = interpolate(variable, volumeCoords);

        minValue = glm::min(minValue, data[index]);
        maxValue = glm::max(maxValue, data[index]);
    }

    return volume;
}

std::vector<std::string> KameleonVolumeReader::variableNames() const {
    std::vector<std::string> variableNames;
    const int nVariables = _kameleon->model->getNumberOfVariables();
    for (int i = 0; i < nVariables; i++) {
        variableNames.push_back(_kameleon->model->getVariableName(i));
    }
    return variableNames;
}

std::vector<std::string> KameleonVolumeReader::variableAttributeNames() const {
    return _kameleon->model->getVariableAttributeNames();
}

std::vector<std::string> KameleonVolumeReader::globalAttributeNames() const {
    std::vector<std::string> attributeNames;
    const int nAttributes = _kameleon->model->getNumberOfGlobalAttributes();
    for (int i = 0; i < nAttributes; i++) {
        attributeNames.push_back(_kameleon->model->getGlobalAttributeName(i));
    }
    return attributeNames;
}

std::array<std::string, 3> KameleonVolumeReader::gridVariableNames() const {
    return openspace::gridVariables(_kameleon->model);
}

void KameleonVolumeReader::addAttributeToDictionary(ghoul::Dictionary& dictionary,
                                                    const std::string& key,
                                                    ccmc::Attribute& attr)
{
    ccmc::Attribute::AttributeType type = attr.getAttributeType();
    switch (type) {
        case ccmc::Attribute::AttributeType::FLOAT:
            dictionary.setValue(key, static_cast<double>(attr.getAttributeFloat()));
            return;
        case ccmc::Attribute::AttributeType::INT:
            dictionary.setValue(key, attr.getAttributeInt());
            return;
        case ccmc::Attribute::AttributeType::STRING:
            dictionary.setValue(key, attr.getAttributeString());
            return;
    }
}

ghoul::Dictionary KameleonVolumeReader::readMetaData() const {
    ghoul::Dictionary globalAttributesDictionary;
    for (const std::string& attributeName : globalAttributeNames()) {
        ccmc::Attribute attribute = _kameleon->model->getGlobalAttribute(attributeName);
        addAttributeToDictionary(globalAttributesDictionary, attributeName, attribute);
    }

    ghoul::Dictionary variableDictionary;
    std::vector<std::string> varAttrNames = variableAttributeNames();
    for (const std::string& variableName : variableNames()) {
        ghoul::Dictionary variableAttributesDictionary;
        for (const std::string& attributeName : varAttrNames) {
            ccmc::Attribute attribute = _kameleon->model->getVariableAttribute(
                variableName,
                attributeName
            );
            addAttributeToDictionary(
                variableAttributesDictionary,
                attributeName,
                attribute
            );
        }
        variableDictionary.setValue(variableName, variableAttributesDictionary);
    }

    ghoul::Dictionary res;
    res.setValue("globalAttributes", std::move(globalAttributesDictionary));
    res.setValue("variableAttributes", std::move(variableDictionary));
    return res;
}

std::string KameleonVolumeReader::simulationStart() const {
    std::string startTime;
    if (_kameleon->model->doesAttributeExist("start_time")) {
        startTime = globalAttribute<std::string>(*_kameleon->model, "start_time");
    }
    else if (_kameleon->model->doesAttributeExist("tim_rundate_cal")) {
        startTime = globalAttribute<std::string>(*_kameleon->model, "tim_rundate_cal");
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
    }
    else if (_kameleon->model->doesAttributeExist("tim_obsdate_cal")) {
        startTime = globalAttribute<std::string>(*_kameleon->model, "tim_obsdate_cal");
    }
    else if (_kameleon->model->doesAttributeExist("tim_crstart_cal")) {
        startTime = globalAttribute<std::string>(*_kameleon->model, "tim_crstart_cal");
    }

    if (startTime.length() == 19) {
        startTime += ".000Z";
    }

    return startTime;
}

float KameleonVolumeReader::elapsedTime() const {
    if (_kameleon->model->doesAttributeExist("elapsed_time_in_seconds")) {
        return globalAttribute<float>(*_kameleon->model, "elapsed_time_in_seconds");
    }
    else if (_kameleon->model->doesAttributeExist("time_physical_time")) {
        return globalAttribute<float>(*_kameleon->model, "time_physical_time");
    }
    return 0.f;
}

std::string KameleonVolumeReader::simulationEnd() const {
    return globalAttribute<std::string>(*_kameleon->model, "end_time");
}

std::string KameleonVolumeReader::getVisUnit(const std::string& variable) const {
    return _kameleon->model->getNativeUnit(variable);
}

std::string KameleonVolumeReader::time() const {
    double start =
        ccmc::Time(simulationStart()).getEpoch();
    // Get elapsed time in seconds and convert to milliseconds.
    double elapsed = elapsedTime() * 1000;
    return ccmc::Time(start + elapsed).toString();
}

double KameleonVolumeReader::minValue(const std::string & variable) const {
    ccmc::Model& m = *_kameleon->model;
    return m.getVariableAttribute(variable, "actual_min").getAttributeFloat();
}

double KameleonVolumeReader::maxValue(const std::string & variable) const {
    ccmc::Model& m = *_kameleon->model;
    return m.getVariableAttribute(variable, "actual_max").getAttributeFloat();
}

} // namespace openspace::kameleonvolume
