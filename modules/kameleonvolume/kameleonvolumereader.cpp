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

#include <ccmc/Model.h>
#include <ccmc/BATSRUS.h>
#include <ccmc/ENLIL.h>
#include <ccmc/CCMCTime.h>
#include <ccmc/Attribute.h>

namespace {
    const char* _loggerCat = "KameleonVolumeReader";
}

namespace openspace {

KameleonVolumeReader::KameleonVolumeReader(const std::string& path)
    : _path(path)
{
    if (!FileSys.fileExists(path)) {
        LERROR(_path << "does not exist");
        return;
    }

    long status = _kameleon.open(_path);
    if (status != ccmc::FileReader::OK) {
        LERROR("Failed to open file " << _path << " with kameleon");
        return;
    }

    _model = _kameleon.model;
    _interpolator = std::unique_ptr<ccmc::Interpolator>(_model->createNewInterpolator());
}

std::unique_ptr<RawVolume<float>> KameleonVolumeReader::readFloatVolume(
    const glm::uvec3 & dimensions,
    const std::string & variable,
    const glm::vec3 & lowerBound,
    const glm::vec3 & upperBound) const
{
    auto volume = std::make_unique<RawVolume<float>>(dimensions);
    const glm::vec3 dims = volume->dimensions();
    const glm::vec3 diff = upperBound - lowerBound;

    _model->loadVariable(variable);

    float* data = volume->data();
    for (size_t index = 0; index < volume->nCells(); index++) {
        glm::vec3 coords = volume->indexToCoords(index);
        glm::vec3 coordsZeroToOne = coords / dims;
        glm::vec3 volumeCoords = lowerBound + diff * coordsZeroToOne;

        data[index] = _interpolator->interpolate(
            variable,
            static_cast<float>(volumeCoords[0]),
            static_cast<float>(volumeCoords[1]),
            static_cast<float>(volumeCoords[2]));
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

    std::transform(x.begin(), x.end(), x.begin(), ::tolower);
    std::transform(y.begin(), y.end(), y.begin(), ::tolower);
    std::transform(z.begin(), z.end(), z.begin(), ::tolower);

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

float KameleonVolumeReader::minValue(const std::string & variable) const {
    return _model->getVariableAttribute(variable, "actual_min").getAttributeFloat();
}

float KameleonVolumeReader::maxValue(const std::string & variable) const {
    return _model->getVariableAttribute(variable, "actual_max").getAttributeFloat();
}




}