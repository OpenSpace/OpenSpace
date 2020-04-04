/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <openspace/scene/profilefile.h>

#include <openspace/scripting/lualibrary.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <string>
#include <iomanip>
#include <cctype>

namespace {
    constexpr const char* _loggerCat = "ProfileFile";
    constexpr const char* KeyIdentifier = "Identifier";
    constexpr const char* KeyParent = "Parent";

} // namespace

namespace openspace {

void ProfileFile::readFromFile(std::string filename) {
    clearAllFields();
    std::ifstream inFile;

    try {
        inFile.open(filename, std::_Ios_Openmode::_S_in);
    }
    catch (std::ifstream::failure& e) {
        throw ghoul::RuntimeError("Exception opening profile file for read: "
            + filename, "profileFile");
    }

    try {
        readLines([&inFile] (std::string& line) {
            if (getline(inFile, line))
                return true;
            else
                return false;
        });
    }
    catch (std::ifstream::failure& e) {
        throw ghoul::RuntimeError(errorString("Read error using getline"),
            "profileFile");
    }

    try {
        inFile.close();
    }
    catch (std::ifstream::failure& e) {
        throw ghoul::RuntimeError("Exception closing profile file after read: "
            + filename, "profileFile");
    }
}

void ProfileFile::readLines(std::function<bool(std::string&)> reader) {
    std::string line;
    bool insideSection = false;
    _lineNum = 1;

    while (reader(line)) {
        processIndividualLine(insideSection, line);
        _lineNum++;
    }
}

void ProfileFile::processIndividualLine(bool& insideSection, std::string line) {
    if (insideSection) {
        if (isBlank(line)) {
            insideSection = false;
        }
        else {
            if (parseCurrentSection != nullptr) {
                (this->*parseCurrentSection)(line);
            }
        }
    }
    else if (line.substr(0, 1) == "#") {
        if (determineSection(line)) {
            insideSection = true;
        }
    }
}

void ProfileFile::writeToFile(std::string filename) {
    std::ofstream outFile;
    try {
        outFile.open(filename, std::ofstream::out | std::ofstream::app);
    }
    catch (std::ofstream::failure& e) {
        LERROR("Exception opening profile file for write: " + filename);
    }

    try {
        write(outFile);
    }
    catch (std::ofstream::failure& e) {
        LERROR("Data write error to file: " + filename);
    }

    try {
        outFile.close();
    }
    catch (std::ofstream::failure& e) {
        LERROR("Exception closing profile file after write: " + filename);
    }
}

void ProfileFile::write(std::ostream& output) {
    output << header_Version << std::endl;
    output << _version << std::endl << std::endl;
    output << header_Module << std::endl;
    addAllElements(output, _modules);
    output << std::endl;
    output << header_Asset << std::endl;
    addAllElements(output, _assets);
    output << std::endl;
    output << header_Property << std::endl;
    addAllElements(output, _properties);
    output << std::endl;
    output << header_Keybinding << std::endl;
    addAllElements(output, _keybindings);
    output << std::endl;
    output << header_Time << std::endl;
    output << _time << std::endl << std::endl;
    output << header_Camera << std::endl;
    output << _camera << std::endl;
    output << header_MarkNodes << std::endl;
    addAllElements(output, _markNodes);
}

const std::string ProfileFile::getVersion() const {
    return _version;
}

void ProfileFile::addAllElements(std::ostream& file, std::vector<std::string>& list) {
    for (auto s : list) {
        file << s << std::endl;
    }
}

void ProfileFile::clearAllFields() {
    _numLinesVersion = 0;
    _numLinesTime = 0;
    _numLinesCamera = 0;

    _version.clear();
    _time.clear();
    _camera.clear();
    _modules.clear();
    _assets.clear();
    _properties.clear();
    _keybindings.clear();
    _markNodes.clear();
}

bool ProfileFile::isBlank(std::string line) {
    char* c = const_cast<char*>(line.c_str());
    int nonBlanks = 0;
    while (*c) {
        if (!isspace(*c)) {
            nonBlanks++;
        }
        c++;
    }
    return (nonBlanks == 0);
}

bool ProfileFile::determineSection(std::string line) {
    bool foundSection = true;

    if (line.compare(header_Version) == 0) {
        parseCurrentSection = &ProfileFile::parseVersion;
    }
    else if (line.compare(header_Module) == 0) {
        parseCurrentSection = &ProfileFile::parseModule;
    }
    else if (line.compare(header_Asset) == 0) {
        parseCurrentSection = &ProfileFile::parseAsset;
    }
    else if (line.compare(header_Property) == 0) {
        parseCurrentSection = &ProfileFile::parseProperty;
    }
    else if (line.compare(header_Keybinding) == 0) {
        parseCurrentSection = &ProfileFile::parseKeybinding;
    }
    else if (line.compare(header_Time) == 0) {
        parseCurrentSection = &ProfileFile::parseTime;
    }
    else if (line.compare(header_Camera) == 0) {
        parseCurrentSection = &ProfileFile::parseCamera;
    }
    else if (line.compare(header_MarkNodes) == 0) {
        parseCurrentSection = &ProfileFile::parseMarkNodes;
    }
    else {
        throw ghoul::RuntimeError(errorString("Invalid section header '" + line + "'"),
            "profileFile");
        foundSection = false;
    }
    return foundSection;
}

std::string ProfileFile::errorString(std::string message) {
    std::string e = "Error @ line " + std::to_string(_lineNum) + ": ";
    return e + message;
}

std::string ProfileFile::time() const {
    return _time;
}

std::string ProfileFile::camera() const {
    return _camera;
}

std::vector<std::string> ProfileFile::modules() const {
    return _modules;
}

std::vector<std::string> ProfileFile::assets() const {
    return _assets;
}

std::vector<std::string> ProfileFile::properties() const {
    return _properties;
}

std::vector<std::string> ProfileFile::keybindings() const {
    return _keybindings;
}

std::vector<std::string> ProfileFile::markNodes() const {
    return _markNodes;
}

void ProfileFile::parseVersion(std::string line) {
    std::vector<std::string> fields;

    if (++_numLinesVersion > _versionLinesExpected) {
        throw ghoul::RuntimeError(errorString("Too many lines in Version section"),
            "profileFile");
    }
    if (splitByTab(line, fields) > _versionFieldsExpected) {
        throw ghoul::RuntimeError(errorString("No tabs allowed in Version entry"),
            "profileFile");
    }
    else {
        _version = line;
    }
}

void ProfileFile::parseModule(std::string line) {
    std::vector<std::string> fields;

    if (splitByTab(line, fields) != _moduleFieldsExpected) {
        throw ghoul::RuntimeError(errorString(std::to_string(_moduleFieldsExpected) +
            " fields required in a Module entry"), "profileFile");
    }
    std::vector<std::string> standard = {
        "module name",
        "",
        ""
    };
    verifyRequiredFields("Module", fields, standard, _moduleFieldsExpected);
    _modules.push_back(line);
}

void ProfileFile::parseAsset(std::string line) {
    std::vector<std::string> fields;

    if (splitByTab(line, fields) != _assetFieldsExpected) {
        throw ghoul::RuntimeError(errorString(std::to_string(_assetFieldsExpected) +
            " fields required in an Asset entry"), "profileFile");
    }
    std::vector<std::string> standard = {
        "asset name",
        ""
    };
    verifyRequiredFields("Asset", fields, standard, _assetFieldsExpected);
    _assets.push_back(line);
}

void ProfileFile::parseProperty(std::string line) {
    std::vector<std::string> fields;

    if (splitByTab(line, fields) != _propertyFieldsExpected) {
        throw ghoul::RuntimeError(errorString(std::to_string(_propertyFieldsExpected) +
            " fields required in Property entry"), "profileFile");
    }
    std::vector<std::string> standard = {
        "set command",
        "name",
        "value"
    };
    verifyRequiredFields("Property", fields, standard, _propertyFieldsExpected);
    _properties.push_back(line);
}

void ProfileFile::parseKeybinding(std::string line) {
    std::vector<std::string> fields;

    if (splitByTab(line, fields) != _keybindingFieldsExpected) {
        throw ghoul::RuntimeError(errorString(std::to_string(_keybindingFieldsExpected)
            + " fields required in Keybinding entry"), "profileFile");
    }
    std::vector<std::string> standard = {
        "key",
        "documentation",
        "name",
        "GuiPath",
        "local(T/F)",
        "script to execute"
    };
    verifyRequiredFields("Keybinding", fields, standard, _keybindingFieldsExpected);
    _keybindings.push_back(line);
}

void ProfileFile::parseTime(std::string line) {
    std::vector<std::string> fields;

    if (++_numLinesTime > _timeLinesExpected) {
        throw ghoul::RuntimeError(errorString("Too many lines in time section"),
            "profileFile");
    }
    if (splitByTab(line, fields) != _timeFieldsExpected) {
        throw ghoul::RuntimeError(errorString(std::to_string(_timeFieldsExpected) +
            " fields required in Time entry"), "profileFile");
    }
    std::vector<std::string> standard = {
        "time set type",
        "time value to set"
    };
    verifyRequiredFields("Time", fields, standard, _timeFieldsExpected);
    _time = line;
}

void ProfileFile::parseCamera(std::string line) {
    std::vector<std::string> fields;

    if (++_numLinesCamera > _cameraLinesExpected) {
        throw ghoul::RuntimeError(errorString("Too many lines in camera section"),
            "profileFile");
    }
    size_t nFields = splitByTab(line, fields);
    if (nFields == _cameraNavigationFieldsExpected) {
        std::vector<std::string> standard = {
            "Type of camera set (setNavigationState)",
            "setNavigationState Anchor",
            "",
            "",
            "setNavigationState position vector",
            "",
            "",
            ""
        };
        verifyRequiredFields("Camera navigation", fields, standard,
            _cameraNavigationFieldsExpected);
    }
    else if (nFields == _cameraGeoFieldsExpected) {
        std::vector<std::string> standard = {
            "Type of camera set (goToGeo)",
            "",
            "Camera goToGeo Latitude",
            "Camera goToGeo Longitude",
            ""
        };
        verifyRequiredFields("Camera goToGeo", fields, standard,
            _cameraGeoFieldsExpected);
    }
    else {
        throw ghoul::RuntimeError(errorString(std::to_string(
            _cameraNavigationFieldsExpected) + " or " + std::to_string(
            _cameraGeoFieldsExpected) + " fields required in Camera entry"),
            "profileFile");
    }
    _camera = line;
}

void ProfileFile::parseMarkNodes(std::string line) {
    std::vector<std::string> fields;

    if (splitByTab(line, fields) != _markNodesFieldsExpected) {
        throw ghoul::RuntimeError(errorString(std::to_string(_markNodesFieldsExpected) +
            " field required in an Mark Nodes entry"), "profileFile");
    }
    std::vector<std::string> standard = {
        "Mark Interesting Node name"
    };
    verifyRequiredFields("Mark Interesting Nodes", fields, standard,
        _markNodesFieldsExpected);
    _markNodes.push_back(line);
}

void ProfileFile::verifyRequiredFields(std::string sectionName,
                                       std::vector<std::string> fields,
                                       std::vector<std::string> standard,
                                       unsigned int nFields)
{
    for (unsigned int i = 0; i < fields.size(); i++) {
        if (!standard[i].empty() && fields[i].empty()) {
            std::string errMsg = sectionName + " " + standard[i];
            errMsg += "(arg " + std::to_string(i) + "/";
            errMsg += std::to_string(nFields) + ") is required";
            throw ghoul::RuntimeError(errorString(errMsg), "profileFile");
        }
    }
}

size_t ProfileFile::splitByTab(std::string line, std::vector<std::string>& result) {
    std::istringstream iss(line);
    std::string tmp;
    result.clear();
    while (getline(iss, tmp, '\t')) {
        result.push_back(tmp);
    }
    //Insert additional empty fields only for the case of tab delimiters at end of
    // string without populated field(s)
    size_t nTabs = std::count(line.begin(), line.end(), '\t');
    for (size_t i = 0; i < (nTabs - result.size() + 1); ++i) {
        result.push_back("");
    }
    return result.size();
}

}  // namespace openspace
