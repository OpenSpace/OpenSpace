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

ProfileFile::ProfileFile(std::string filename) {
    readIn(filename);
}

ProfileFile::ProfileFile(std::function<bool(std::string&)> reader) {
    readIn(reader);
}

void ProfileFile::readIn(std::string filename) {
    clearAllFields();
    std::ifstream inFile;

    try {
        inFile.open(filename, std::ifstream::in);
    }
    catch (std::ifstream::failure& e) {
        throw ghoul::RuntimeError("Exception opening profile file for read: "
            + filename + " (" + e.what() + ")", "profileFile");
    }

    try {
        readIn([&inFile] (std::string& line) {
            if (getline(inFile, line))
                return true;
            else
                return false;
        });
    }
    catch (std::ifstream::failure& e) {
        throw ghoul::RuntimeError(errorString("Read error using getline in: "
            + filename + " (" + e.what() + ")"), "profileFile");
    }

    try {
        inFile.close();
    }
    catch (std::ifstream::failure& e) {
        throw ghoul::RuntimeError("Exception closing profile file after read in: "
            + filename + " (" + e.what() + ")", "profileFile");
    }
}

void ProfileFile::readIn(std::function<bool(std::string&)> reader) {
    clearAllFields();
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

void ProfileFile::writeToFile(const std::string& filename) {
    if (filename.find("/") != std::string::npos) {
        LERROR("Profile filename must not contain path (/) elements");
        return;
    }
    else if (filename.find(":") != std::string::npos) {
        LERROR("Profile filename must not contain path (:) elements");
        return;
    }
    else if (filename.find(".") != std::string::npos) {
        LERROR("Only provide the filename to save without file extension");
        return;
    }
    const std::string absFilename = absPath("${ASSETS}/" + filename + ".profile");

    if (FileSys.fileExists(absFilename)) {
        LERROR(fmt::format(
            "Unable to save profile '{}'. File of same name already exists.",
            absFilename.c_str()
        ));
        return;
    }

    std::ofstream outFile;
    try {
        outFile.open(absFilename, std::ofstream::out);
    }
    catch (std::ofstream::failure& e) {
        LERROR("Exception opening profile file for write: "
            + absFilename + " (" + e.what() + ")");
    }

    try {
        outFile << writeToString();
    }
    catch (std::ofstream::failure& e) {
        LERROR("Data write error to file: "
            + absFilename + " (" + e.what() + ")");
    }

    try {
        outFile.close();
    }
    catch (std::ofstream::failure& e) {
        LERROR("Exception closing profile file after write: "
            + absFilename + " (" + e.what() + ")");
    }
}

std::string ProfileFile::writeToString() {
    std::string output;
    output = header_Version + '\n';
    output += _version + '\n' + '\n';
    output += header_Module + '\n';
    addAllElements(output, _modules);
    output += '\n';
    output += header_Asset + '\n';
    addAllElements(output, _assets);
    output += '\n';
    output += header_Property + '\n';
    addAllElements(output, _properties);
    output += '\n';
    output += header_Keybinding + '\n';
    addAllElements(output, _keybindings);
    output += '\n';
    output += header_Time + '\n';
    output += _time + '\n' + '\n';
    output += header_Camera + '\n';
    output += _camera + '\n' + '\n';
    output += header_MarkNodes + '\n';
    addAllElements(output, _markNodes);

    return output;
}

const std::string ProfileFile::getVersion() const {
    return _version;
}

void ProfileFile::setVersion(std::string v) {
    _version = std::move(v);
}

void ProfileFile::addAllElements(std::string& str, std::vector<std::string>& list) {
    for (const std::string& s : list) {
        str += s + '\n';
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

ProfileFile::Lines ProfileFile::modules() const {
    return _modules;
}

ProfileFile::Lines ProfileFile::assets() const {
    return _assets;
}

ProfileFile::Lines ProfileFile::properties() const {
    return _properties;
}

ProfileFile::Lines ProfileFile::keybindings() const {
    return _keybindings;
}

ProfileFile::Lines ProfileFile::markNodes() const {
    return _markNodes;
}

void ProfileFile::parseVersion(std::string line) {
    std::vector<std::string> fields;

    if (++_numLinesVersion > versionLinesExpected) {
        throw ghoul::RuntimeError(errorString("Too many lines in Version section"),
            "profileFile");
    }
    if (splitByTab(line, fields) > versionFieldsExpected) {
        throw ghoul::RuntimeError(errorString("No tabs allowed in Version entry"),
            "profileFile");
    }
    else {
        _version = line;
    }
}

void ProfileFile::parseModule(std::string line) {
    std::vector<std::string> fields;

    if (splitByTab(line, fields) != moduleFieldsExpected) {
        throw ghoul::RuntimeError(errorString(std::to_string(moduleFieldsExpected) +
            " fields required in a Module entry"), "profileFile");
    }
    std::vector<std::string> standard = {
        "module name",
        "",
        ""
    };
    verifyRequiredFields("Module", fields, standard, moduleFieldsExpected);
    _modules.push_back(line);
}

void ProfileFile::parseAsset(std::string line) {
    std::vector<std::string> fields;
    if (splitByTab(line, fields) != assetFieldsExpected) {
        throw ghoul::RuntimeError(errorString(std::to_string(assetFieldsExpected) +
            " fields required in an Asset entry"), "profileFile");
    }
    std::vector<std::string> standard = {
        "asset name",
        ""
    };
    verifyRequiredFields("Asset", fields, standard, assetFieldsExpected);
    _assets.push_back(line);
}

void ProfileFile::parseProperty(std::string line) {
    std::vector<std::string> fields;

    if (splitByTab(line, fields) != propertyFieldsExpected) {
        throw ghoul::RuntimeError(errorString(std::to_string(propertyFieldsExpected) +
            " fields required in Property entry"), "profileFile");
    }
    std::vector<std::string> standard = {
        "set command",
        "name",
        "value"
    };
    verifyRequiredFields("Property", fields, standard, propertyFieldsExpected);
    _properties.push_back(line);
}

void ProfileFile::parseKeybinding(std::string line) {
    std::vector<std::string> fields;

    if (splitByTab(line, fields) != keybindingFieldsExpected) {
        throw ghoul::RuntimeError(errorString(std::to_string(keybindingFieldsExpected)
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
    verifyRequiredFields("Keybinding", fields, standard, keybindingFieldsExpected);
    _keybindings.push_back(line);
}

void ProfileFile::parseTime(std::string line) {
    std::vector<std::string> fields;

    if (++_numLinesTime > timeLinesExpected) {
        throw ghoul::RuntimeError(errorString("Too many lines in time section"),
            "profileFile");
    }
    if (splitByTab(line, fields) != timeFieldsExpected) {
        throw ghoul::RuntimeError(errorString(std::to_string(timeFieldsExpected) +
            " fields required in Time entry"), "profileFile");
    }
    std::vector<std::string> standard = {
        "time set type",
        "time value to set"
    };
    verifyRequiredFields("Time", fields, standard, timeFieldsExpected);
    _time = line;
}

void ProfileFile::parseCamera(std::string line) {
    std::vector<std::string> fields;

    if (++_numLinesCamera > cameraLinesExpected) {
        throw ghoul::RuntimeError(errorString("Too many lines in camera section"),
            "profileFile");
    }
    size_t nFields = splitByTab(line, fields);
    if (nFields == cameraNavigationFieldsExpected) {
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
            cameraNavigationFieldsExpected);
    }
    else if (nFields == cameraGeoFieldsExpected) {
        std::vector<std::string> standard = {
            "Type of camera set (goToGeo)",
            "",
            "Camera goToGeo Latitude",
            "Camera goToGeo Longitude",
            ""
        };
        verifyRequiredFields("Camera goToGeo", fields, standard,
            cameraGeoFieldsExpected);
    }
    else {
        throw ghoul::RuntimeError(errorString(std::to_string(
            cameraNavigationFieldsExpected) + " or " + std::to_string(
            cameraGeoFieldsExpected) + " fields required in Camera entry"),
            "profileFile");
    }
    _camera = line;
}

void ProfileFile::parseMarkNodes(std::string line) {
    std::vector<std::string> fields;

    if (splitByTab(line, fields) != markNodesFieldsExpected) {
        throw ghoul::RuntimeError(errorString(std::to_string(markNodesFieldsExpected) +
            " field required in an Mark Nodes entry"), "profileFile");
    }
    std::vector<std::string> standard = {
        "Mark Interesting Node name"
    };
    verifyRequiredFields("Mark Interesting Nodes", fields, standard,
        markNodesFieldsExpected);
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

void ProfileFile::updateTime(std::string line) {
    _time = std::move(line);
}

void ProfileFile::updateCamera(std::string line) {
    _camera = std::move(line);
}

void ProfileFile::addModuleLine(std::string line) {
    _modules.push_back(std::move(line));
}

void ProfileFile::addAssetLine(std::string line) {
    _assets.push_back(std::move(line));
}

void ProfileFile::addPropertyLine(std::string line) {
    _properties.push_back(std::move(line));
}

void ProfileFile::addKeybindingLine(std::string line) {
    _keybindings.push_back(std::move(line));
}

void ProfileFile::addMarkNodesLine(std::string line) {
    _markNodes.push_back(std::move(line));
}

void ProfileFile::clearAssets() {
    _assets.clear();
}

}  // namespace openspace
