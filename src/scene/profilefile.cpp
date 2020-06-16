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
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/misc.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/fmt.h>
#include <string>
#include <iomanip>
#include <cctype>

namespace {
    constexpr const char* _loggerCat = "ProfileFile";
    constexpr const char* KeyIdentifier = "Identifier";
    constexpr const char* KeyParent = "Parent";

    constexpr const char* headerVersion = "#Version";
    constexpr const char* headerModule = "#Module";
    constexpr const char* headerAsset = "#Asset";
    constexpr const char* headerProperty = "#Property";
    constexpr const char* headerKeybinding = "#Keybinding";
    constexpr const char* headerTime = "#Time";
    constexpr const char* headerCamera = "#Camera";
    constexpr const char* headerMarkNodes = "#MarkNodes";

    struct ProfileError : public ghoul::RuntimeError {
        explicit ProfileError(unsigned int lineNum, std::string msg)
            : ghoul::RuntimeError(
                fmt::format("Error @ line {}: {}", lineNum, std::move(msg)),
                "profileFile"
            )
        {}
    };
} // namespace

namespace openspace {

ProfileFile::ProfileFile(std::string filename) {
    readIn(filename);
}

void ProfileFile::readIn(std::string filename) {
    clearAllFields();
    _lineNum = 1;
    std::ifstream inFile;

    try {
        inFile.open(filename, std::ifstream::in);
    }
    catch (std::ifstream::failure& e) {
        throw ghoul::RuntimeError(fmt::format(
            "Exception opening profile file for read: {} ({})", filename, e.what()),
            "profileFile"
        );
    }

    try {
        std::string line;
        bool insideSection = false;
        while (std::getline(inFile, line)) {
            processIndividualLine(insideSection, line);
            _lineNum++;
        }
    }
    catch (std::ifstream::failure& e) {
        throw ProfileError(
            _lineNum,
            fmt::format("Read error using getline in: {} ({})", filename, e.what()
        ));
    }
}

void ProfileFile::processIndividualLine(bool& insideSection, std::string line) {
    if (insideSection) {
        if (std::all_of(line.begin(), line.end(), ::isspace)) {
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
    // @TODO (abock, 2020-06-15) Replace with non-throwing stream
    try {
        outFile.open(absFilename, std::ofstream::out);
    }
    catch (std::ofstream::failure& e) {
        LERROR(fmt::format(
            "Exception opening profile file for write: {} ({})", absFilename, e.what()
        ));
    }

    try {
        outFile << writeToString();
    }
    catch (std::ofstream::failure& e) {
        LERROR("Data write error to file: "
            + absFilename + " (" + e.what() + ")");
    }

    outFile.close();
}

std::string ProfileFile::writeToString() {
    std::string output;
    output = headerVersion + '\n';
    output += _version + '\n' + '\n';
    output += headerModule + '\n';
    output += ghoul::join(_modules, "\n");
    output += '\n';
    output += headerAsset + '\n';
    output += ghoul::join(_assets, "\n");
    output += '\n';
    output += headerProperty + '\n';
    output += ghoul::join(_properties, "\n");
    output += '\n';
    output += headerKeybinding + '\n';
    output += ghoul::join(_keybindings, "\n");
    output += '\n';
    output += headerTime + '\n';
    output += _time + '\n' + '\n';
    output += headerCamera + '\n';
    output += _camera + '\n' + '\n';
    output += headerMarkNodes + '\n';
    output += ghoul::join(_markNodes, "\n");

    return output;
}

const std::string& ProfileFile::version() const {
    return _version;
}

void ProfileFile::setVersion(std::string v) {
    _version = std::move(v);
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

bool ProfileFile::determineSection(std::string line) {
    bool foundSection = true;

    if (line.compare(headerVersion) == 0) {
        parseCurrentSection = &ProfileFile::parseVersion;
    }
    else if (line.compare(headerModule) == 0) {
        parseCurrentSection = &ProfileFile::parseModule;
    }
    else if (line.compare(headerAsset) == 0) {
        parseCurrentSection = &ProfileFile::parseAsset;
    }
    else if (line.compare(headerProperty) == 0) {
        parseCurrentSection = &ProfileFile::parseProperty;
    }
    else if (line.compare(headerKeybinding) == 0) {
        parseCurrentSection = &ProfileFile::parseKeybinding;
    }
    else if (line.compare(headerTime) == 0) {
        parseCurrentSection = &ProfileFile::parseTime;
    }
    else if (line.compare(headerCamera) == 0) {
        parseCurrentSection = &ProfileFile::parseCamera;
    }
    else if (line.compare(headerMarkNodes) == 0) {
        parseCurrentSection = &ProfileFile::parseMarkNodes;
    }
    else {
        throw ProfileError(
            _lineNum,
            fmt::format("Invalid section header '{}'", line)
        );
        foundSection = false;
    }
    return foundSection;
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
    if (++_numLinesVersion > versionLinesExpected) {
        throw ProfileError(_lineNum, "Too many lines in Version section");
    }
    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
    if (fields.size() > versionFieldsExpected) {
        throw ProfileError(_lineNum, "No tabs allowed in Version entry");
    }
    else {
        _version = line;
    }
}

void ProfileFile::parseModule(std::string line) {
    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');

    if (fields.size() != moduleFieldsExpected) {
        throw ProfileError(
            _lineNum,
            fmt::format("{} fields required in a Module entry", moduleFieldsExpected)
        );
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
    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
    if (fields.size() != assetFieldsExpected) {
        throw ProfileError(
            _lineNum,
            fmt::format("{} fields required in an Asset entry", assetFieldsExpected)
        );
    }
    std::vector<std::string> standard = {
        "asset name",
        ""
    };
    verifyRequiredFields("Asset", fields, standard, assetFieldsExpected);
    _assets.push_back(line);
}

void ProfileFile::parseProperty(std::string line) {
    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');

    if (fields.size() != propertyFieldsExpected) {
        throw ProfileError(
            _lineNum,
            fmt::format("{} fields required in Property entry", propertyFieldsExpected)
        );
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
    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');

    if (fields.size() != keybindingFieldsExpected) {
        throw ProfileError(
            _lineNum,
            fmt::format("{} fields required in Keybinding entry", keybindingFieldsExpected)
        );
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
    if (++_numLinesTime > timeLinesExpected) {
        throw ProfileError(_lineNum, "Too many lines in Time section");
    }

    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
    if (fields.size() != timeFieldsExpected) {
        throw ProfileError(
            _lineNum,
            fmt::format("{} fields required in Time entry", timeFieldsExpected)
        );
    }
    std::vector<std::string> standard = {
        "time set type",
        "time value to set"
    };
    verifyRequiredFields("Time", fields, standard, timeFieldsExpected);
    _time = line;
}

void ProfileFile::parseCamera(std::string line) {
    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');

    if (++_numLinesCamera > cameraLinesExpected) {
        throw ProfileError(_lineNum, "Too many lines in Camera section");
    }
    
    if (fields.size() == cameraNavigationFieldsExpected) {
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
    else if (fields.size() == cameraGeoFieldsExpected) {
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
        throw ProfileError(
            _lineNum,
            fmt::format(
                "{} or {} fields required in Camera entry",
                cameraNavigationFieldsExpected, cameraGeoFieldsExpected
            )
        );
    }
    _camera = line;
}

void ProfileFile::parseMarkNodes(std::string line) {
    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');

    if (fields.size() != markNodesFieldsExpected) {
        throw ProfileError(
            _lineNum,
            fmt::format("{} field required in a Mark Nodes entry", markNodesFieldsExpected)
        );
    }
    std::vector<std::string> standard = { "Mark Interesting Node name" };
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
            throw ProfileError(_lineNum, std::move(errMsg));
        }
    }
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
