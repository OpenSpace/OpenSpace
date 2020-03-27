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

#include <openspace/scene/profileFile.h>

#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scenelicensewriter.h>
#include <openspace/scene/sceneinitializer.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/camera.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <string>
#include <stack>
#include <iomanip>
#include <cctype>

namespace {
    constexpr const char* _loggerCat = "ProfileFile";
    constexpr const char* KeyIdentifier = "Identifier";
    constexpr const char* KeyParent = "Parent";

} // namespace

namespace openspace {

ProfileFile::ProfileFile(std::string filename)
    : _filename(filename)
{
}

ProfileFile::~ProfileFile() {
}

void ProfileFile::read() {
    clearAllFields();
    std::ifstream inFile;
    inFile.open(_filename, std::ifstream::in);
    std::string line;
    _lineNum = 1;
    bool insideSection = false;

    while (getline(inFile, line)) {
        if (insideSection) {
            if (isBlank(line)) {
                insideSection = false;
            }
            else {
                parseCurrentSection(line);
            }
        }
        else if (line.substr(0, 1) == "#") {
            if (determineSection(line)) {
                insideSection = true;
            }
        }
        _lineNum++;
    }
    inFile.close();
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
    char* c = line.c_str();
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

}

void ProfileFile::parseCurrentSection(std::string line) {

}

void ProfileFile::logError(std::string message) {
    std::string e = "Error @ line " + std::to_string(_lineNum) + ": ";
    LERROR(e + message);
}

void ProfileFile::parseVersion(std::string line) {
    std::vector<std::string> fields;

    if (++_numLinesVersion > versionLinesExpected) {
        logError("Too many lines in Version section");
    }
    if (splitByTab(line, fields) > versionFieldsExpected) {
        logError("No tabs allowed in Version entry");
    }
    else {
        _version = line;
    }
}

void ProfileFile::parseModule(std::string line) {
    std::vector<std::string> fields;

    if (splitByTab(line, fields) != moduleFieldsExpected) {
        logError(std::to_string(moduleFieldsExpected) +
            " fields required in a Module entry");
    }
    std::vector<std::string> standard = {
        "module name",
        "",
        ""
    };
    verifyRequiredFields(fields, standard, moduleFieldsExpected);
    _modules.push_back(line);
}

void ProfileFile::parseAsset(std::string line) {
    std::vector<std::string> fields;

    if (splitByTab(line, fields) != assetFieldsExpected) {
        logError(std::to_string(assetFieldsExpected) +
            " fields required in an Asset entry");
    }
    std::vector<std::string> standard = {
        "asset name",
        ""
    };
    verifyRequiredFields(fields, standard, assetFieldsExpected);
    _assets.push_back(line);
}

void ProfileFile::parseProperty(std::string line) {
    std::vector<std::string> fields;

    if (splitByTab(line, fields) != propertyFieldsExpected) {
        logError(std::to_string(propertyFieldsExpected) +
            " fields required in Property entry");
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
        logError(std::to_string(keybindingFieldsExpected) +
            " fields required in Keybinding entry");
    }
    std::vector<std::string> standard = {
        "key",
        "documentation",
        "name",
        "GuiPath",
        "local(T/F)",
        "script to execute"
    };
    verifyRequiredFields("Keybinding", fields, standard, propertyFieldsExpected);
    _properties.push_back(line);
}

void ProfileFile::parseTime(std::string line) {
    std::vector<std::string> fields;

    if (++_numLinesTime > timeLinesExpected) {
        logError("Too many lines in time section");
    }
    if (splitByTab(line, fields) != timeFieldsExpected) {
        logError(std::to_string(timeFieldsExpected) +
            " fields required in Time entry");
    }
    std::vector<std::string> standard = {
        "time set type",
        "time value to set"
    };
    verifyRequiredFields("Time", fields, standard, propertyFieldsExpected);
    _time = line;
}

void ProfileFile::parseCamera(std::string line) {
    std::vector<std::string> fields;

    if (++_numLinesCamera > cameraLinesExpected) {
        logError("Too many lines in camera section");
    }
    int nFields = splitByTab(line, fields);
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
        verifyRequiredFields("Camera", fields, standard, cameraNavigationFieldsExpected);
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
            cameraNavigationFieldsExpected);
    }
    else {
        logError(std::to_string(cameraNavigationFieldsExpected) + " or " +
            std::to_string(cameraGeoFieldsExpected) + " fields required in Camera entry");
    }
    _camera = line;
}

void ProfileFile::parseMarkNodes(std::string line) {
    std::vector<std::string> fields;

    if (splitByTab(line, fields) != markNodesFieldsExpected) {
        logError(std::to_string(markNodesFieldsExpected) +
            " field required in an Mark Nodes entry");
    }
    std::vector<std::string> standard = {
        "Mark Interesting Node name"
    };
    verifyRequiredFields("Mark Interesting Nodes", fields, standard,
        markNodesFieldsExpected);
    _assets.push_back(line);
}

void ProfileFile::verifyRequiredFields(std::string sectionName,
                                       std::vector<std::string> fields,
                                       std::vector<std::string> standard,
                                       unsigned int nFields)
{
    for (unsigned int i = 0; i < fields.size(); i++) {
        if (!standard[i].empty() && fields[i].empty()) {
            std::string errMsg = sectionName + " " + standard[i];
            errMsg += "(arg " + std::to_string(i) + "/" + nFields + ") is required";
            logError(errMsg);
        }
    }
}

int ProfileFile::splitByTab(std::string line, std::vector<std::string>& result) {
    std::istringstream iss(line);
    std::string tmp;
    result.clear();
    while(std::getline(iss, tmp, '\t')) {
        result.push_back(tmp);
    }
    return result.size();
}

}  // namespace openspace
