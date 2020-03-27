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

#ifndef __OPENSPACE_CORE___PROFILEFILE___H__
#define __OPENSPACE_CORE___PROFILEFILE___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scenelicense.h>
#include <ghoul/misc/easing.h>
#include <ghoul/misc/exception.h>
#include <mutex>
#include <set>
#include <unordered_map>
#include <vector>

namespace ghoul { class Dictionary; }
namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

namespace documentation { struct Documentation; }
namespace scripting { struct LuaLibrary; }


class ProfileFile {
public:
    ProfileFile(std::string filename);
    ~ProfileFile();

    //Need a copy constructor here to do a deep copy

    void read();
    void write();

    void setFilename(std::string filename);

    //Methods for updating contents
    void updateTime();
    void updateCamera();
    void addModuleLine(std::string line);
    void addAssetLine(std::string line);
    void addPropertyLine(std::string line);
    void addKeybindingLine(std::string line);
    void addMarkNodesLine(std::string line);

    //Methods for getting contents of each section
    std::string time();
    std::string camera();
    std::vector<std::string> modules();
    std::vector<std::string> assets();
    std::vector<std::string> properties();
    std::vector<std::string> keybindings();
    std::vector<std::string> markNodes();

private:
    void logError(std::string message);
    void clearAllFields();
    bool isBlank(std::string line);
    int  splitByTab(std::string line, std::vector<std::string>& result);
    bool determineSection(std::string line);
    void parseCurrentSection(std::string line);
    void parseVersion(std::string line);
    void parseModule(std::string line);
    void parseAsset(std::string line);
    void parseProperty(std::string line);
    void parseKeybinding(std::string line);
    void parseTime(std::string line);
    void parseCamera(std::string line);
    void parseMarkNodes(std::string line);
    void verifyRequiredFields(std::string sectionName, std::vector<std::string> fields,
                              std::vector<std::string> standard, unsigned int nFields);

    const int versionLinesExpected = 1;
    const int timeLinesExpected = 1;
    const int cameraLinesExpected = 1;

    const int versionFieldsExpected = 1;
    const int moduleFieldsExpected = 3;
    const int assetFieldsExpected = 2;
    const int propertyFieldsExpected = 3;
    const int keybindingFieldsExpected = 6;
    const int timeFieldsExpected = 2;
    const int cameraNavigationFieldsExpected = 8;
    const int cameraGeoFieldsExpected = 5;
    const int markNodesFieldsExpected = 1;

    std::string _filename;
    unsigned int _lineNum = 1;
    unsigned int _numLinesVersion = 0;
    unsigned int _numLinesTime    = 0;
    unsigned int _numLinesCamera  = 0;

    std::string _version;
    std::string _time;
    std::string _camera;
    std::vector<std::string> _modules;
    std::vector<std::string> _assets;
    std::vector<std::string> _properties;
    std::vector<std::string> _keybindings;
    std::vector<std::string> _markNodes;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___PROFILEFILE___H__
