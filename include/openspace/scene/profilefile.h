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

#include <string>
#include <istream>
#include <fstream>
#include <functional>
#include <vector>

namespace ghoul { class Dictionary; }
namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

namespace documentation { struct Documentation; }
namespace scripting { struct LuaLibrary; }

class ProfileFile {
public:
    void readLines(std::function<bool(std::string&)> reader);
    void readFromFile(std::string filename);
    void processIndividualLine(bool& insideSection, std::string line);
    void write(std::ostream& output);
    void writeToFile(std::string filename);

    const std::string getVersion() const;

    //Methods for updating contents
    void updateTime();
    void updateCamera();
    void addModuleLine(std::string line);
    void addAssetLine(std::string line);
    void addPropertyLine(std::string line);
    void addKeybindingLine(std::string line);
    void addMarkNodesLine(std::string line);

    //Methods for getting contents of each section
    std::string time() const;
    std::string camera() const;
    std::vector<std::string> modules() const;
    std::vector<std::string> assets() const;
    std::vector<std::string> properties() const;
    std::vector<std::string> keybindings() const;
    std::vector<std::string> markNodes() const;

private:
    std::string errorString(std::string message);
    void clearAllFields();
    bool isBlank(std::string line);
    size_t splitByTab(std::string line, std::vector<std::string>& result);
    void verifyRequiredFields(std::string sectionName, std::vector<std::string> fields,
                              std::vector<std::string> standard, unsigned int nFields);

    bool determineSection(std::string line);
    void (ProfileFile::* parseCurrentSection)(std::string);
    void parseVersion(std::string line);
    void parseModule(std::string line);
    void parseAsset(std::string line);
    void parseProperty(std::string line);
    void parseKeybinding(std::string line);
    void parseTime(std::string line);
    void parseCamera(std::string line);
    void parseMarkNodes(std::string line);
    void addAllElements(std::ostream& file, std::vector<std::string>& list);

    const size_t _versionLinesExpected = 1;
    const size_t _timeLinesExpected = 1;
    const size_t _cameraLinesExpected = 1;
    const size_t _versionFieldsExpected = 1;
    const size_t _moduleFieldsExpected = 3;
    const size_t _assetFieldsExpected = 2;
    const size_t _propertyFieldsExpected = 3;
    const size_t _keybindingFieldsExpected = 6;
    const size_t _timeFieldsExpected = 2;
    const size_t _cameraNavigationFieldsExpected = 8;
    const size_t _cameraGeoFieldsExpected = 5;
    const size_t _markNodesFieldsExpected = 1;

    const std::string header_Version    = "#Version";
    const std::string header_Module     = "#Module";
    const std::string header_Asset      = "#Asset";
    const std::string header_Property   = "#Property";
    const std::string header_Keybinding = "#Keybinding";
    const std::string header_Time       = "#Time";
    const std::string header_Camera     = "#Camera";
    const std::string header_MarkNodes  = "#MarkNodes";

    size_t _lineNum = 1;
    size_t _numLinesVersion = 0;
    size_t _numLinesTime    = 0;
    size_t _numLinesCamera  = 0;

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
